use std::collections::HashMap;
use std::{iter, mem};

use calyx_ir as ir;
use calyx_utils::NameGenerator;
use itertools::Itertools;

use super::builtins;
use super::libm::{MathLib, Prototype};
use super::stdlib::{self, Arguments, Primitive, Signature};
use crate::analysis::{self as sem, Binding, PassManager};
use crate::fpcore::ast;
use crate::opts::Opts;
use crate::utils::rational::{FixedPoint, RoundBinary};
use crate::utils::{Diagnostic, Format, Reporter};

/// A compiled expression.
///
/// The output port is valid after executing the control program, as long as
/// the assignments are active.
struct Expression {
    control: ir::Control,
    assignments: Vec<ir::Assignment<ir::Nothing>>,
    out: ir::RRC<ir::Port>,
}

impl Expression {
    fn from_constant(port: ir::RRC<ir::Port>) -> Expression {
        Expression {
            control: ir::Control::empty(),
            assignments: Vec::new(),
            out: port,
        }
    }
}

struct ExpressionBuilder<'b, 'ast, 'comp> {
    format: &'b Format,
    bindings: &'b sem::NameResolution<'ast>,
    signatures: &'b HashMap<ast::Id, Prototype>,
    reporter: &'b mut Reporter<'ast>,
    libm: &'b mut HashMap<ast::NodeId, Prototype>,
    builder: &'b mut ir::Builder<'comp>,
    stores: HashMap<ast::NodeId, ir::RRC<ir::Cell>>,
}

impl ExpressionBuilder<'_, '_, '_> {
    fn compile_number(&mut self, num: &ast::Number) -> Option<Expression> {
        let rounded = (&num.value).round_convergent(self.format.lsb());

        let Some(val) = rounded.to_fixed_point(self.format) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("overflow")
                    .with_primary(num.span, "constant overflows target format"),
            );

            return None;
        };

        let Ok(val) = u64::try_from(&val) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("code generation failed")
                    .with_primary(num.span, "constant is too wide")
                    .with_note(
                        "calyx doesn't support constants wider than 64 bits",
                    ),
            );

            return None;
        };

        let cell = self.builder.add_constant(val, u64::from(self.format.width));
        let port = cell.borrow().get("out");

        Some(Expression::from_constant(port))
    }

    fn compile_constant(
        &mut self,
        constant: ast::Constant,
        span: ast::Span,
    ) -> Option<Expression> {
        let params = match constant {
            ast::Constant::Math(_) => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported numeric constant")
                        .with_primary(span, "unsupported constant"),
                );

                return None;
            }
            ast::Constant::Bool(val) => [1, u64::from(val)],
        };

        let cell = self.builder.add_primitive("c", "std_const", &params);
        let port = cell.borrow().get("out");

        Some(Expression::from_constant(port))
    }

    fn compile_symbol(
        &self,
        sym: &ast::Symbol,
        uid: ast::NodeId,
    ) -> Expression {
        let port = match self.bindings.names[&uid] {
            Binding::Argument(_) => {
                let signature = self.builder.component.signature.borrow();

                signature.get(sym.id)
            }
            Binding::Let(binding) => {
                let cell = &self.stores[&binding.expr.uid];
                let port = cell.borrow().get("out");

                port
            }
            _ => unreachable!("pass rejects loops"),
        };

        Expression::from_constant(port)
    }

    fn compile_variadic_operation(
        &mut self,
        op: ast::TestOp,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        let mut assignments = Vec::new();

        let (control, args): (Vec<_>, Vec<_>) = args
            .iter()
            .map(|arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = collapse(control, ir::Control::par);

        let mut reduce = |left, right, decl: &Primitive| {
            let prim = self.builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(self.format),
            );

            let out = prim.borrow().get("out");

            let assigns =
                [("left", left), ("right", right)].map(|(dst, src)| {
                    self.builder.build_assignment(
                        prim.borrow().get(dst),
                        src,
                        ir::Guard::True,
                    )
                });

            assignments.extend(assigns);

            out
        };

        let args = match op {
            ast::TestOp::Lt
            | ast::TestOp::Gt
            | ast::TestOp::Leq
            | ast::TestOp::Geq
            | ast::TestOp::Eq => {
                let decl = match op {
                    ast::TestOp::Lt => builtins::lt(self.format),
                    ast::TestOp::Gt => builtins::gt(self.format),
                    ast::TestOp::Leq => builtins::le(self.format),
                    ast::TestOp::Geq => builtins::ge(self.format),
                    ast::TestOp::Eq => builtins::eq(self.format),
                    _ => unreachable!(),
                };

                args.into_iter()
                    .tuple_windows()
                    .map(|(left, right)| reduce(left, right, decl))
                    .collect()
            }
            ast::TestOp::Neq => {
                let decl = builtins::neq(self.format);

                args.into_iter()
                    .tuple_combinations()
                    .map(|(left, right)| reduce(left, right, decl))
                    .collect()
            }
            ast::TestOp::And | ast::TestOp::Or => args,
            _ => unreachable!(),
        };

        let decl = if matches!(op, ast::TestOp::Or) {
            &stdlib::core::STD_OR
        } else {
            &stdlib::core::STD_AND
        };

        let out = args
            .into_iter()
            .tree_fold1(|left, right| reduce(left, right, decl))
            .unwrap();

        Some(Expression {
            control,
            assignments,
            out,
        })
    }

    fn compile_instantiated_operation(
        &mut self,
        cell: ir::RRC<ir::Cell>,
        signature: &Signature,
        is_comb: bool,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        let mut assignments = Vec::new();

        let (control, args): (Vec<_>, Vec<_>) = args
            .iter()
            .map(|arg| {
                let expr = self.compile_expression(arg)?;

                assignments.extend(expr.assignments);

                Some((expr.control, expr.out))
            })
            .collect::<Option<_>>()?;

        let control = collapse(control, ir::Control::par);
        let out = cell.borrow().get(signature.output);

        let inputs: Vec<_> =
            signature.args.iter().map(ir::Id::new).zip(args).collect();

        let control = if is_comb {
            assignments.extend(inputs.into_iter().map(|(dst, src)| {
                self.builder.build_assignment(
                    cell.borrow().get(dst),
                    src,
                    ir::Guard::True,
                )
            }));

            control
        } else {
            let invoke = invoke_with(
                cell,
                inputs,
                mem::take(&mut assignments),
                self.builder,
            );

            collapse([control, invoke], ir::Control::seq)
        };

        Some(Expression {
            control,
            assignments,
            out,
        })
    }

    fn compile_primitive_operation(
        &mut self,
        primitive: &Primitive,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        let cell = self.builder.add_primitive(
            primitive.prefix_hint,
            primitive.name,
            &primitive.build_params(self.format),
        );

        self.compile_instantiated_operation(
            cell,
            &primitive.signature,
            primitive.is_comb,
            args,
        )
    }

    fn compile_component_operation(
        &mut self,
        prototype: &Prototype,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        let cell = self.builder.add_component(
            prototype.prefix_hint,
            prototype.name,
            prototype.signature.clone(),
        );

        let signature_args: Vec<_> = prototype
            .signature
            .iter()
            .filter_map(|port| {
                (port.direction == ir::Direction::Input)
                    .then_some(port.name().id.as_str())
            })
            .collect();

        let signature = Signature {
            args: Arguments(&signature_args),
            output: "out",
        };

        self.compile_instantiated_operation(
            cell,
            &signature,
            prototype.is_comb,
            args,
        )
    }

    fn compile_operation(
        &mut self,
        op: &ast::Operation,
        uid: ast::NodeId,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        if let Some(prototype) = self.libm.remove(&uid) {
            return self.compile_component_operation(&prototype, args);
        }

        match op.kind {
            ast::OpKind::Math(ast::MathOp::Add) => self
                .compile_primitive_operation(builtins::add(self.format), args),
            ast::OpKind::Math(ast::MathOp::Sub) => self
                .compile_primitive_operation(builtins::sub(self.format), args),
            ast::OpKind::Math(ast::MathOp::Mul) => self
                .compile_primitive_operation(builtins::mul(self.format), args),
            ast::OpKind::Math(ast::MathOp::Div) => self
                .compile_primitive_operation(builtins::div(self.format), args),
            ast::OpKind::Math(ast::MathOp::Neg) => self
                .compile_primitive_operation(&stdlib::numbers::NUM_NEG, args),
            ast::OpKind::Math(ast::MathOp::Sqrt) => self
                .compile_primitive_operation(builtins::sqrt(self.format), args),
            ast::OpKind::Test(op) if op.is_variadic() => {
                self.compile_variadic_operation(op, args)
            }
            ast::OpKind::Test(ast::TestOp::Not) => {
                self.compile_primitive_operation(&stdlib::core::STD_NOT, args)
            }
            ast::OpKind::FPCore(id) => {
                self.compile_component_operation(&self.signatures[&id], args)
            }
            _ => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported operation")
                        .with_primary(op.span, "unsupported operator"),
                );

                None
            }
        }
    }

    fn compile_if(
        &mut self,
        cond: &ast::Expression,
        true_branch: &ast::Expression,
        false_branch: &ast::Expression,
    ) -> Option<Expression> {
        let cond = self.compile_expression(cond)?;
        let true_branch = self.compile_expression(true_branch)?;
        let false_branch = self.compile_expression(false_branch)?;

        let params = [u64::from(self.format.width)];

        match (&true_branch.control, &false_branch.control) {
            (ir::Control::Empty(_), ir::Control::Empty(_)) => {
                let mux = self.builder.add_primitive("mux", "std_mux", &params);
                let out = mux.borrow().get("out");

                let inputs = [
                    ("cond", cond.out),
                    ("tru", true_branch.out),
                    ("fal", false_branch.out),
                ];

                let mut assignments: Vec<_> = inputs
                    .into_iter()
                    .map(|(dst, src)| {
                        self.builder.build_assignment(
                            mux.borrow().get(dst),
                            src,
                            ir::Guard::True,
                        )
                    })
                    .collect();

                assignments.extend(cond.assignments);
                assignments.extend(true_branch.assignments);
                assignments.extend(false_branch.assignments);

                Some(Expression {
                    control: cond.control,
                    assignments,
                    out,
                })
            }
            _ => {
                let reg = self.builder.add_primitive("r", "std_reg", &params);
                let out = reg.borrow().get("out");

                let store_true = invoke_with(
                    reg.clone(),
                    vec![(ir::Id::new("in"), true_branch.out)],
                    true_branch.assignments,
                    self.builder,
                );

                let store_false = invoke_with(
                    reg,
                    vec![(ir::Id::new("in"), false_branch.out)],
                    false_branch.assignments,
                    self.builder,
                );

                let group = self.builder.add_comb_group("cond");
                group.borrow_mut().assignments = cond.assignments;

                let conditional = ir::Control::if_(
                    cond.out,
                    Some(group),
                    Box::new(collapse(
                        [true_branch.control, store_true],
                        ir::Control::seq,
                    )),
                    Box::new(collapse(
                        [false_branch.control, store_false],
                        ir::Control::seq,
                    )),
                );

                let control =
                    collapse([cond.control, conditional], ir::Control::seq);

                Some(Expression {
                    control,
                    assignments: Vec::new(),
                    out,
                })
            }
        }
    }

    fn compile_let(
        &mut self,
        bindings: &[ast::Binding],
        body: &ast::Expression,
        sequential: bool,
    ) -> Option<Expression> {
        let (args, stores): (Vec<_>, Vec<_>) = bindings
            .iter()
            .map(|binding| {
                let expr = self.compile_expression(&binding.expr)?;

                let params = [expr.out.borrow().width];
                let reg = self.builder.add_primitive("r", "std_reg", &params);

                let invoke = invoke_with(
                    reg.clone(),
                    vec![(ir::Id::new("in"), expr.out)],
                    expr.assignments,
                    self.builder,
                );

                self.stores.insert(binding.expr.uid, reg);

                Some((expr.control, invoke))
            })
            .collect::<Option<_>>()?;

        let body = self.compile_expression(body)?;

        let control = if sequential {
            collapse(
                itertools::interleave(args, stores)
                    .chain(iter::once(body.control)),
                ir::Control::seq,
            )
        } else {
            collapse(
                [
                    collapse(args, ir::Control::par),
                    collapse(stores, ir::Control::par),
                    body.control,
                ],
                ir::Control::seq,
            )
        };

        Some(Expression { control, ..body })
    }

    fn compile_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Option<Expression> {
        match &expr.kind {
            ast::ExprKind::Num(num) => self.compile_number(num),
            ast::ExprKind::Const(constant) => {
                self.compile_constant(*constant, expr.span)
            }
            ast::ExprKind::Id(sym) => Some(self.compile_symbol(sym, expr.uid)),
            ast::ExprKind::Op(op, args) => {
                self.compile_operation(op, expr.uid, args)
            }
            ast::ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => self.compile_if(cond, true_branch, false_branch),
            ast::ExprKind::Let {
                bindings,
                body,
                sequential,
            } => self.compile_let(bindings, body, *sequential),
            ast::ExprKind::Annotation { props: _, body } => {
                self.compile_expression(body)
            }
            _ => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported expression")
                        .with_primary(expr.span, ""),
                );

                None
            }
        }
    }
}

struct GlobalContext<'c, 'ast> {
    format: &'c Format,
    bindings: &'c sem::NameResolution<'ast>,
    reporter: &'c mut Reporter<'ast>,
    lib: &'c ir::LibrarySignatures,
    names: &'c mut NameGenerator,
    libm: &'c mut HashMap<ast::NodeId, Prototype>,
    signatures: HashMap<ast::Id, Prototype>,
}

fn compile_definition(
    def: &ast::FPCore,
    ctx: &mut GlobalContext,
) -> Option<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| ctx.names.gen_name("main"), |sym| sym.id);

    let ports = || {
        def.args
            .iter()
            .map(|arg| {
                ir::PortDef::new(
                    arg.var.id,
                    u64::from(ctx.format.width),
                    ir::Direction::Input,
                    Default::default(),
                )
            })
            .chain(iter::once(ir::PortDef::new(
                "out",
                u64::from(ctx.format.width),
                ir::Direction::Output,
                Default::default(),
            )))
            .collect()
    };

    let mut component = ir::Component::new(name, ports(), false, false, None);
    let mut builder = ir::Builder::new(&mut component, ctx.lib).not_generated();

    let mut expr_builder = ExpressionBuilder {
        format: ctx.format,
        bindings: ctx.bindings,
        signatures: &ctx.signatures,
        reporter: ctx.reporter,
        libm: ctx.libm,
        builder: &mut builder,
        stores: HashMap::new(),
    };

    let body = expr_builder.compile_expression(&def.body)?;

    let assign = builder.build_assignment(
        builder.component.signature.borrow().get("out"),
        body.out,
        ir::Guard::True,
    );

    component.continuous_assignments.extend(body.assignments);
    component.continuous_assignments.push(assign);

    component.is_comb = matches!(body.control, ir::Control::Empty(_));

    *component.control.borrow_mut() = body.control;

    if let Some(sym) = &def.name {
        let prototype = Prototype {
            name: component.name,
            prefix_hint: sym.id,
            signature: ports(),
            is_comb: component.is_comb,
        };

        ctx.signatures.insert(sym.id, prototype);
    }

    Some(component)
}

pub fn compile_fpcore<'ast>(
    defs: &'ast [ast::FPCore],
    opts: &Opts,
    rpt: &mut Reporter<'ast>,
    mut lib: ir::LibrarySignatures,
) -> Option<ir::Context> {
    let mut names = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
            .collect(),
    );

    let pm = PassManager::new(opts, defs, rpt);

    let _: &sem::TypeCheck = pm.get_analysis()?;
    let call_graph: &sem::CallGraph = pm.get_analysis()?;

    let MathLib {
        mut components,
        mut prototypes,
    } = MathLib::new(&pm, &mut lib)?;

    let mut ctx = GlobalContext {
        format: &opts.format,
        bindings: pm.get_analysis()?,
        reporter: &mut pm.rpt(),
        lib: &lib,
        names: &mut names,
        libm: &mut prototypes,
        signatures: HashMap::new(),
    };

    for def in &call_graph.linearized {
        components.push(compile_definition(def, &mut ctx)?);
    }

    Some(ir::Context {
        components,
        lib,
        bc: Default::default(),
        entrypoint: ir::Id::new("main"),
        extra_opts: Vec::new(),
        metadata: None,
    })
}

fn collapse<I, F>(stmts: I, f: F) -> ir::Control
where
    I: IntoIterator<Item = ir::Control>,
    F: FnOnce(Vec<ir::Control>) -> ir::Control,
{
    let stmts: Vec<_> = stmts
        .into_iter()
        .filter(|stmt| !matches!(stmt, ir::Control::Empty(_)))
        .collect();

    match stmts.len() {
        0 => ir::Control::empty(),
        1 => stmts.into_iter().next().unwrap(),
        _ => f(stmts),
    }
}

fn invoke_with(
    comp: ir::RRC<ir::Cell>,
    inputs: Vec<(ir::Id, ir::RRC<ir::Port>)>,
    assignments: Vec<ir::Assignment<ir::Nothing>>,
    builder: &mut ir::Builder,
) -> ir::Control {
    let comb_group = (!assignments.is_empty()).then(|| {
        let group = builder.add_comb_group("expr");
        group.borrow_mut().assignments = assignments;

        group
    });

    ir::Control::Invoke(ir::Invoke {
        comp,
        inputs,
        outputs: Vec::new(),
        attributes: Default::default(),
        comb_group,
        ref_cells: Vec::new(),
    })
}
