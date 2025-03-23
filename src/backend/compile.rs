//! FPCore to Calyx compiler.

use std::collections::HashMap;
use std::iter;

use calyx_ir as ir;
use calyx_utils::{Id, NameGenerator};
use itertools::Itertools;

use super::builtins;
use super::libm::{MathLib, Prototype};
use super::stdlib::{self, Arguments, Primitive, Signature};
use crate::analysis::{Binding, NameResolution, PassManager, TypeCheck};
use crate::format::Format;
use crate::fpcore::ast;
use crate::opts::Opts;
use crate::utils::diagnostics::{Diagnostic, Reporter};
use crate::utils::rational::FixedPoint;

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
    bindings: &'b NameResolution<'ast>,
    reporter: &'b mut Reporter<'ast>,
    libm: &'b mut HashMap<ast::NodeId, Prototype>,
    builder: &'b mut ir::Builder<'comp>,
    stores: HashMap<ast::NodeId, ir::RRC<ir::Cell>>,
}

impl ExpressionBuilder<'_, '_, '_> {
    fn compile_number(&mut self, num: &ast::Number) -> Option<Expression> {
        let Some(val) = num.value.to_fixed_point(self.format) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("unrepresentable constant")
                    .with_primary(
                        num.span,
                        "constant is not representable in the global format",
                    ),
            );

            return None;
        };

        let Ok(val) = u64::try_from(&val) else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("code generation failed")
                    .with_primary(num.span, "generated constant is too large")
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
        let mut control = Vec::with_capacity(args.len());
        let mut assignments = Vec::new();

        let args: Vec<_> = args
            .iter()
            .map(|arg| {
                let expr = self.compile_expression(arg)?;

                control.push(expr.control);
                assignments.extend(expr.assignments);

                Some(expr.out)
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

    fn compile_library_operation(
        &mut self,
        op: &ast::Operation,
        uid: ast::NodeId,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        let (arg_ctrl, arg_assigns, arg_ports): (Vec<_>, Vec<_>, Vec<_>) = args
            .iter()
            .map(|arg| {
                let expr = self.compile_expression(arg)?;

                Some((expr.control, expr.assignments, expr.out))
            })
            .collect::<Option<_>>()?;

        let decl = match op.kind {
            ast::OpKind::Math(op) => match op {
                ast::MathOp::Add => Some(builtins::add(self.format)),
                ast::MathOp::Sub => Some(builtins::sub(self.format)),
                ast::MathOp::Mul => Some(builtins::mul(self.format)),
                ast::MathOp::Div => Some(builtins::div(self.format)),
                ast::MathOp::Neg => Some(&stdlib::numbers::NUM_NEG),
                ast::MathOp::Sqrt => Some(builtins::sqrt(self.format)),
                _ => None,
            },
            ast::OpKind::Test(op) => match op {
                ast::TestOp::Not => Some(&stdlib::core::STD_NOT),
                _ => None,
            },
            ast::OpKind::Tensor(_) => None,
        };

        let (cell, signature, is_comb) = if let Some(decl) = decl {
            let prim = self.builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(self.format),
            );

            (prim, &decl.signature, decl.is_comb)
        } else if let Some(proto) = self.libm.remove(&uid) {
            let comp = self.builder.add_component(
                proto.prefix_hint,
                proto.name,
                proto.signature,
            );

            (comp, &Signature::UNARY_DEFAULT, proto.is_comb)
        } else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("unsupported operation")
                    .with_primary(op.span, "unsupported operator"),
            );

            return None;
        };

        let inputs = match signature.args {
            Arguments::Unary { input } => {
                let (arg,) = arg_ports.into_iter().collect_tuple().unwrap();

                vec![(Id::new(input), arg)]
            }
            Arguments::Binary { left, right } => {
                let (left_arg, right_arg) =
                    arg_ports.into_iter().collect_tuple().unwrap();

                vec![(Id::new(left), left_arg), (Id::new(right), right_arg)]
            }
        };

        let arg_ctrl = collapse(arg_ctrl, ir::Control::par);
        let arg_assigns = arg_assigns.into_iter().flatten().collect();

        let out = cell.borrow().get(signature.output);

        let (control, assignments) = if is_comb {
            let assigns = inputs
                .into_iter()
                .map(|(dst, src)| {
                    self.builder.build_assignment(
                        cell.borrow().get(dst),
                        src,
                        ir::Guard::True,
                    )
                })
                .chain(arg_assigns)
                .collect();

            (arg_ctrl, assigns)
        } else {
            let invoke = invoke_with(cell, inputs, arg_assigns, self.builder);
            let control = collapse([arg_ctrl, invoke], ir::Control::seq);

            (control, vec![])
        };

        Some(Expression {
            control,
            assignments,
            out,
        })
    }

    fn compile_operation(
        &mut self,
        op: &ast::Operation,
        uid: ast::NodeId,
        args: &[ast::Expression],
    ) -> Option<Expression> {
        match op.kind {
            ast::OpKind::Test(op) if op.is_variadic() => {
                self.compile_variadic_operation(op, args)
            }
            _ => self.compile_library_operation(op, uid, args),
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
                    vec![(Id::new("in"), true_branch.out)],
                    true_branch.assignments,
                    self.builder,
                );

                let store_false = invoke_with(
                    reg,
                    vec![(Id::new("in"), false_branch.out)],
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
                    vec![(Id::new("in"), expr.out)],
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
    bindings: &'c NameResolution<'ast>,
    reporter: &'c mut Reporter<'ast>,
    lib: &'c ir::LibrarySignatures,
    names: &'c mut NameGenerator,
    libm: &'c mut HashMap<ast::NodeId, Prototype>,
}

fn compile_definition(
    def: &ast::FPCore,
    ctx: &mut GlobalContext,
) -> Option<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| ctx.names.gen_name("main"), |sym| sym.id);

    let mut ports = vec![ir::PortDef::new(
        "out",
        u64::from(ctx.format.width),
        ir::Direction::Output,
        Default::default(),
    )];

    ports.extend(def.args.iter().map(|arg| {
        ir::PortDef::new(
            arg.var.id,
            u64::from(ctx.format.width),
            ir::Direction::Input,
            Default::default(),
        )
    }));

    let mut component = ir::Component::new(name, ports, false, false, None);
    let mut builder = ir::Builder::new(&mut component, ctx.lib).not_generated();

    let mut expr_builder = ExpressionBuilder {
        format: ctx.format,
        bindings: ctx.bindings,
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

    pm.get_analysis::<TypeCheck>()?;

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
    };

    for def in defs {
        components.push(compile_definition(def, &mut ctx)?);
    }

    Some(ir::Context {
        components,
        lib,
        bc: Default::default(),
        entrypoint: Id::new("main"),
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
    inputs: Vec<(Id, ir::RRC<ir::Port>)>,
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
