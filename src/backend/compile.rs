//! FPCore to Calyx compiler.

use std::collections::HashMap;
use std::iter;

use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error, Id, NameGenerator};
use itertools::Itertools;

use super::builtins;
use super::libm::{MathLib, Prototype};
use super::stdlib::{self, Arguments, Primitive, Signature};
use crate::analysis::{Binding, NameResolution, PassManager, TypeCheck};
use crate::format::Format;
use crate::fpcore::ast;
use crate::opts::Opts;
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

struct ExpressionBuilder<'a, 'b> {
    builder: &'a mut ir::Builder<'b>,
    stores: HashMap<ast::NodeId, ir::RRC<ir::Cell>>,
    format: &'a Format,
    bindings: &'a NameResolution<'a>,
    libm: &'a mut HashMap<ast::NodeId, Prototype>,
}

impl ExpressionBuilder<'_, '_> {
    fn compile_number(&mut self, num: &ast::Number) -> CalyxResult<Expression> {
        let val = num
            .value
            .to_fixed_point(self.format)
            .ok_or_else(|| {
                Error::misc("Constant is not representable").with_pos(num)
            })
            .and_then(|val| {
                u64::try_from(&val)
                    .map_err(|_| Error::misc("Constant width exceeds 64 bits"))
            })?;

        let cell = self.builder.add_constant(val, u64::from(self.format.width));
        let port = cell.borrow().get("out");

        Ok(Expression::from_constant(port))
    }

    fn compile_constant(&mut self, constant: ast::Constant) -> Expression {
        let params = match constant {
            ast::Constant::Math(_) => unimplemented!(),
            ast::Constant::Bool(val) => [1, u64::from(val)],
        };

        let cell = self.builder.add_primitive("c", "std_const", &params);
        let port = cell.borrow().get("out");

        Expression::from_constant(port)
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
            _ => unimplemented!(),
        };

        Expression::from_constant(port)
    }

    fn compile_variadic_operation(
        &mut self,
        op: ast::TestOp,
        args: &[ast::Expression],
    ) -> CalyxResult<Expression> {
        let mut control = Vec::new();
        let mut assignments = Vec::new();

        let args: Vec<_> = args
            .iter()
            .map(|arg| {
                let expr = self.compile_expression(arg)?;

                control.push(expr.control);
                assignments.extend(expr.assignments);

                Ok(expr.out)
            })
            .collect::<CalyxResult<_>>()?;

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

        Ok(Expression {
            control,
            assignments,
            out,
        })
    }

    fn compile_library_operation(
        &mut self,
        op: ast::OpKind,
        uid: ast::NodeId,
        args: &[ast::Expression],
    ) -> CalyxResult<Expression> {
        let (arg_ctrl, arg_assigns, arg_ports): (Vec<_>, Vec<_>, Vec<_>) =
            itertools::process_results(
                args.iter().map(|arg| {
                    let expr = self.compile_expression(arg)?;

                    CalyxResult::Ok((expr.control, expr.assignments, expr.out))
                }),
                |iter| iter.multiunzip(),
            )?;

        let decl = match op {
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
            unimplemented!()
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

        Ok(Expression {
            control,
            assignments,
            out,
        })
    }

    fn compile_operation(
        &mut self,
        op: ast::OpKind,
        uid: ast::NodeId,
        args: &[ast::Expression],
    ) -> CalyxResult<Expression> {
        match op {
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
    ) -> CalyxResult<Expression> {
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

                Ok(Expression {
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

                Ok(Expression {
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
    ) -> CalyxResult<Expression> {
        let (args, stores): (Vec<_>, Vec<_>) = itertools::process_results(
            bindings.iter().map(|binding| {
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

                CalyxResult::Ok((expr.control, invoke))
            }),
            |iter| iter.unzip(),
        )?;

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

        Ok(Expression { control, ..body })
    }

    fn compile_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> CalyxResult<Expression> {
        match &expr.kind {
            ast::ExprKind::Num(num) => self.compile_number(num),
            ast::ExprKind::Const(constant) => {
                Ok(self.compile_constant(*constant))
            }
            ast::ExprKind::Id(sym) => Ok(self.compile_symbol(sym, expr.uid)),
            ast::ExprKind::Op(op, args) => {
                self.compile_operation(op.kind, expr.uid, args)
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
            _ => unimplemented!(),
        }
    }
}

struct GlobalContext<'a> {
    format: &'a Format,
    bindings: &'a NameResolution<'a>,
    libm: &'a mut HashMap<ast::NodeId, Prototype>,
}

fn compile_definition(
    def: &ast::FPCore,
    lib: &ir::LibrarySignatures,
    context: &mut GlobalContext,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| name_gen.gen_name("main"), |sym| sym.id);

    let mut ports = vec![ir::PortDef::new(
        "out",
        u64::from(context.format.width),
        ir::Direction::Output,
        Default::default(),
    )];

    ports.extend(def.args.iter().map(|arg| {
        ir::PortDef::new(
            arg.var.id,
            u64::from(context.format.width),
            ir::Direction::Input,
            Default::default(),
        )
    }));

    let mut component = ir::Component::new(name, ports, false, false, None);
    let mut builder = ir::Builder::new(&mut component, lib).not_generated();

    let mut expr_builder = ExpressionBuilder {
        builder: &mut builder,
        stores: HashMap::new(),
        format: context.format,
        bindings: context.bindings,
        libm: context.libm,
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

    Ok(component)
}

pub fn compile_fpcore(
    defs: &[ast::FPCore],
    opts: &Opts,
    mut lib: ir::LibrarySignatures,
) -> CalyxResult<ir::Context> {
    let mut name_gen = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
            .collect(),
    );

    let pm = PassManager::new(opts, defs);

    pm.get_analysis::<TypeCheck>()?;

    let MathLib {
        mut components,
        mut prototypes,
    } = MathLib::new(&pm, &mut lib)?;

    let mut context = GlobalContext {
        format: &opts.format,
        bindings: pm.get_analysis()?,
        libm: &mut prototypes,
    };

    for def in defs {
        let component =
            compile_definition(def, &lib, &mut context, &mut name_gen)?;

        components.push(component);
    }

    Ok(ir::Context {
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
