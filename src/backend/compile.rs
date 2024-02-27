//! FPCore to Calyx compiler.

use std::collections::HashMap;
use std::iter;

use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error, Id, NameGenerator};
use itertools::Itertools;

use super::builtins;
use super::libm::{MathLib, Prototype};
use super::stdlib::{Arguments, Primitive, Signature};
use crate::analysis::{Binding, ContextResolution, PassManager, TypeCheck};
use crate::format::Format;
use crate::fpcore::ast;
use crate::opts::Opts;

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
    context: &'a ContextResolution<'a>,
    libm: &'a mut HashMap<ast::NodeId, Prototype>,
}

impl ExpressionBuilder<'_, '_> {
    fn compile_number(&mut self, num: &ast::Number) -> CalyxResult<Expression> {
        let val = num.value.to_format(self.format).ok_or_else(|| {
            Error::misc(format!(
                "Constant value {} is not representable in the given format",
                num.value
            ))
            .with_pos(num)
        })?;

        let cell = self.builder.add_constant(val, u64::from(self.format.width));
        let port = cell.borrow().get("out");

        Ok(Expression::from_constant(port))
    }

    fn compile_constant(&mut self, constant: ast::Constant) -> Expression {
        match constant {
            ast::Constant::Math(_) => unimplemented!(),
            ast::Constant::Bool(val) => {
                let params = [1, u64::from(val)];

                let cell =
                    self.builder.add_primitive("const", "std_const", &params);
                let port = cell.borrow().get("out");

                Expression::from_constant(port)
            }
        }
    }

    fn compile_symbol(
        &self,
        sym: &ast::Symbol,
        uid: ast::NodeId,
    ) -> Expression {
        let port = match self.context.names[&uid] {
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

    fn get_primitive_operation(
        &self,
        op: &ast::Operation,
    ) -> Option<&'static Primitive<'static>> {
        match op.kind {
            ast::OpKind::Math(op) => match op {
                ast::MathOp::Add => Some(builtins::add(self.format)),
                ast::MathOp::Sub => Some(builtins::sub(self.format)),
                ast::MathOp::Mul => Some(builtins::mul(self.format)),
                ast::MathOp::Div => Some(builtins::div(self.format)),
                ast::MathOp::Neg => Some(builtins::neg(self.format)),
                ast::MathOp::Sqrt => Some(builtins::sqrt(self.format)),
                _ => None,
            },
            ast::OpKind::Test(op) => match op {
                ast::TestOp::Lt => Some(builtins::lt(self.format)),
                ast::TestOp::Gt => Some(builtins::gt(self.format)),
                ast::TestOp::Leq => Some(builtins::le(self.format)),
                ast::TestOp::Geq => Some(builtins::ge(self.format)),
                ast::TestOp::Eq => Some(builtins::eq(self.format)),
                ast::TestOp::Neq => Some(builtins::neq(self.format)),
                _ => None,
            },
            ast::OpKind::Tensor(_) => None,
        }
    }

    fn compile_operation(
        &mut self,
        op: &ast::Operation,
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

        let (cell, signature, is_comb) =
            if let Some(decl) = self.get_primitive_operation(op) {
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
                self.compile_operation(op, expr.uid, args)
            }
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
    context: &'a ContextResolution<'a>,
    libm: &'a mut HashMap<ast::NodeId, Prototype>,
}

fn compile_definition(
    def: &ast::FPCore,
    lib: &ir::LibrarySignatures,
    global: &mut GlobalContext,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| name_gen.gen_name("main"), |sym| sym.id);

    let mut ports = vec![ir::PortDef::new(
        "out",
        u64::from(global.format.width),
        ir::Direction::Output,
        Default::default(),
    )];

    ports.extend(def.args.iter().map(|arg| {
        ir::PortDef::new(
            arg.var.id,
            u64::from(global.format.width),
            ir::Direction::Input,
            Default::default(),
        )
    }));

    let mut component = ir::Component::new(name, ports, false, false, None);
    let mut builder = ir::Builder::new(&mut component, lib).not_generated();

    let mut expr_builder = ExpressionBuilder {
        builder: &mut builder,
        stores: HashMap::new(),
        format: global.format,
        context: global.context,
        libm: global.libm,
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

    let context = pm.get_analysis::<ContextResolution>()?;
    let _ = pm.get_analysis::<TypeCheck>()?;

    let MathLib {
        mut components,
        mut prototypes,
    } = MathLib::new(&pm, &mut lib)?;

    let mut global = GlobalContext {
        format: &opts.format,
        context,
        libm: &mut prototypes,
    };

    for def in defs {
        let component =
            compile_definition(def, &lib, &mut global, &mut name_gen)?;

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
