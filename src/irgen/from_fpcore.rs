//! FPCore to Calyx compiler.

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use calyx_ir::{self as ir, build_assignments};
use calyx_utils::{CalyxResult, Error, Id, NameGenerator};

use super::libm::MathLib;
use super::stdlib::{Arguments, Primitive, Signature};
use crate::analysis::context::ContextResolution;
use crate::format::Format;
use crate::fpcore::ast;
use crate::functions::{builtins, lookup};
use crate::utils::mangling;

/// Prefix for components generated from anonymous FPCores.
const ANONYMOUS_PREFIX: &str = "anonymous";

struct LocalContext<'a> {
    format: &'a Format,
    resolution: &'a ContextResolution<'a>,
}

fn compile_number(
    num: &ast::Number,
    format: &Format,
    builder: &mut ir::Builder,
) -> CalyxResult<ir::RRC<ir::Port>> {
    let val = num.rational.to_format(format).ok_or_else(|| {
        Error::misc(format!(
            "Constant value {} is not representable in the given format",
            num.rational
        ))
        .with_pos(num)
    })?;

    let cell = builder.add_constant(val, format.width);
    let port = cell.borrow().get("out");

    Ok(port)
}

fn compile_symbol(
    sym: &ast::Symbol,
    comp: &ir::Component,
) -> CalyxResult<ir::RRC<ir::Port>> {
    // We don't support let bindings (or any other sort of bindings) yet, so all
    // symbols must refer to ports in the component signature.
    let signature = comp.signature.borrow();

    signature.find(sym.id).ok_or_else(|| {
        Error::misc(format!("Undefined symbol `{}`", sym.id)).with_pos(sym)
    })
}

fn get_primitive_operation(
    op: &ast::Operation,
    format: &Format,
) -> Option<&'static Primitive<'static>> {
    match op.kind {
        ast::OpKind::Math(op) => match op {
            ast::MathOp::Add => Some(builtins::primitive_adder(format)),
            ast::MathOp::Sub => Some(builtins::primitive_subtractor(format)),
            ast::MathOp::Mul => Some(builtins::primitive_multiplier(format)),
            ast::MathOp::Div => Some(builtins::primitive_divider(format)),
            ast::MathOp::Sqrt => Some(builtins::primitive_sqrt(format)),
            _ => None,
        },
        ast::OpKind::Test(_) => unimplemented!(),
        ast::OpKind::Tensor(_) => unimplemented!(),
    }
}

fn compile_operation(
    op: &ast::Operation,
    uid: ast::NodeId,
    args: &[ast::Expression],
    context: &LocalContext,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    let (arg_ports, arg_ctrl): (Vec<_>, Vec<_>) = itertools::process_results(
        args.iter()
            .map(|arg| compile_expression(arg, context, builder)),
        |iter| iter.unzip(),
    )?;

    let (cell, signature, is_comb) =
        if let Some(decl) = get_primitive_operation(op, context.format) {
            let prim = builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(context.format),
            );

            (prim, &decl.signature, decl.is_comb)
        } else {
            let local = context.resolution[uid];

            let name = match op.kind {
                ast::OpKind::Math(op) => mangling::mangle_function(
                    op.try_into().unwrap(),
                    context.format,
                    local.domain.unwrap(),
                    local.strategy.unwrap(),
                ),
                _ => unreachable!(),
            };

            let cell = builder.add_component(
                Id::new("fn"),
                Id::new(name),
                lookup::signature(context.format),
            );

            (cell, &Signature::UNARY_DEFAULT, true)
        };

    let mut assigns = match signature.args {
        Arguments::Unary { input: cell_in } => {
            if let [arg] = &arg_ports[..] {
                vec![builder.build_assignment(
                    cell.borrow().get(cell_in),
                    arg.clone(),
                    ir::Guard::True,
                )]
            } else {
                return Err(Error::misc(format!(
                    "Operation expects 1 argument, got {}",
                    arg_ports.len()
                ))
                .with_pos(op));
            }
        }
        Arguments::Binary {
            left: cell_left,
            right: cell_right,
        } => {
            if let [left_arg, right_arg] = &arg_ports[..] {
                vec![
                    builder.build_assignment(
                        cell.borrow().get(cell_left),
                        left_arg.clone(),
                        ir::Guard::True,
                    ),
                    builder.build_assignment(
                        cell.borrow().get(cell_right),
                        right_arg.clone(),
                        ir::Guard::True,
                    ),
                ]
            } else {
                return Err(Error::misc(format!(
                    "Operation expects 2 arguments, got {}",
                    arg_ports.len()
                ))
                .with_pos(op));
            }
        }
    };

    let arg_ctrl: Vec<_> = arg_ctrl
        .into_iter()
        .filter(|ctrl| !matches!(ctrl, ir::Control::Empty(_)))
        .collect();

    let arg_ctrl = match arg_ctrl.len() {
        0 => ir::Control::empty(),
        1 => arg_ctrl.into_iter().next().unwrap(),
        _ => ir::Control::par(arg_ctrl),
    };

    let out = cell.borrow().get(signature.output);

    let ctrl = if is_comb {
        builder.add_continuous_assignments(assigns);

        arg_ctrl
    } else {
        let group = builder.add_group("op");
        let one = builder.add_constant(1, 1);

        assigns.extend(build_assignments!(builder;
            cell["go"] = ? one["out"];
            group["done"] = ? cell["done"];
        ));

        group.borrow_mut().assignments = assigns;

        let enable = ir::Control::enable(group);

        if let ir::Control::Empty(_) = arg_ctrl {
            enable
        } else {
            ir::Control::seq(vec![arg_ctrl, enable])
        }
    };

    Ok((out, ctrl))
}

fn compile_expression(
    expr: &ast::Expression,
    context: &LocalContext,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    match &expr.kind {
        ast::ExprKind::Num(num) => Ok((
            compile_number(num, context.format, builder)?,
            ir::Control::empty(),
        )),
        ast::ExprKind::Id(sym) => Ok((
            compile_symbol(sym, builder.component)?,
            ir::Control::empty(),
        )),
        ast::ExprKind::Op(op, args) => {
            compile_operation(op, expr.uid, args, context, builder)
        }
        ast::ExprKind::Annotation { props: _, body } => {
            compile_expression(body, context, builder)
        }
        _ => unimplemented!(),
    }
}

fn compile_benchmark(
    def: &ast::BenchmarkDef,
    context: &LocalContext,
    lib: &ir::LibrarySignatures,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| name_gen.gen_name(ANONYMOUS_PREFIX), |sym| sym.id);

    let mut ports = vec![ir::PortDef {
        name: Id::new("out"),
        width: context.format.width,
        direction: ir::Direction::Output,
        attributes: Default::default(),
    }];

    ports.extend(def.args.iter().map(|arg| match arg {
        ast::ArgumentDef::Id(sym) => ir::PortDef {
            name: sym.id,
            width: context.format.width,
            direction: ir::Direction::Input,
            attributes: Default::default(),
        },
        _ => unimplemented!(),
    }));

    let mut component = ir::Component::new(name, ports, false, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let (port, control) = compile_expression(&def.body, context, &mut builder)?;

    let assigns = vec![builder.build_assignment(
        builder.component.signature.borrow().get("out"),
        port,
        ir::Guard::True,
    )];

    let is_comb = matches!(control, ir::Control::Empty(_));

    builder.add_continuous_assignments(assigns);
    builder.component.control = Rc::new(RefCell::new(control));

    component.is_comb = is_comb;

    Ok(component)
}

pub fn compile_fpcore<I>(
    defs: &[ast::BenchmarkDef],
    format: &Format,
    math_lib: I,
) -> CalyxResult<ir::Context>
where
    I: IntoIterator<Item = (Option<PathBuf>, Vec<ir::Primitive>)>,
{
    let mut name_gen = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
            .collect(),
    );

    let resolution = ContextResolution::new(defs)?;
    let libm = MathLib::new(defs, format, &resolution, math_lib)?;

    let context = LocalContext {
        format,
        resolution: &resolution,
    };

    let mut components: Vec<_> = defs
        .iter()
        .map(|def| compile_benchmark(def, &context, &libm.lib, &mut name_gen))
        .collect::<CalyxResult<_>>()?;

    components.extend(libm.components);

    Ok(ir::Context {
        components,
        lib: libm.lib,
        bc: Default::default(),
        entrypoint: Id::new("main"),
        extra_opts: Vec::new(),
        metadata: None,
    })
}
