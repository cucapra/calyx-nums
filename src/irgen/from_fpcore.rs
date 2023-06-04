//! FPCore to Calyx compiler.

use std::{cell::RefCell, rc::Rc};

use calyx_ir::{self as ir, build_assignments};
use calyx_utils::{CalyxResult, Error, NameGenerator};

use super::stdlib::{self, Arguments, Primitive};
use crate::format::Format;
use crate::fpcore::ast;

/// Prefix for generated groups.
const GROUP_PREFIX: &str = "grp";

/// Prefix for components generated from anonymous FPCores.
const ANONYMOUS_PREFIX: &str = "anonymous";

fn get_constant_value(num: &ast::Number, format: &Format) -> CalyxResult<u64> {
    let rational = match num {
        ast::Number::Rational(rational) => rational,
        ast::Number::Digits { .. } => unimplemented!(),
    };

    let conv = if format.is_signed {
        ast::Rational::to_fixed_point
    } else {
        ast::Rational::to_unsigned_fixed_point
    };

    conv(rational, format.width, format.frac_width).ok_or_else(|| {
        Error::misc(format!(
            "Constant value {} is not representable in the given format",
            rational
        ))
    })
}

fn compile_number(
    num: &ast::Number,
    format: &Format,
    builder: &mut ir::Builder,
) -> CalyxResult<ir::RRC<ir::Port>> {
    let val = get_constant_value(num, format)?;

    let cell = builder.add_constant(val, format.width);
    let port = cell.borrow().get("out");

    Ok(port)
}

fn get_symbol(
    id: &ast::Symbol,
    comp: &ir::Component,
) -> CalyxResult<ir::RRC<ir::Port>> {
    // We don't support let bindings (or any other sort of bindings) yet, so all
    // symbols must refer to ports in the component signature.
    let signature = comp.signature.borrow();

    signature
        .find(id.as_id())
        .ok_or_else(|| Error::misc(format!("Undefined symbol `{id}`")))
}

fn get_primitive_adder(
    fixed_point: bool,
    signed: bool,
) -> &'static Primitive<'static> {
    match (fixed_point, signed) {
        (false, false) => &stdlib::compile::STD_ADD,
        (false, true) => &stdlib::binary_operators::STD_SADD,
        (true, false) => &stdlib::binary_operators::STD_FP_ADD,
        (true, true) => &stdlib::binary_operators::STD_FP_SADD,
    }
}

fn get_primitive_subtractor(
    fixed_point: bool,
    signed: bool,
) -> &'static Primitive<'static> {
    match (fixed_point, signed) {
        (false, false) => &stdlib::core::STD_SUB,
        (false, true) => &stdlib::binary_operators::STD_SSUB,
        (true, false) => &stdlib::binary_operators::STD_FP_SUB,
        (true, true) => &stdlib::binary_operators::STD_FP_SSUB,
    }
}

fn get_primitive_multiplier(
    fixed_point: bool,
    signed: bool,
) -> &'static Primitive<'static> {
    match (fixed_point, signed) {
        (false, false) => &stdlib::binary_operators::STD_MULT_PIPE,
        (false, true) => &stdlib::binary_operators::STD_SMULT_PIPE,
        (true, false) => &stdlib::binary_operators::STD_FP_MULT_PIPE,
        (true, true) => &stdlib::binary_operators::STD_FP_SMULT_PIPE,
    }
}

fn get_primitive_divider(
    fixed_point: bool,
    signed: bool,
) -> &'static Primitive<'static> {
    match (fixed_point, signed) {
        (false, false) => &stdlib::binary_operators::STD_DIV_PIPE_QUOTIENT,
        (false, true) => &stdlib::binary_operators::STD_SDIV_PIPE_QUOTIENT,
        (true, false) => &stdlib::binary_operators::STD_FP_DIV_PIPE_QUOTIENT,
        (true, true) => &stdlib::binary_operators::STD_FP_SDIV_PIPE_QUOTIENT,
    }
}

fn get_primitive_operation(
    op: &ast::Operation,
    format: &Format,
) -> &'static Primitive<'static> {
    let is_fixed_point = format.frac_width != 0;

    match op {
        ast::Operation::Math(op) => match op {
            ast::MathOp::Add => {
                get_primitive_adder(is_fixed_point, format.is_signed)
            }
            ast::MathOp::Sub => {
                get_primitive_subtractor(is_fixed_point, format.is_signed)
            }
            ast::MathOp::Mul => {
                get_primitive_multiplier(is_fixed_point, format.is_signed)
            }
            ast::MathOp::Div => {
                get_primitive_divider(is_fixed_point, format.is_signed)
            }
            ast::MathOp::Sqrt => {
                if format.is_signed {
                    log::debug!("Using sqrt with a signed format");
                }

                match is_fixed_point {
                    false => &stdlib::math::STD_SQRT,
                    true => &stdlib::math::STD_FP_SQRT,
                }
            }
            _ => unimplemented!(),
        },
        ast::Operation::Test(_) => unimplemented!(),
        ast::Operation::Tensor(_) => unimplemented!(),
    }
}

fn compile_operation(
    op: &ast::Operation,
    args: &[ast::Expression],
    format: &Format,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    let prim_decl = get_primitive_operation(op, format);

    let (arg_ports, arg_ctrl): (Vec<_>, Vec<_>) = itertools::process_results(
        args.iter()
            .map(|arg| compile_expression(arg, format, builder)),
        |iter| iter.unzip(),
    )?;

    let prim = builder.add_primitive(
        prim_decl.prefix_hint,
        prim_decl.name,
        &prim_decl.build_params(format),
    );

    let mut assigns = match prim_decl.signature.args {
        Arguments::Unary { input: prim_in } => {
            if let [arg] = &arg_ports[..] {
                vec![builder.build_assignment(
                    prim.borrow().get(prim_in),
                    arg.clone(),
                    ir::Guard::True,
                )]
            } else {
                return Err(Error::misc(format!(
                    "Operation {op:?} expects 1 argument, got {}",
                    arg_ports.len()
                )));
            }
        }
        Arguments::Binary {
            left: prim_left,
            right: prim_right,
        } => {
            if let [left_arg, right_arg] = &arg_ports[..] {
                vec![
                    builder.build_assignment(
                        prim.borrow().get(prim_left),
                        left_arg.clone(),
                        ir::Guard::True,
                    ),
                    builder.build_assignment(
                        prim.borrow().get(prim_right),
                        right_arg.clone(),
                        ir::Guard::True,
                    ),
                ]
            } else {
                return Err(Error::misc(format!(
                    "Operation {op:?} expects 2 arguments, got {}",
                    arg_ports.len()
                )));
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

    let out = prim.borrow().get(prim_decl.signature.output);

    let ctrl = if prim_decl.is_comb {
        builder.add_continuous_assignments(assigns);

        arg_ctrl
    } else {
        let group = builder.add_group(GROUP_PREFIX);
        let one = builder.add_constant(1, 1);

        assigns.extend(build_assignments!(builder;
            prim["go"] = ? one["out"];
            group["done"] = ? prim["done"];
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
    format: &Format,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    match expr {
        ast::Expression::Num(num) => {
            Ok((compile_number(num, format, builder)?, ir::Control::empty()))
        }
        ast::Expression::Id(id) => {
            Ok((get_symbol(id, builder.component)?, ir::Control::empty()))
        }
        ast::Expression::Op(op, args) => {
            compile_operation(op, args, format, builder)
        }
        _ => unimplemented!(),
    }
}

fn compile_benchmark(
    def: &ast::BenchmarkDef,
    format: &Format,
    lib: &ir::LibrarySignatures,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def.name.as_ref().map_or_else(
        || name_gen.gen_name(ANONYMOUS_PREFIX),
        ast::Symbol::as_id,
    );

    let mut ports = vec![ir::PortDef {
        name: "out".into(),
        width: format.width,
        direction: ir::Direction::Output,
        attributes: Default::default(),
    }];

    ports.extend(def.args.iter().map(|arg| match arg {
        ast::ArgumentDef::Id(id) => ir::PortDef {
            name: id.as_id(),
            width: format.width,
            direction: ir::Direction::Input,
            attributes: Default::default(),
        },
        _ => unimplemented!(),
    }));

    for prop in &def.props {
        log::warn!("Ignoring property `{}`", prop.name);
    }

    let mut component = ir::Component::new(name, ports, false, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let (port, control) = compile_expression(&def.body, format, &mut builder)?;

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

pub fn compile_fpcore(
    defs: &[ast::BenchmarkDef],
    format: &Format,
    math_lib: ir::LibrarySignatures,
) -> CalyxResult<ir::Context> {
    let mut name_gen = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|id| id.as_id()))
            .collect(),
    );

    let comps: Vec<_> = defs
        .iter()
        .map(|def| compile_benchmark(def, format, &math_lib, &mut name_gen))
        .collect::<CalyxResult<_>>()?;

    Ok(ir::Context {
        components: comps,
        lib: math_lib,
        bc: ir::BackendConf::default(),
        entrypoint: "main".into(),
        extra_opts: Vec::new(),
        metadata: None,
    })
}
