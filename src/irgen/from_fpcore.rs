//! FPCore to Calyx compiler.

use std::{cell::RefCell, rc::Rc};

use calyx_ir::{self as ir, build_assignments};
use calyx_utils::{CalyxResult, Error, NameGenerator};
use num::bigint::TryFromBigIntError;

use super::stdlib::{self, Arguments, Parameters, Primitive};
use crate::fpcore::ast;

/// Prefix for generated groups.
const GROUP_PREFIX: &str = "grp";

/// Prefix for components generated from anonymous FPCores.
const ANONYMOUS_PREFIX: &str = "anonymous";

fn get_literal_val(num: &ast::Number) -> CalyxResult<u64> {
    match num {
        ast::Number::Rational(rational) => {
            // Assuming simple constants for now
            assert!(!rational.is_negative());
            assert!(rational.value.is_integer());

            rational.value.to_integer().try_into().map_err(
                |err: TryFromBigIntError<_>| {
                    Error::misc(format!(
                        "Constant value {} cannot fit in 64 bits",
                        err.into_original()
                    ))
                },
            )
        }
        ast::Number::Digits { .. } => unimplemented!(),
    }
}

fn compile_number(
    num: &ast::Number,
    width: u64,
    builder: &mut ir::Builder,
) -> CalyxResult<ir::RRC<ir::Port>> {
    let cell = builder.add_constant(get_literal_val(num)?, width);
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

fn get_primitive_operation(op: &ast::Operation) -> &Primitive {
    match op {
        ast::Operation::Math(op) => match op {
            ast::MathOp::Add => &stdlib::compile::STD_ADD,
            ast::MathOp::Sub => &stdlib::core::STD_SUB,
            ast::MathOp::Mul => &stdlib::binary_operators::STD_MULT_PIPE,
            ast::MathOp::Div => {
                &stdlib::binary_operators::STD_DIV_PIPE_QUOTIENT
            }
            ast::MathOp::Sqrt => &stdlib::math::STD_SQRT,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn compile_operation(
    op: &ast::Operation,
    args: &[ast::Expression],
    width: u64,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    let prim_decl = get_primitive_operation(op);

    let (arg_ports, arg_ctrl): (Vec<_>, Vec<_>) = itertools::process_results(
        args.iter()
            .map(|arg| compile_expression(arg, width, builder)),
        |iter| iter.unzip(),
    )?;

    let prim = builder.add_primitive(
        prim_decl.prefix_hint,
        prim_decl.name,
        &Parameters::build_bitnum_params(width),
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
    width: u64,
    builder: &mut ir::Builder,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    match expr {
        ast::Expression::Num(num) => {
            Ok((compile_number(num, width, builder)?, ir::Control::empty()))
        }
        ast::Expression::Id(id) => {
            Ok((get_symbol(id, builder.component)?, ir::Control::empty()))
        }
        ast::Expression::Op(op, args) => {
            compile_operation(op, args, width, builder)
        }
        _ => unimplemented!(),
    }
}

fn compile_benchmark(
    def: &ast::BenchmarkDef,
    width: u64,
    lib: &ir::LibrarySignatures,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def.name.as_ref().map_or_else(
        || name_gen.gen_name(ANONYMOUS_PREFIX),
        ast::Symbol::as_id,
    );

    let mut ports = vec![ir::PortDef {
        name: "out".into(),
        width,
        direction: ir::Direction::Output,
        attributes: Default::default(),
    }];

    ports.extend(def.args.iter().map(|arg| match arg {
        ast::ArgumentDef::Id(id) => ir::PortDef {
            name: id.as_id(),
            width,
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

    let (port, control) = compile_expression(&def.body, width, &mut builder)?;

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
    width: u64,
    math_lib: ir::LibrarySignatures,
) -> CalyxResult<ir::Context> {
    let mut name_gen = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|id| id.as_id()))
            .collect(),
    );

    let comps: Vec<_> = defs
        .iter()
        .map(|def| compile_benchmark(def, width, &math_lib, &mut name_gen))
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
