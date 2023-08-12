//! FPCore to Calyx compiler.

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;

use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error, Id, NameGenerator};
use itertools::Itertools;

use super::libm::MathLib;
use super::stdlib::{Arguments, Primitive, Signature};
use crate::analysis::{Binding, ContextResolution, PassManager, TypeCheck};
use crate::format::Format;
use crate::fpcore::ast;
use crate::functions::{builtins, lookup};
use crate::opts::Opts;
use crate::utils::mangling;

/// Prefix for components generated from anonymous FPCores.
const ANONYMOUS_PREFIX: &str = "anonymous";

struct Context<'a, 'b> {
    builder: &'a mut ir::Builder<'b>,
    format: &'a Format,
    resolved: &'a ContextResolution<'b>,
    stores: HashMap<ast::NodeId, ir::RRC<ir::Cell>>,
}

fn compile_number(
    num: &ast::Number,
    ctx: &mut Context,
) -> CalyxResult<ir::RRC<ir::Port>> {
    let val = num.rational.to_format(ctx.format).ok_or_else(|| {
        Error::misc(format!(
            "Constant value {} is not representable in the given format",
            num.rational
        ))
        .with_pos(num)
    })?;

    let cell = ctx.builder.add_constant(val, ctx.format.width);
    let port = cell.borrow().get("out");

    Ok(port)
}

fn compile_symbol(
    sym: &ast::Symbol,
    uid: ast::NodeId,
    ctx: &mut Context,
) -> ir::RRC<ir::Port> {
    match ctx.resolved.names[&uid] {
        Binding::Argument(_) => {
            let signature = ctx.builder.component.signature.borrow();

            signature.get(sym.id)
        }
        Binding::Let(binder) => {
            let cell = &ctx.stores[&binder.expr.uid];
            let port = cell.borrow().get("out");

            port
        }
    }
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
    ctx: &mut Context,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    let (arg_ports, arg_ctrl): (Vec<_>, Vec<_>) = itertools::process_results(
        args.iter().map(|arg| compile_expression(arg, ctx)),
        |iter| iter.unzip(),
    )?;

    let (cell, signature, is_comb) =
        if let Some(decl) = get_primitive_operation(op, ctx.format) {
            let prim = ctx.builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(ctx.format),
            );

            (prim, &decl.signature, decl.is_comb)
        } else {
            let local = ctx.resolved.props[&uid];

            let name = match op.kind {
                ast::OpKind::Math(op) => mangling::mangle_function(
                    op.try_into().unwrap(),
                    ctx.format,
                    local.domain.unwrap(),
                    local.strategy.unwrap(),
                ),
                _ => unreachable!(),
            };

            let cell = ctx.builder.add_component(
                Id::new("fn"),
                Id::new(name),
                lookup::signature(1, ctx.format),
            );

            (cell, &Signature::UNARY_DEFAULT, true)
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

    let out = cell.borrow().get(signature.output);

    let control = if is_comb {
        let assigns = inputs
            .into_iter()
            .map(|(dst, src)| {
                ctx.builder.build_assignment(
                    cell.borrow().get(dst),
                    src,
                    ir::Guard::True,
                )
            })
            .collect();

        ctx.builder.add_continuous_assignments(assigns);

        arg_ctrl
    } else {
        let invoke = ir::Control::invoke(cell, inputs, vec![]);

        collapse([arg_ctrl, invoke], ir::Control::seq)
    };

    Ok((out, control))
}

fn compile_let(
    binders: &[ast::Binder],
    body: &ast::Expression,
    sequential: bool,
    ctx: &mut Context,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    let params = [ctx.format.width];

    let (args, stores): (Vec<_>, Vec<_>) = itertools::process_results(
        binders.iter().map(|binder| {
            let (port, control) = compile_expression(&binder.expr, ctx)?;

            let reg = ctx.builder.add_primitive("r", "std_reg", &params);

            let invoke = ir::Control::invoke(
                reg.clone(),
                vec![(Id::new("in"), port)],
                vec![],
            );

            ctx.stores.insert(binder.expr.uid, reg);

            CalyxResult::Ok((control, invoke))
        }),
        |iter| iter.unzip(),
    )?;

    let (port, body) = compile_expression(body, ctx)?;

    let control = if sequential {
        collapse(
            itertools::interleave(args, stores).chain(iter::once(body)),
            ir::Control::seq,
        )
    } else {
        collapse(
            [
                collapse(args, ir::Control::par),
                collapse(stores, ir::Control::par),
                body,
            ],
            ir::Control::seq,
        )
    };

    Ok((port, control))
}

fn compile_expression(
    expr: &ast::Expression,
    ctx: &mut Context,
) -> CalyxResult<(ir::RRC<ir::Port>, ir::Control)> {
    match &expr.kind {
        ast::ExprKind::Num(num) => {
            Ok((compile_number(num, ctx)?, ir::Control::empty()))
        }
        ast::ExprKind::Id(sym) => {
            Ok((compile_symbol(sym, expr.uid, ctx), ir::Control::empty()))
        }
        ast::ExprKind::Op(op, args) => {
            compile_operation(op, expr.uid, args, ctx)
        }
        ast::ExprKind::Let {
            binders,
            body,
            sequential,
        } => compile_let(binders, body, *sequential, ctx),
        ast::ExprKind::Annotation { props: _, body } => {
            compile_expression(body, ctx)
        }
        _ => unimplemented!(),
    }
}

fn compile_benchmark(
    def: &ast::BenchmarkDef,
    format: &Format,
    resolved: &ContextResolution,
    lib: &ir::LibrarySignatures,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| name_gen.gen_name(ANONYMOUS_PREFIX), |sym| sym.id);

    let mut ports = vec![ir::PortDef {
        name: Id::new("out"),
        width: format.width,
        direction: ir::Direction::Output,
        attributes: Default::default(),
    }];

    ports.extend(def.args.iter().map(|arg| ir::PortDef {
        name: arg.var.id,
        width: format.width,
        direction: ir::Direction::Input,
        attributes: Default::default(),
    }));

    let mut component = ir::Component::new(name, ports, false, false, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let mut context = Context {
        builder: &mut builder,
        format,
        resolved,
        stores: HashMap::new(),
    };

    let (port, control) = compile_expression(&def.body, &mut context)?;

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
    opts: &Opts,
    mut lib: ir::LibrarySignatures,
) -> CalyxResult<ir::Context> {
    let mut name_gen = NameGenerator::with_prev_defined_names(
        defs.iter()
            .filter_map(|def| def.name.as_ref().map(|sym| sym.id))
            .collect(),
    );

    let pm = PassManager::new(opts, defs);

    let context: &ContextResolution = pm.get_analysis()?;
    let _: &TypeCheck = pm.get_analysis()?;

    let libm = MathLib::new(&pm, &mut lib)?;

    let mut components: Vec<_> = defs
        .iter()
        .map(|def| {
            compile_benchmark(def, &opts.format, context, &lib, &mut name_gen)
        })
        .collect::<CalyxResult<_>>()?;

    components.extend(libm.components);

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
