//! FPCore to Calyx compiler.

use std::collections::HashMap;
use std::iter;

use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error, Id, NameGenerator};
use itertools::Itertools;

use super::libm::{MathLib, Prototype};
use super::stdlib::{Arguments, Primitive, Signature};
use crate::analysis::{Binding, ContextResolution, PassManager, TypeCheck};
use crate::format::Format;
use crate::fpcore::ast;
use crate::functions::builtins;
use crate::opts::Opts;

/// Prefix for components generated from anonymous FPCores.
const ANONYMOUS_PREFIX: &str = "anonymous";

struct Context<'a, 'b> {
    builder: &'a mut ir::Builder<'b>,
    libm: &'a mut HashMap<ast::NodeId, Prototype>,
    format: &'a Format,
    resolved: &'a ContextResolution<'b>,
    stores: HashMap<ast::NodeId, ir::RRC<ir::Cell>>,
}

fn compile_number(
    num: &ast::Number,
    ctx: &mut Context,
) -> CalyxResult<ir::RRC<ir::Port>> {
    let val = num.value.to_format(ctx.format).ok_or_else(|| {
        Error::misc(format!(
            "Constant value {} is not representable in the given format",
            num.value
        ))
        .with_pos(num)
    })?;

    let cell = ctx.builder.add_constant(val, ctx.format.width);
    let port = cell.borrow().get("out");

    Ok(port)
}

fn compile_constant(
    constant: ast::Constant,
    ctx: &mut Context,
) -> ir::RRC<ir::Port> {
    match constant {
        ast::Constant::Math(_) => unimplemented!(),
        ast::Constant::Bool(val) => {
            let params = [1, val.into()];

            let cell = ctx.builder.add_primitive("const", "std_const", &params);
            let port = cell.borrow().get("out");

            port
        }
    }
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
            ast::MathOp::Add => Some(builtins::add(format)),
            ast::MathOp::Sub => Some(builtins::sub(format)),
            ast::MathOp::Mul => Some(builtins::mul(format)),
            ast::MathOp::Div => Some(builtins::div(format)),
            ast::MathOp::Sqrt => Some(builtins::sqrt(format)),
            _ => None,
        },
        ast::OpKind::Test(op) => match op {
            ast::TestOp::Lt => Some(builtins::lt(format)),
            ast::TestOp::Gt => Some(builtins::gt(format)),
            ast::TestOp::Leq => Some(builtins::le(format)),
            ast::TestOp::Geq => Some(builtins::ge(format)),
            ast::TestOp::Eq => Some(builtins::eq(format)),
            ast::TestOp::Neq => Some(builtins::neq(format)),
            _ => None,
        },
        ast::OpKind::Tensor(_) => None,
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
        } else if let Some(proto) = ctx.libm.remove(&uid) {
            let comp = ctx.builder.add_component(
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
    let (args, stores): (Vec<_>, Vec<_>) = itertools::process_results(
        binders.iter().map(|binder| {
            let (port, control) = compile_expression(&binder.expr, ctx)?;

            let params = [port.borrow().width];
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
        ast::ExprKind::Const(constant) => {
            Ok((compile_constant(*constant, ctx), ir::Control::empty()))
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
    libm: &mut HashMap<ast::NodeId, Prototype>,
    name_gen: &mut NameGenerator,
) -> CalyxResult<ir::Component> {
    let name = def
        .name
        .as_ref()
        .map_or_else(|| name_gen.gen_name(ANONYMOUS_PREFIX), |sym| sym.id);

    let mut ports = vec![ir::PortDef::new(
        "out",
        format.width,
        ir::Direction::Output,
        Default::default(),
    )];

    ports.extend(def.args.iter().map(|arg| {
        ir::PortDef::new(
            arg.var.id,
            format.width,
            ir::Direction::Input,
            Default::default(),
        )
    }));

    let mut component = ir::Component::new(name, ports, false, false, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let mut context = Context {
        builder: &mut builder,
        libm,
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
    builder.component.control = ir::rrc(control);

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

    let MathLib {
        mut components,
        mut prototypes,
    } = MathLib::new(&pm, &mut lib)?;

    for def in defs {
        let component = compile_benchmark(
            def,
            &opts.format,
            context,
            &lib,
            &mut prototypes,
            &mut name_gen,
        )?;

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
