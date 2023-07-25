//! Math library construction.

use std::collections::HashSet;
use std::iter;
use std::path::PathBuf;

use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use crate::analysis::context::ContextResolution;
use crate::format::Format;
use crate::fpcore::ast;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::visitor::{self, Visitor};
use crate::functions::{lookup, lut, remez};
use crate::utils::mangling;
use crate::utils::sollya::SollyaFunction;

pub struct MathLib {
    pub components: Vec<ir::Component>,
    pub lib: ir::LibrarySignatures,
}

impl MathLib {
    pub fn new<I>(
        defs: &[ast::BenchmarkDef],
        format: &Format,
        context: &ContextResolution,
        core_lib: I,
    ) -> CalyxResult<MathLib>
    where
        I: IntoIterator<Item = (Option<PathBuf>, Vec<frontend::Primitive>)>,
    {
        let mut builder = Builder {
            primitives: Vec::new(),
            luts: Vec::new(),
            format,
            context,
            generated: HashSet::new(),
        };

        builder.visit_benchmarks(defs)?;

        let lib = ir::LibrarySignatures::from(
            core_lib
                .into_iter()
                .chain(iter::once((None, builder.primitives))),
        );

        let components = builder
            .luts
            .iter()
            .map(|lut| build_component(lut, format, &lib))
            .collect::<CalyxResult<_>>()?;

        Ok(MathLib { components, lib })
    }
}

struct Lut<'a> {
    name: ir::Id,
    function: SollyaFunction,
    domain: &'a CalyxDomain,
    strategy: &'a CalyxImpl,
}

struct Builder<'a> {
    primitives: Vec<frontend::Primitive>,
    luts: Vec<Lut<'a>>,
    format: &'a Format,
    context: &'a ContextResolution<'a>,
    generated: HashSet<ir::Id>,
}

impl Visitor<'_> for Builder<'_> {
    type Error = Error;

    fn visit_expression(&mut self, expr: &ast::Expression) -> CalyxResult<()> {
        match &expr.kind {
            ast::ExprKind::Op(
                op @ ast::Operation {
                    kind: ast::OpKind::Math(f),
                    ..
                },
                _,
            ) => {
                if !matches!(
                    f,
                    ast::MathOp::Add
                        | ast::MathOp::Sub
                        | ast::MathOp::Mul
                        | ast::MathOp::Div
                        | ast::MathOp::Sqrt
                ) {
                    let f: SollyaFunction = (*f).try_into().map_err(|_| {
                        Error::misc(String::from("Unsupported operation"))
                            .with_pos(op)
                    })?;

                    let context = self.context.props[&expr.uid];

                    let domain = context.domain.ok_or_else(|| {
                        Error::misc(String::from("No domain specified"))
                            .with_pos(op)
                    })?;

                    let strategy = context.strategy.ok_or_else(|| {
                        Error::misc(String::from("No implementation specified"))
                            .with_pos(op)
                    })?;

                    self.build_lut(f, domain, strategy)?;
                }

                visitor::visit_expression(self, expr)
            }
            _ => visitor::visit_expression(self, expr),
        }
    }
}

impl<'a> Builder<'a> {
    fn build_lut(
        &mut self,
        function: SollyaFunction,
        domain: &'a CalyxDomain,
        strategy: &'a CalyxImpl,
    ) -> CalyxResult<()> {
        let size = match strategy {
            CalyxImpl::Lut { lut_size } => *lut_size,
            CalyxImpl::Poly { .. } => unimplemented!(),
        };

        let base_name = format!("{function}_lut");

        let name = ir::Id::new(mangling::mangle_name(
            &base_name,
            self.format,
            domain,
            strategy,
        ));

        if self.generated.insert(name) {
            let prim =
                build_primitive(name, function, self.format, domain, size)?;

            let lut = Lut {
                name,
                function,
                domain,
                strategy,
            };

            self.primitives.push(prim);
            self.luts.push(lut);
        }

        Ok(())
    }
}

fn build_primitive(
    name: ir::Id,
    function: SollyaFunction,
    format: &Format,
    domain: &CalyxDomain,
    size: u32,
) -> CalyxResult<frontend::Primitive> {
    let table = remez::build_table(
        function,
        0,
        &domain.left.rational,
        &domain.right.rational,
        size,
        format,
    )?;

    let values: Vec<_> = table
        .iter()
        .map(|row| {
            itertools::process_results(
                row.iter().map(|value| {
                    value.to_format(format).ok_or_else(|| {
                        Error::misc(format!(
                            "Generated constant {value} in implementation of \
                             {function} is not representable in the given \
                             format"
                        ))
                    })
                }),
                |bits| lut::pack(bits, format.width),
            )
        })
        .collect::<CalyxResult<_>>()?;

    Ok(lut::compile_lut(name, &values))
}

fn build_component(
    lut: &Lut,
    format: &Format,
    lib: &ir::LibrarySignatures,
) -> CalyxResult<ir::Component> {
    let lut_size = match lut.strategy {
        CalyxImpl::Lut { lut_size } => *lut_size,
        CalyxImpl::Poly { .. } => unimplemented!(),
    };

    let name = ir::Id::new(mangling::mangle_function(
        lut.function,
        format,
        lut.domain,
        lut.strategy,
    ));

    lookup::compile_lookup(name, lut.name, lut_size, 1, format, lut.domain, lib)
}
