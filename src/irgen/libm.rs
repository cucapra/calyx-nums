//! Math library construction.

use std::collections::HashMap;

use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use crate::analysis::{ContextResolution, PassManager};
use crate::format::Format;
use crate::fpcore::ast;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::visitor::{self, Visitor};
use crate::functions::lookup::{self, TableDomain};
use crate::functions::{lut, remez};
use crate::utils::mangling::mangle;
use crate::utils::sollya::SollyaFunction;

#[derive(Clone)]
pub struct Prototype {
    pub name: ir::Id,
    pub prefix_hint: ir::Id,
    pub signature: Vec<ir::PortDef<u64>>,
    pub is_comb: bool,
}

pub struct MathLib {
    pub components: Vec<ir::Component>,
    pub prototypes: HashMap<ast::NodeId, Prototype>,
}

impl MathLib {
    pub fn new(
        pm: &PassManager,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<MathLib> {
        let opts = pm.opts();

        let mut builder = Builder {
            result: MathLib {
                components: Vec::new(),
                prototypes: HashMap::new(),
            },
            generated: HashMap::new(),
            format: &opts.format,
            context: pm.get_analysis()?,
            lib,
        };

        builder.visit_benchmarks(pm.ast())?;

        Ok(builder.result)
    }
}

struct Builder<'a> {
    result: MathLib,
    generated: HashMap<ir::Id, ast::NodeId>,
    format: &'a Format,
    context: &'a ContextResolution<'a>,
    lib: &'a mut ir::LibrarySignatures,
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

                    let domain = self.widen_domain(f, domain, strategy);

                    self.build_function(f, expr.uid, &domain, strategy)?;
                }

                visitor::visit_expression(self, expr)
            }
            _ => visitor::visit_expression(self, expr),
        }
    }
}

impl Builder<'_> {
    fn widen_domain(
        &self,
        function: SollyaFunction,
        domain: &CalyxDomain,
        strategy: &CalyxImpl,
    ) -> TableDomain {
        let left = &domain.left.value;
        let right = &domain.right.value;

        let widened =
            TableDomain::from_hint(left, right, strategy, self.format);

        if left != &widened.left || right != &widened.right {
            log::info!("Domain widened in implementation of {function}");
        }

        widened
    }

    fn build_function(
        &mut self,
        function: SollyaFunction,
        uid: ast::NodeId,
        domain: &TableDomain,
        strategy: &CalyxImpl,
    ) -> CalyxResult<()> {
        let size = match strategy {
            CalyxImpl::Lut { lut_size } => *lut_size,
            CalyxImpl::Poly { .. } => unimplemented!(),
        };

        let base_name = format!("{function}_lut");

        let name =
            ir::Id::new(mangle!(base_name, self.format, domain, strategy));

        match self.generated.insert(name, uid) {
            Some(prev) => {
                self.result
                    .prototypes
                    .insert(uid, self.result.prototypes[&prev].clone());
            }
            None => {
                let prim =
                    build_primitive(name, function, self.format, domain, size)?;

                self.lib.add_inline_primitive(prim).set_source();

                let (comp, proto) = build_component(
                    name,
                    function,
                    self.format,
                    domain,
                    strategy,
                    self.lib,
                )?;

                self.result.components.push(comp);
                self.result.prototypes.insert(uid, proto);
            }
        };

        Ok(())
    }
}

fn build_primitive(
    name: ir::Id,
    function: SollyaFunction,
    format: &Format,
    domain: &TableDomain,
    size: u32,
) -> CalyxResult<frontend::Primitive> {
    let table = remez::build_table(
        function,
        0,
        &domain.left,
        &domain.right,
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
    lut: ir::Id,
    function: SollyaFunction,
    format: &Format,
    domain: &TableDomain,
    strategy: &CalyxImpl,
    lib: &ir::LibrarySignatures,
) -> CalyxResult<(ir::Component, Prototype)> {
    let lut_size = match strategy {
        CalyxImpl::Lut { lut_size } => *lut_size,
        CalyxImpl::Poly { .. } => unimplemented!(),
    };

    let name =
        ir::Id::new(mangle!(function.as_str(), format, domain, strategy));

    let comp =
        lookup::compile_lookup(name, lut, lut_size, 1, format, domain, lib)
            .map_err(|err| {
                Error::misc(format!(
                    "Invalid domain in implementation of {function}: {err}"
                ))
            })?;

    let proto = Prototype {
        name,
        prefix_hint: ir::Id::new(function.as_str()),
        signature: lookup::signature(1, format),
        is_comb: true,
    };

    Ok((comp, proto))
}
