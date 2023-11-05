//! Math library construction.

use std::collections::HashMap;
use std::fmt;

use calyx_frontend as frontend;
use calyx_ir as ir;
use calyx_utils::{self as utils, CalyxResult, Error};

use super::{components::lookup, primitives::lut};
use crate::analysis::{ContextResolution, DomainInference, PassManager};
use crate::format::Format;
use crate::fpcore::ast;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::visitor::{self, Visitor};
use crate::functions::addressing::{AddressSpec, TableDomain};
use crate::functions::remez;
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
            domains: opts
                .infer_domains
                .then(|| pm.get_analysis())
                .transpose()?,
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
    domains: Option<&'a DomainInference>,
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
                args,
            ) => {
                if !matches!(
                    f,
                    ast::MathOp::Add
                        | ast::MathOp::Sub
                        | ast::MathOp::Mul
                        | ast::MathOp::Div
                        | ast::MathOp::Sqrt
                ) {
                    let f = Function {
                        kind: (*f).try_into().map_err(|_| {
                            Error::misc("Unsupported operation").with_pos(op)
                        })?,
                        uid: expr.uid,
                        span: op.span,
                    };

                    let context = self.context.props[&expr.uid];

                    let domain =
                        self.choose_domain(&f, args, context.domain)?;

                    let strategy = context.strategy.ok_or_else(|| {
                        Error::misc("No implementation specified").with_pos(op)
                    })?;

                    self.build_function(&f, &domain, strategy)?;
                }

                visitor::visit_expression(self, expr)
            }
            _ => visitor::visit_expression(self, expr),
        }
    }
}

impl<'a> Builder<'a> {
    fn choose_domain(
        &self,
        function: &Function,
        args: &[ast::Expression],
        hint: Option<&'a CalyxDomain>,
    ) -> CalyxResult<DomainHint<'a>> {
        let (left, right) = hint
            .map(|domain| (&domain.left.value, &domain.right.value))
            .or_else(|| {
                self.domains.map(|domains| {
                    let [left, right] = &domains[args[0].uid];

                    (left, right)
                })
            })
            .ok_or_else(|| {
                Error::misc("No domain specified").with_pos(function)
            })?;

        Ok(DomainHint { left, right })
    }

    fn build_function(
        &mut self,
        function: &Function,
        domain: &DomainHint,
        strategy: &CalyxImpl,
    ) -> CalyxResult<()> {
        let size = match *strategy {
            CalyxImpl::Lut { lut_size } => lut_size,
            CalyxImpl::Poly { .. } => unimplemented!(),
        };

        let (spec, domain) = domain.widen(function, self.format, size)?;

        let name =
            mangle!("lut", function.kind, self.format, domain, strategy).into();

        if let Some(prev) = self.generated.insert(name, function.uid) {
            self.result
                .prototypes
                .insert(function.uid, self.result.prototypes[&prev].clone());
        } else {
            let prim =
                build_primitive(name, function, self.format, &domain, size)?;

            self.lib.add_inline_primitive(prim).set_source();

            let comp_name =
                mangle!("lookup", function.kind, self.format, domain, strategy);

            let (comp, proto) = build_component(
                comp_name.into(),
                name,
                function,
                self.format,
                &spec,
                self.lib,
            );

            self.result.components.push(comp);
            self.result.prototypes.insert(function.uid, proto);
        }

        Ok(())
    }
}

fn build_primitive(
    name: ir::Id,
    function: &Function,
    format: &Format,
    domain: &TableDomain,
    size: u32,
) -> CalyxResult<frontend::Primitive> {
    let table = remez::build_table(
        function.kind,
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
                        .with_pos(function)
                    })
                }),
                |bits| lut::pack(bits, format.width),
            )
        })
        .collect::<CalyxResult<_>>()?;

    Ok(lut::compile_lut(name, &values))
}

fn build_component(
    name: ir::Id,
    lut: ir::Id,
    function: &Function,
    format: &Format,
    spec: &AddressSpec,
    lib: &ir::LibrarySignatures,
) -> (ir::Component, Prototype) {
    let comp = lookup::compile_lookup(name, lut, 1, format, spec, lib);

    let proto = Prototype {
        name,
        prefix_hint: function.as_str().into(),
        signature: lookup::signature(1, format),
        is_comb: true,
    };

    (comp, proto)
}

struct DomainHint<'a> {
    left: &'a ast::Rational,
    right: &'a ast::Rational,
}

impl DomainHint<'_> {
    fn widen(
        &self,
        function: &Function,
        format: &Format,
        size: u32,
    ) -> CalyxResult<(AddressSpec, TableDomain)> {
        let (spec, domain) =
            AddressSpec::from_domain_hint(self.left, self.right, format, size)
                .map_err(|err| {
                    Error::misc(format!(
                        "Invalid domain in implementation of {function}: {err}"
                    ))
                    .with_pos(function)
                })?;

        if &domain.left != self.left || &domain.right != self.right {
            log::info!("Domain widened in implementation of {function}");
        }

        Ok((spec, domain))
    }
}

struct Function {
    kind: SollyaFunction,
    uid: ast::NodeId,
    span: utils::GPosIdx,
}

impl Function {
    fn as_str(&self) -> &str {
        self.kind.as_str()
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl utils::WithPos for Function {
    fn copy_span(&self) -> utils::GPosIdx {
        self.span
    }
}
