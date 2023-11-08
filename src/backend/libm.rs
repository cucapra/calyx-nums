//! Math library construction.

use std::collections::HashMap;
use std::fmt;

use calyx_ir as ir;
use calyx_utils::{self as utils, CalyxResult, Error};

use super::components::{ComponentManager, LookupTable, PiecewisePoly};
use crate::analysis::{ContextResolution, DomainInference, PassManager};
use crate::format::Format;
use crate::fpcore::ast;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::visitor::{self, Visitor};
use crate::functions::addressing::{AddressSpec, TableDomain};
use crate::utils::sollya::SollyaFunction;

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
            cm: ComponentManager::new(),
            prototypes: HashMap::new(),
            format: &opts.format,
            context: pm.get_analysis()?,
            domains: opts
                .infer_domains
                .then(|| pm.get_analysis())
                .transpose()?,
            lib,
        };

        builder.visit_benchmarks(pm.ast())?;

        Ok(MathLib {
            components: builder.cm.into_components(),
            prototypes: builder.prototypes,
        })
    }
}

struct Builder<'a> {
    cm: ComponentManager,
    prototypes: HashMap<ast::NodeId, Prototype>,
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
        let (degree, size) = match *strategy {
            CalyxImpl::Lut { lut_size } => (0, lut_size),
            CalyxImpl::Poly { degree, lut_size } => (degree, lut_size),
        };

        let (spec, domain) = &domain.widen(function, self.format, size)?;

        let table = LookupTable {
            function,
            format: self.format,
            spec,
            domain,
            degree,
            size,
        };

        let prefix_hint = ir::Id::new(function);

        let prototype = match strategy {
            CalyxImpl::Lut { .. } => {
                let (name, signature) = self.cm.get(&table, self.lib)?;

                Prototype {
                    name,
                    prefix_hint,
                    signature,
                    is_comb: true,
                }
            }
            CalyxImpl::Poly { .. } => {
                let (name, signature) =
                    self.cm.get(&PiecewisePoly(table), self.lib)?;

                Prototype {
                    name,
                    prefix_hint,
                    signature,
                    is_comb: false,
                }
            }
        };

        self.prototypes.insert(function.uid, prototype);

        Ok(())
    }
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

pub struct Function {
    kind: SollyaFunction,
    uid: ast::NodeId,
    span: utils::GPosIdx,
}

impl Function {
    pub fn kind(&self) -> SollyaFunction {
        self.kind
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
