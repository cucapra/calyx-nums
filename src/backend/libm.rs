//! Math library construction.

use std::collections::HashMap;
use std::{fmt, slice};

use calyx_ir as ir;
use calyx_utils::{self as utils, CalyxResult, Error};

use super::components::{
    ComponentManager, LookupTable, PiecewisePoly, TableData,
};

use crate::analysis::{NameResolution, PassManager, RangeAnalysis};
use crate::format::Format;
use crate::fpcore::ast;
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::visitor::{self, Visitor};
use crate::functions::{faithful, remez};
use crate::functions::{AddressSpec, Datapath, TableDomain};
use crate::utils::mangling::Mangle;
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
            bindings: pm.get_analysis()?,
            ranges: opts
                .infer_domains
                .then(|| pm.get_analysis())
                .transpose()?,
            lib,
        };

        builder.visit_definitions(pm.ast())?;

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
    bindings: &'a NameResolution<'a>,
    ranges: Option<&'a RangeAnalysis>,
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
                        | ast::MathOp::Neg
                        | ast::MathOp::Sqrt
                ) {
                    let f = Function {
                        kind: (*f).try_into().map_err(|_| {
                            Error::misc("Unsupported operation").with_pos(op)
                        })?,
                        uid: expr.uid,
                        span: op.span,
                    };

                    let context = self.bindings.props[&expr.uid];

                    let domain =
                        self.choose_domain(&f, args, context.domain)?;

                    let strategy = context.strategy.ok_or_else(|| {
                        Error::misc("No implementation specified").with_pos(op)
                    })?;

                    let prototype = match *strategy {
                        CalyxImpl::Lut { size } => {
                            self.build_lut(&f, &domain, size)?
                        }
                        CalyxImpl::Poly { degree } => {
                            self.build_poly(&f, &domain, degree)?
                        }
                    };

                    self.prototypes.insert(f.uid, prototype);
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
                self.ranges.map(|ranges| {
                    let [left, right] = &ranges[args[0].uid];

                    (left, right)
                })
            })
            .ok_or_else(|| {
                Error::misc("No domain specified").with_pos(function)
            })?;

        Ok(DomainHint { left, right })
    }

    fn build_lut(
        &mut self,
        function: &Function,
        domain: &DomainHint,
        size: u32,
    ) -> CalyxResult<Prototype> {
        let (spec, domain) = &domain.widen(function, self.format, size)?;

        let degree = 0;
        let scale = self.format.scale;

        let values =
            &remez::build_table(function.kind, degree, domain, size, scale)?;

        let data = TableData {
            values,
            formats: slice::from_ref(self.format),
            spec: &TableSpec {
                function: function.kind,
                degree,
                domain,
                size,
                scale,
            },
        };

        let builder = LookupTable {
            data,
            format: self.format,
            spec,
            span: function.span,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: ir::Id::new(function),
            signature,
            is_comb: true,
        })
    }

    fn build_poly(
        &mut self,
        function: &Function,
        domain: &DomainHint,
        degree: u32,
    ) -> CalyxResult<Prototype> {
        let scale = self.format.scale;

        let size = faithful::segment_domain(
            function.kind,
            degree,
            domain.left,
            domain.right,
            scale,
        )?;

        let (spec, domain) = &domain.widen(function, self.format, size)?;

        let approx =
            faithful::build_table(function.kind, degree, domain, size, scale)?;

        let datapath = Datapath::from_approx(&approx, degree, scale);
        let formats = &datapath.lut_formats();

        let data = TableData {
            values: &approx.table,
            formats,
            spec: &TableSpec {
                function: function.kind,
                degree,
                domain,
                size,
                scale: datapath.lut_scale,
            },
        };

        let builder = PiecewisePoly {
            table: LookupTable {
                data,
                format: self.format,
                spec,
                span: function.span,
            },
            spec: datapath,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: ir::Id::new(function),
            signature,
            is_comb: false,
        })
    }
}

#[derive(Mangle)]
struct TableSpec<'a> {
    function: SollyaFunction,
    degree: u32,
    domain: &'a TableDomain,
    size: u32,
    scale: i32,
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
                    Error::misc(format!("Invalid domain: {err}"))
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
    span: ast::Span,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl utils::WithPos for Function {
    fn copy_span(&self) -> utils::GPosIdx {
        self.span.copy_span()
    }
}
