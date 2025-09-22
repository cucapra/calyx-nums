use std::collections::HashMap;
use std::slice;

use calyx_ir as ir;

use super::components::{self as comp, ComponentManager};
use crate::analysis::RangeAnalysis;
use crate::approx::{AddressSpec, Datapath, TableDomain, faithful, remez};
use crate::hir::{self, Metadata, Pool, Visitor};
use crate::opts::{Opts, RangeAnalysis as AnalysisMode};
use crate::utils::sollya::SollyaFunction;
use crate::utils::{Diagnostic, Format, Mangle, Reporter};

pub struct Prototype {
    pub name: ir::Id,
    pub prefix_hint: ir::Id,
    pub signature: Vec<ir::PortDef<u64>>,
    pub is_comb: bool,
}

pub fn compile_math_library(
    ctx: &hir::Context,
    opts: &Opts,
    reporter: &mut Reporter,
    cm: &mut ComponentManager,
    lib: &mut ir::LibrarySignatures,
) -> Option<HashMap<hir::ExprIdx, Prototype>> {
    let ranges = match opts.range_analysis {
        AnalysisMode::Interval => {
            Some(RangeAnalysis::new(ctx, opts, reporter)?)
        }
        AnalysisMode::None => None,
    };

    let mut builder = Builder {
        ctx,
        ranges: ranges.as_ref(),
        format: &opts.format,
        reporter,
        cm,
        lib,
        prototypes: HashMap::new(),
    };

    builder.visit_definitions(ctx).ok()?;

    Some(builder.prototypes)
}

#[derive(Debug)]
pub struct LibraryError;

struct Builder<'a, 'src> {
    ctx: &'a hir::Context,
    ranges: Option<&'a RangeAnalysis>,
    format: &'a Format,

    reporter: &'a mut Reporter<'src>,
    cm: &'a mut ComponentManager,
    lib: &'a mut ir::LibrarySignatures,

    prototypes: HashMap<hir::ExprIdx, Prototype>,
}

impl Visitor for Builder<'_, '_> {
    type Error = LibraryError;

    fn visit_operation(
        &mut self,
        idx: hir::ExprIdx,
        op: &hir::Operation,
        args: hir::EntityList<hir::ExprIdx>,
        ctx: &hir::Context,
    ) -> Result<(), LibraryError> {
        if let hir::Operation {
            kind: hir::OpKind::Math(op),
            span,
        } = op
            && !op.is_primitive()
        {
            let Ok(kind) = SollyaFunction::try_from(*op) else {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported operation")
                        .with_primary(*span, "unsupported operator"),
                );

                return Err(LibraryError);
            };

            let op = Operator { kind, span: *span };

            let prototype =
                self.build(&self.ctx[idx], &op, args).map_err(|err| {
                    self.reporter.emit(&err);

                    LibraryError
                })?;

            self.prototypes.insert(idx, prototype);
        }

        hir::visitor::visit_operation(self, idx, op, args, ctx)
    }
}

impl<'a> Builder<'a, '_> {
    fn build(
        &mut self,
        expr: &hir::Expression,
        op: &Operator,
        args: hir::EntityList<hir::ExprIdx>,
    ) -> Result<Prototype, Diagnostic> {
        let domain = expr.props(self.ctx).find_map(|prop| match prop {
            hir::Property::Domain(domain) => Some(domain),
            _ => None,
        });

        let strategy = expr.props(self.ctx).find_map(|prop| match prop {
            hir::Property::Impl(strategy) => Some(strategy),
            _ => None,
        });

        let domain = self.choose_domain(op, args, domain)?;

        match strategy {
            Some(hir::Strategy::Lut { size }) => {
                self.build_lut(op, &domain, *size)
            }
            Some(hir::Strategy::Poly { degree }) => {
                self.build_poly(op, &domain, *degree)
            }
            None => Err(Diagnostic::error()
                .with_message("operator with unspecified implementation")
                .with_primary(op.span, "no implementation specified")
                .with_note("help: add a `:calyx-impl` annotation")),
        }
    }

    fn choose_domain(
        &self,
        op: &Operator,
        args: hir::EntityList<hir::ExprIdx>,
        hint: Option<&hir::Domain>,
    ) -> Result<DomainHint<'a>, Diagnostic> {
        let (left, right) = hint
            .map(|domain| {
                (&self.ctx[domain.left].value, &self.ctx[domain.right].value)
            })
            .or_else(|| {
                self.ranges.map(|ranges| {
                    let [left, right] =
                        &ranges[args.first(self.ctx.pool()).unwrap()];

                    (left, right)
                })
            })
            .ok_or_else(|| {
                Diagnostic::error()
                    .with_message("operator with unknown domain")
                    .with_primary(op.span, "unknown domain")
                    .with_note("help: add a `:calyx-domain` annotation or enable range analysis")
            })?;

        Ok(DomainHint { left, right })
    }

    fn build_lut(
        &mut self,
        op: &Operator,
        domain: &DomainHint,
        size: u32,
    ) -> Result<Prototype, Diagnostic> {
        let (spec, domain) = &domain.widen(op, self.format, size)?;

        let degree = 0;
        let scale = self.format.scale;

        let values = &remez::build_table(op.kind, degree, domain, size, scale)
            .map_err(|err| Diagnostic::from_sollya_and_span(err, op.span))?;

        let data = comp::TableData {
            values,
            formats: slice::from_ref(self.format),
            spec: &TableSpec {
                function: op.kind,
                degree,
                domain,
                size,
                scale,
            },
        };

        let builder = comp::LookupTable {
            data,
            format: self.format,
            spec,
            span: op.span,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: ir::Id::new(op.kind),
            signature,
            is_comb: true,
        })
    }

    fn build_poly(
        &mut self,
        op: &Operator,
        domain: &DomainHint,
        degree: u32,
    ) -> Result<Prototype, Diagnostic> {
        let scale = self.format.scale;
        let DomainHint { left, right } = domain;

        let size =
            faithful::segment_domain(op.kind, degree, left, right, scale)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(err, op.span)
                })?;

        let (spec, domain) = &domain.widen(op, self.format, size)?;

        let approx =
            faithful::build_table(op.kind, degree, domain, size, scale)
                .map_err(|err| {
                    Diagnostic::from_sollya_and_span(err, op.span)
                })?;

        let datapath = Datapath::from_approx(&approx, degree, scale);
        let formats = &datapath.lut_formats();

        let data = comp::TableData {
            values: &approx.table,
            formats,
            spec: &TableSpec {
                function: op.kind,
                degree,
                domain,
                size,
                scale: datapath.lut_scale,
            },
        };

        let builder = comp::PiecewisePoly {
            table: comp::LookupTable {
                data,
                format: self.format,
                spec,
                span: op.span,
            },
            spec: datapath,
        };

        let (name, signature) = self.cm.get(&builder, self.lib)?;

        Ok(Prototype {
            name,
            prefix_hint: ir::Id::new(op.kind),
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
    left: &'a hir::Rational,
    right: &'a hir::Rational,
}

impl DomainHint<'_> {
    fn widen(
        &self,
        op: &Operator,
        format: &Format,
        size: u32,
    ) -> Result<(AddressSpec, TableDomain), Diagnostic> {
        AddressSpec::from_domain_hint(self.left, self.right, format, size)
            .map_err(|err| {
                Diagnostic::error()
                    .with_message("operator with infeasible domain")
                    .with_primary(op.span, "operator has infeasible domain")
                    .with_note(err.to_string())
            })
    }
}

struct Operator {
    kind: SollyaFunction,
    span: hir::Span,
}

impl hir::MathOp {
    fn is_primitive(self) -> bool {
        matches!(
            self,
            hir::MathOp::Add
                | hir::MathOp::Sub
                | hir::MathOp::Mul
                | hir::MathOp::Div
                | hir::MathOp::Neg
                | hir::MathOp::FAbs
                | hir::MathOp::Sqrt
                | hir::MathOp::FMax
                | hir::MathOp::FMin
        )
    }
}
