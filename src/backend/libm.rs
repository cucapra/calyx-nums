//! Math library construction.

use std::collections::HashMap;
use std::slice;

use calyx_ir as ir;

use super::components::{
    ComponentManager, LookupTable, PiecewisePoly, TableData,
};

use crate::analysis::{Context, NameResolution, PassManager, RangeAnalysis};
use crate::approx::{AddressSpec, Datapath, TableDomain};
use crate::approx::{faithful, remez};
use crate::fpcore::metadata::{CalyxDomain, CalyxImpl};
use crate::fpcore::{Visitor, ast, visitor};
use crate::opts::RangeAnalysis as AnalysisMode;
use crate::utils::sollya::SollyaFunction;
use crate::utils::{Diagnostic, Format, Mangle, Reporter};

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
    ) -> Option<MathLib> {
        let opts = pm.opts();

        let ranges = match opts.range_analysis {
            AnalysisMode::Interval => Some(pm.get_analysis()?),
            AnalysisMode::None => None,
        };

        let mut builder = Builder {
            format: &opts.format,
            bindings: pm.get_analysis()?,
            ranges,
            reporter: &mut pm.rpt(),
            lib,
            cm: ComponentManager::new(),
            prototypes: HashMap::new(),
        };

        builder.visit_definitions(pm.ast()).ok()?;

        Some(MathLib {
            components: builder.cm.into_components(),
            prototypes: builder.prototypes,
        })
    }
}

#[derive(Debug)]
pub struct LibraryError;

struct Builder<'b, 'ast> {
    format: &'b Format,
    bindings: &'b NameResolution<'ast>,
    ranges: Option<&'b RangeAnalysis>,
    reporter: &'b mut Reporter<'ast>,
    lib: &'b mut ir::LibrarySignatures,
    cm: ComponentManager,
    prototypes: HashMap<ast::NodeId, Prototype>,
}

impl Visitor<'_> for Builder<'_, '_> {
    type Error = LibraryError;

    fn visit_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<(), LibraryError> {
        match &expr.kind {
            ast::ExprKind::Op(
                ast::Operation {
                    kind: ast::OpKind::Math(op),
                    span,
                },
                args,
            ) if !op.is_primitive() => {
                let Ok(kind) = SollyaFunction::try_from(*op) else {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("unsupported operation")
                            .with_primary(*span, "unsupported operator"),
                    );

                    return Err(LibraryError);
                };

                let op = Operator { kind, span: *span };
                let context = self.bindings.props[&expr.uid];

                let prototype =
                    self.build(&op, args, &context).map_err(|err| {
                        self.reporter.emit(&err);

                        LibraryError
                    })?;

                self.prototypes.insert(expr.uid, prototype);

                visitor::visit_expression(self, expr)
            }
            _ => visitor::visit_expression(self, expr),
        }
    }
}

impl<'b> Builder<'b, '_> {
    fn build(
        &mut self,
        op: &Operator,
        args: &[ast::Expression],
        context: &Context,
    ) -> Result<Prototype, Diagnostic> {
        let domain = self.choose_domain(op, args, context.domain)?;

        match context.strategy {
            Some(CalyxImpl::Lut { size }) => self.build_lut(op, &domain, *size),
            Some(CalyxImpl::Poly { degree }) => {
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
        args: &[ast::Expression],
        hint: Option<&'b CalyxDomain>,
    ) -> Result<DomainHint<'b>, Diagnostic> {
        let (left, right) = hint
            .map(|domain| (&domain.left.value, &domain.right.value))
            .or_else(|| {
                self.ranges.map(|ranges| {
                    let [left, right] = &ranges[args[0].uid];

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

        let data = TableData {
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

        let builder = LookupTable {
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

        let data = TableData {
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

        let builder = PiecewisePoly {
            table: LookupTable {
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
    left: &'a ast::Rational,
    right: &'a ast::Rational,
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
    span: ast::Span,
}

impl ast::MathOp {
    fn is_primitive(self) -> bool {
        matches!(
            self,
            ast::MathOp::Add
                | ast::MathOp::Sub
                | ast::MathOp::Mul
                | ast::MathOp::Div
                | ast::MathOp::Neg
                | ast::MathOp::Sqrt
        )
    }
}
