use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops::Index;

use itertools::Itertools;

use super::passes::{Pass, PassManager};
use super::{Binding, CallGraph, NameResolution, Precondition, TypeCheck};
use crate::fpcore::{Visitor, ast};
use crate::utils::rational::Dyadic;
use crate::utils::sollya::{self, ScriptError, SollyaFunction};
use crate::utils::{Diagnostic, Format, Reporter};

const PROLOGUE: &str = "\
dieonerrormode = on!;
display = dyadic!;

s = parse(__argv[0]);

procedure rnd(x) {
    return [
        floor(inf(x) / 2^s) * 2^s;
        ceil(sup(x) / 2^s) * 2^s
    ];
};
";

const EPILOGUE: &str = "quit;\n";

pub struct RangeAnalysis {
    ranges: HashMap<ast::NodeId, [ast::Rational; 2]>,
}

impl Pass<'_> for RangeAnalysis {
    fn run(pm: &PassManager) -> Option<Self> {
        let _: &TypeCheck = pm.get_analysis()?;
        let call_graph: &CallGraph = pm.get_analysis()?;

        let mut builder = Builder {
            bindings: pm.get_analysis()?,
            reporter: &mut pm.rpt(),
            script: String::from(PROLOGUE),
        };

        builder
            .visit_definitions(call_graph.linearized.iter().copied())
            .ok()?;

        builder.script.push_str(EPILOGUE);

        let ranges = run_script(&builder.script, &pm.opts().format)
            .map_err(|err| {
                builder.reporter.emit(
                    &Diagnostic::from_sollya(err)
                        .with_note("range analysis failed"),
                );
            })
            .ok()?;

        Some(RangeAnalysis { ranges })
    }
}

fn run_script(
    script: &str,
    format: &Format,
) -> Result<HashMap<ast::NodeId, [ast::Rational; 2]>, ScriptError> {
    let cmd = script.as_bytes();
    let arg = [format.scale.to_string()];

    let result = sollya::sollya(cmd, &arg)?;

    result
        .lines()
        .map(|line| parse_response_line(line).ok_or(ScriptError::BadResponse))
        .collect()
}

fn parse_response_line(
    line: &str,
) -> Option<(ast::NodeId, [ast::Rational; 2])> {
    let (uid, left, right) = line.split(' ').collect_tuple()?;

    let uid = ast::NodeId(uid.parse().ok()?);
    let left = ast::Rational::from_dyadic(left)?;
    let right = ast::Rational::from_dyadic(right)?;

    Some((uid, [left, right]))
}

impl Index<ast::NodeId> for RangeAnalysis {
    type Output = [ast::Rational; 2];

    fn index(&self, index: ast::NodeId) -> &Self::Output {
        &self.ranges[&index]
    }
}

#[derive(Debug)]
struct RangeAnalysisError;

struct Builder<'p, 'ast> {
    bindings: &'p NameResolution<'ast>,
    reporter: &'p mut Reporter<'ast>,
    script: String,
}

impl Builder<'_, '_> {
    fn script_bool(&mut self, dst: SollyaVar) {
        writeln!(self.script, "{dst} = [0;1];").unwrap();
    }

    fn script_point<D, N>(&mut self, dst: D, num: &N)
    where
        SollyaVar: From<D>,
        N: fmt::Display,
    {
        writeln!(self.script, "{} = rnd([{num}]);", SollyaVar::from(dst))
            .unwrap();
    }

    fn script_interval<D, N>(&mut self, dst: D, left: &N, right: &N)
    where
        SollyaVar: From<D>,
        N: fmt::Display,
    {
        writeln!(
            self.script,
            "{} = rnd([{left};{right}]);",
            SollyaVar::from(dst)
        )
        .unwrap();
    }

    fn script_unary<D, S>(&mut self, dst: D, function: &str, src: S)
    where
        SollyaVar: From<D> + From<S>,
    {
        writeln!(
            self.script,
            "{} = rnd({function}({}));",
            SollyaVar::from(dst),
            SollyaVar::from(src)
        )
        .unwrap();
    }

    fn script_binary<D, L, R>(&mut self, dst: D, left: L, op: &str, right: R)
    where
        SollyaVar: From<D> + From<L> + From<R>,
    {
        writeln!(
            self.script,
            "{} = rnd({} {op} {});",
            SollyaVar::from(dst),
            SollyaVar::from(left),
            SollyaVar::from(right)
        )
        .unwrap();
    }

    fn script_union<D, L, R>(&mut self, dst: D, left: L, right: R)
    where
        SollyaVar: From<D> + From<L> + From<R>,
    {
        writeln!(
            self.script,
            "{0} = [min(inf({1}), inf({2})); max(sup({1}), sup({2}))];",
            SollyaVar::from(dst),
            SollyaVar::from(left),
            SollyaVar::from(right)
        )
        .unwrap();
    }

    fn script_assign<D, S>(&mut self, dst: D, src: S)
    where
        SollyaVar: From<D> + From<S>,
    {
        writeln!(
            self.script,
            "{} = {};",
            SollyaVar::from(dst),
            SollyaVar::from(src)
        )
        .unwrap();
    }

    fn script_print(&mut self, var: SollyaVar) {
        writeln!(
            self.script,
            "print(\"{0}\", inf({1}), sup({1}));",
            var.0.0, var
        )
        .unwrap();
    }
}

impl Visitor<'_> for Builder<'_, '_> {
    type Error = RangeAnalysisError;

    fn visit_definition(
        &mut self,
        def: &ast::FPCore,
    ) -> Result<(), RangeAnalysisError> {
        let mut pre = Precondition::new();

        for prop in &def.props {
            if let ast::PropKind::Pre(expr) = &prop.kind {
                pre.add_constraint(expr, self.bindings, self.reporter)
                    .map_err(|_| RangeAnalysisError)?;
            }
        }

        for arg in &def.args {
            let (left, right) = pre
                .domains
                .get(&arg.uid)
                .and_then(|domain| domain.bounds())
                .ok_or_else(|| {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("precondition is too weak")
                            .with_primary(
                                arg.var.span,
                                "argument has unbounded domain",
                            ),
                    );

                    RangeAnalysisError
                })?;

            self.script_interval(arg, left, right);
            self.script_print(arg.into());
        }

        self.visit_expression(&def.body)
    }

    fn visit_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<(), RangeAnalysisError> {
        match &expr.kind {
            ast::ExprKind::Num(num) => {
                self.script_point(expr, &num.value);
            }
            ast::ExprKind::Const(ast::Constant::Bool(_)) => {
                self.script_bool(SollyaVar::from(expr));
            }
            ast::ExprKind::Id(_) => {
                let binding = match self.bindings.names[&expr.uid] {
                    Binding::Argument(arg) => SollyaVar::from(arg),
                    Binding::Let(binding) => SollyaVar::from(&binding.expr),
                    _ => unreachable!("pass rejects loops"),
                };

                self.script_assign(expr, binding);
            }
            ast::ExprKind::Op(
                ast::Operation {
                    kind: ast::OpKind::Math(op),
                    span,
                },
                args,
            ) => {
                for arg in args {
                    self.visit_expression(arg)?;
                }

                let (op, is_binary) = match op {
                    ast::MathOp::Add => ("+", true),
                    ast::MathOp::Sub => ("-", true),
                    ast::MathOp::Mul => ("*", true),
                    ast::MathOp::Div => ("/", true),
                    ast::MathOp::Neg => ("-", false),
                    ast::MathOp::FAbs => ("abs", false),
                    _ => (
                        SollyaFunction::try_from(*op)
                            .map_err(|_| {
                                self.reporter.emit(
                                    &Diagnostic::error()
                                        .with_message("unsupported operation")
                                        .with_primary(
                                            *span,
                                            "unsupported operator",
                                        ),
                                );

                                RangeAnalysisError
                            })?
                            .as_str(),
                        false,
                    ),
                };

                if is_binary {
                    self.script_binary(expr, &args[0], op, &args[1]);
                } else {
                    self.script_unary(expr, op, &args[0]);
                }
            }
            ast::ExprKind::Op(
                ast::Operation {
                    kind: ast::OpKind::Test(_),
                    ..
                },
                args,
            ) => {
                for arg in args {
                    self.visit_expression(arg)?;
                }

                self.script_bool(SollyaVar::from(expr));
            }
            ast::ExprKind::Op(
                ast::Operation {
                    kind: ast::OpKind::FPCore(id),
                    ..
                },
                args,
            ) => {
                for arg in args {
                    self.visit_expression(arg)?;
                }

                self.script_assign(expr, &self.bindings.defs[id].body);
            }
            ast::ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => {
                self.visit_expression(cond)?;
                self.visit_expression(true_branch)?;
                self.visit_expression(false_branch)?;

                self.script_union(
                    expr,
                    true_branch.as_ref(),
                    false_branch.as_ref(),
                );
            }
            ast::ExprKind::Let { bindings, body, .. } => {
                for binding in bindings {
                    self.visit_binding(binding)?;
                }

                self.visit_expression(body)?;
                self.script_assign(expr, body.as_ref());
            }
            ast::ExprKind::Annotation { body, .. } => {
                self.visit_expression(body)?;
                self.script_assign(expr, body.as_ref());
            }
            _ => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported expression")
                        .with_primary(expr.span, ""),
                );

                return Err(RangeAnalysisError);
            }
        };

        self.script_print(expr.into());

        Ok(())
    }
}

#[derive(Clone, Copy)]
struct SollyaVar(ast::NodeId);

impl From<&ast::Argument> for SollyaVar {
    fn from(value: &ast::Argument) -> Self {
        SollyaVar(value.uid)
    }
}

impl From<&ast::Expression> for SollyaVar {
    fn from(value: &ast::Expression) -> Self {
        SollyaVar(value.uid)
    }
}

impl fmt::Display for SollyaVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "n{:x}", self.0.0)
    }
}
