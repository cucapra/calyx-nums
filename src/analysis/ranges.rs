use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops::Index;

use calyx_utils::{CalyxResult, Error};
use itertools::Itertools;

use super::bindings::{Binding, NameResolution};
use super::passes::{Pass, PassManager};
use super::type_check::TypeCheck;
use crate::format::Format;
use crate::fpcore::{ast, Visitor};
use crate::utils::sollya::{self, SollyaFunction};

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
    fn run(pm: &PassManager) -> CalyxResult<Self> {
        pm.get_analysis::<TypeCheck>()?;

        let mut builder = Builder {
            bindings: pm.get_analysis()?,
            script: String::from(PROLOGUE),
        };

        builder.visit_definitions(pm.ast())?;
        builder.script.push_str(EPILOGUE);

        Ok(RangeAnalysis {
            ranges: run_script(&builder.script, &pm.opts().format)?,
        })
    }
}

fn run_script(
    script: &str,
    format: &Format,
) -> CalyxResult<HashMap<ast::NodeId, [ast::Rational; 2]>> {
    let cmd = script.as_bytes();
    let arg = [format.scale.to_string()];

    let result = sollya::sollya(cmd, &arg)
        .map_err(|err| Error::misc(format!("Sollya error: {err}")))?;

    result
        .lines()
        .map(|line| {
            parse_response_line(line).ok_or_else(|| {
                Error::misc(format!(
                    "Sollya error: failed to parse line `{line}`",
                ))
            })
        })
        .collect()
}

fn parse_response_line(
    line: &str,
) -> Option<(ast::NodeId, [ast::Rational; 2])> {
    let (uid, left, right) = line.split(' ').collect_tuple()?;

    let uid = ast::NodeId(uid.parse().ok()?);
    let left = ast::Rational::from_dyadic(left).ok()?;
    let right = ast::Rational::from_dyadic(right).ok()?;

    Some((uid, [left, right]))
}

impl Index<ast::NodeId> for RangeAnalysis {
    type Output = [ast::Rational; 2];

    fn index(&self, index: ast::NodeId) -> &Self::Output {
        &self.ranges[&index]
    }
}

struct Builder<'ast> {
    bindings: &'ast NameResolution<'ast>,
    script: String,
}

impl Builder<'_> {
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
        .unwrap()
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
            var.0 .0, var
        )
        .unwrap();
    }
}

impl<'ast> Visitor<'ast> for Builder<'ast> {
    type Error = Error;

    fn visit_definition(&mut self, def: &'ast ast::FPCore) -> CalyxResult<()> {
        for arg in &def.args {
            let (left, right) = arg
                .props
                .iter()
                .find_map(|prop| match prop {
                    ast::Property::CalyxDomain(domain) => {
                        Some((&domain.left.value, &domain.right.value))
                    }
                    _ => None,
                })
                .ok_or_else(|| {
                    Error::misc("No domain specified").with_pos(&arg.var)
                })?;

            self.script_interval(arg, left, right);
        }

        self.visit_expression(&def.body)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> CalyxResult<()> {
        match &expr.kind {
            ast::ExprKind::Num(num) => {
                self.script_point(expr, &num.value);
            }
            ast::ExprKind::Id(_) => {
                let binding = match self.bindings.names[&expr.uid] {
                    Binding::Argument(arg) => SollyaVar::from(arg),
                    Binding::Let(binding) => SollyaVar::from(&binding.expr),
                    _ => unimplemented!(),
                };

                self.script_assign(expr, binding);
            }
            ast::ExprKind::Op(
                op @ ast::Operation {
                    kind: ast::OpKind::Math(function),
                    ..
                },
                args,
            ) => {
                for arg in args {
                    self.visit_expression(arg)?;
                }

                let (function, is_binary) = match function {
                    ast::MathOp::Add => ("+", true),
                    ast::MathOp::Sub => ("-", true),
                    ast::MathOp::Mul => ("*", true),
                    ast::MathOp::Div => ("/", true),
                    ast::MathOp::Neg => ("-", false),
                    _ => (
                        SollyaFunction::try_from(*function)
                            .map_err(|_| {
                                Error::misc("Unsupported operation")
                                    .with_pos(op)
                            })?
                            .as_str(),
                        false,
                    ),
                };

                if is_binary {
                    self.script_binary(expr, &args[0], function, &args[1]);
                } else {
                    self.script_unary(expr, function, &args[0]);
                }
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
            _ => unimplemented!(),
        };

        self.script_print(SollyaVar::from(expr));

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
        write!(f, "n{:x}", self.0 .0)
    }
}
