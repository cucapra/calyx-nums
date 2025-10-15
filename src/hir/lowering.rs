use std::collections::HashMap;

use crate::fpcore::ast;
use crate::hir::{self, EntityList, PackedOption, Pool};
use crate::opts::Opts;
use crate::sem::{self, PassManager};
use crate::utils::{Diagnostic, Reporter};

pub fn lower_ast(
    defs: Vec<ast::FPCore>,
    opts: &Opts,
    reporter: &mut Reporter,
) -> Option<hir::Context> {
    let pm = PassManager::new(opts, &defs, reporter);

    let _types: &sem::TypeCheck = pm.get_analysis()?;
    let bindings: &sem::NameResolution = pm.get_analysis()?;
    let call_graph: &sem::CallGraph = pm.get_analysis()?;

    let mut ctx = hir::Context::new();

    let mut builder = Builder {
        bindings,
        reporter: &mut pm.rpt(),
        ctx: &mut ctx,
        parent: Default::default(),
        defs: HashMap::with_capacity(bindings.defs.len()),
        vars: HashMap::new(),
    };

    builder.lower_definitions(&call_graph.linearized).ok()?;

    Some(ctx)
}

#[derive(Debug)]
struct LoweringError;

struct Builder<'a, 'ast, 'src> {
    bindings: &'a sem::NameResolution<'ast>,
    reporter: &'a mut Reporter<'src>,
    ctx: &'a mut hir::Context,
    parent: PackedOption<hir::ScopeIdx>,
    defs: HashMap<ast::Id, hir::DefIdx>,
    vars: HashMap<ast::NodeId, (hir::VarIdx, hir::VarKind)>,
}

impl<'ast> Builder<'_, 'ast, '_> {
    fn lower_definition(
        &mut self,
        def: &'ast ast::FPCore,
    ) -> Result<hir::DefIdx, LoweringError> {
        self.parent = Default::default();

        let args = self.lower_arguments(&def.args)?;
        let scope = self.lower_properties(&def.props)?;

        self.parent = scope;

        let lowered = hir::Definition {
            name: def.name.clone(),
            args,
            scope,
            body: self.lower_expression(&def.body)?,
        };

        let idx = self.ctx.defs.push(lowered);

        if let Some(symbol) = &def.name {
            self.defs.insert(symbol.id, idx);
        }

        Ok(idx)
    }

    fn lower_definitions(
        &mut self,
        defs: &[&'ast ast::FPCore],
    ) -> Result<(), LoweringError> {
        self.ctx.defs.reserve_exact(defs.len());

        for def in defs {
            self.lower_definition(def)?;
        }

        Ok(())
    }

    fn lower_argument(
        &mut self,
        arg: &'ast ast::Argument,
    ) -> Result<hir::ArgIdx, LoweringError> {
        let lowered = hir::Argument {
            var: arg.var.clone(),
            scope: self.lower_properties(&arg.props)?,
        };

        let var = self.ctx.vars.push(());
        let idx = self.ctx.args.push(lowered);

        self.vars.insert(arg.uid, (var, hir::VarKind::Arg(idx)));

        Ok(idx)
    }

    fn lower_arguments(
        &mut self,
        args: &'ast [ast::Argument],
    ) -> Result<hir::IndexRange<hir::ArgIdx>, LoweringError> {
        self.ctx.args.reserve(args.len());

        let start = self.ctx.args.next_key();

        for arg in args {
            self.lower_argument(arg)?;
        }

        let end = self.ctx.args.next_key();

        Ok(hir::IndexRange { start, end })
    }

    fn lower_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<hir::ExprIdx, LoweringError> {
        let kind = match &expr.kind {
            ast::ExprKind::Num(number) => {
                hir::ExprKind::Num(self.ctx.numbers.push(number.clone()))
            }
            ast::ExprKind::Const(value) => hir::ExprKind::Const(*value),
            ast::ExprKind::Id(_) => {
                let (var, kind) = match self.bindings.names[&expr.uid] {
                    sem::Binding::Argument(arg) => self.vars[&arg.uid],
                    sem::Binding::Let(binding) => self.vars[&binding.uid],
                    sem::Binding::Mut(var) => self.vars[&var.uid],
                    sem::Binding::Index(_) => unreachable!(),
                };

                hir::ExprKind::Var(var, kind)
            }
            ast::ExprKind::Op(op, args) => {
                let kind = match op.kind {
                    ast::OpKind::Math(math) => {
                        self.lower_math_operation(op, math)?
                    }
                    ast::OpKind::Test(test) => hir::OpKind::Test(test),
                    ast::OpKind::Tensor(_) => unreachable!(),
                    ast::OpKind::FPCore(id) => hir::OpKind::Def(self.defs[&id]),
                };

                let op = hir::Operation {
                    kind,
                    span: op.span,
                };

                let args = self.lower_expressions(args)?;

                hir::ExprKind::Op(op, args)
            }
            ast::ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => hir::ExprKind::If(hir::If {
                cond: self.lower_expression(cond)?,
                if_true: self.lower_expression(true_branch)?,
                if_false: self.lower_expression(false_branch)?,
            }),
            ast::ExprKind::Let {
                bindings,
                body,
                sequential,
            } => hir::ExprKind::Let(hir::Let {
                writes: self.lower_bindings(
                    bindings.iter().map(|binding| (binding.uid, &binding.expr)),
                    hir::VarKind::Let,
                    true,
                )?,
                body: self.lower_expression(body)?,
                sequential: *sequential,
            }),
            ast::ExprKind::While {
                cond,
                vars,
                body,
                sequential,
            } => hir::ExprKind::While(hir::While {
                inits: self.lower_bindings(
                    vars.iter().map(|var| (var.uid, &var.init)),
                    |_| hir::VarKind::Mut,
                    true,
                )?,
                updates: self.lower_bindings(
                    vars.iter().map(|var| (var.uid, &var.update)),
                    |_| hir::VarKind::Mut,
                    false,
                )?,
                cond: self.lower_expression(cond)?,
                body: self.lower_expression(body)?,
                sequential: *sequential,
            }),
            ast::ExprKind::For { .. } => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("for expressions not supported")
                        .with_primary(expr.span, "unsupported expression"),
                );

                return Err(LoweringError);
            }
            ast::ExprKind::Tensor { .. } | ast::ExprKind::TensorStar { .. } => {
                unreachable!()
            }
            ast::ExprKind::Cast(_) => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("cast expressions not supported")
                        .with_primary(expr.span, "unsupported expression"),
                );

                return Err(LoweringError);
            }
            ast::ExprKind::Array(_) => unreachable!(),
            ast::ExprKind::Annotation { props, body } => {
                let outer = self.parent;
                let inner = self.lower_properties(props)?;

                self.parent = inner;

                let body = self.lower_expression(body);

                self.parent = outer;

                return body;
            }
        };

        let lowered = hir::Expression {
            kind,
            scope: self.parent,
            span: expr.span,
        };

        Ok(self.ctx.exprs.push(lowered))
    }

    fn lower_expressions<I>(
        &mut self,
        exprs: I,
    ) -> Result<EntityList<hir::ExprIdx>, LoweringError>
    where
        I: IntoIterator<Item = &'ast ast::Expression>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut list = EntityList::new();
        let it = exprs.into_iter();

        list.grow_at(0, it.len(), self.ctx.mut_pool());

        for (i, expr) in it.enumerate() {
            list.as_mut_slice(self.ctx.mut_pool())[i] =
                self.lower_expression(expr)?;
        }

        Ok(list)
    }

    fn lower_bindings<I, K>(
        &mut self,
        bindings: I,
        kind: K,
        init: bool,
    ) -> Result<EntityList<hir::WriteIdx>, LoweringError>
    where
        I: IntoIterator<Item = (ast::NodeId, &'ast ast::Expression)>,
        I::IntoIter: ExactSizeIterator,
        K: Fn(hir::ExprIdx) -> hir::VarKind,
    {
        let mut list = EntityList::new();
        let it = bindings.into_iter();

        list.grow_at(0, it.len(), self.ctx.mut_pool());

        for (i, (id, expr)) in it.enumerate() {
            let val = self.lower_expression(expr)?;

            let var = if init {
                let var = self.ctx.vars.push(());

                self.vars.insert(id, (var, kind(val)));

                var
            } else {
                self.vars[&id].0
            };

            let write = self.ctx.writes.push(hir::Write { var, val });

            list.as_mut_slice(self.ctx.mut_pool())[i] = write;
        }

        Ok(list)
    }

    fn lower_math_operation(
        &mut self,
        op: &ast::Operation,
        kind: ast::MathOp,
    ) -> Result<hir::OpKind, LoweringError> {
        if let Ok(op) = hir::ArithOp::try_from(kind) {
            Ok(hir::OpKind::Arith(op))
        } else if let Ok(f) = hir::SollyaFn::try_from(kind) {
            let var = self.ctx.ops.push(hir::SollyaExpr::Variable);
            let idx = self.ctx.ops.push(hir::SollyaExpr::Call(f, var));

            Ok(hir::OpKind::Sollya(idx))
        } else {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("unsupported operation")
                    .with_primary(op.span, "unsupported operator"),
            );

            Err(LoweringError)
        }
    }

    fn lower_property(
        &mut self,
        prop: &'ast ast::PropKind,
        parent: PackedOption<hir::ScopeIdx>,
    ) -> Result<PackedOption<hir::ScopeIdx>, LoweringError> {
        let prop = match prop {
            ast::PropKind::Pre(expr) => {
                hir::Property::Pre(self.lower_expression(expr)?)
            }
            ast::PropKind::CalyxDomain(domain) => {
                let left = self.ctx.numbers.push(domain.left.clone());
                let right = self.ctx.numbers.push(domain.right.clone());

                hir::Property::Domain(hir::Domain { left, right })
            }
            ast::PropKind::CalyxImpl(strategy) => {
                hir::Property::Impl(*strategy)
            }
            _ => {
                return Ok(parent);
            }
        };

        Ok(self.ctx.scopes.push(hir::Scope { prop, parent }).into())
    }

    fn lower_properties(
        &mut self,
        props: &'ast [ast::Property],
    ) -> Result<PackedOption<hir::ScopeIdx>, LoweringError> {
        props.iter().try_fold(self.parent, |parent, prop| {
            self.lower_property(&prop.kind, parent)
        })
    }
}

impl TryFrom<ast::MathOp> for hir::ArithOp {
    type Error = ();

    fn try_from(value: ast::MathOp) -> Result<Self, Self::Error> {
        match value {
            ast::MathOp::Add => Ok(hir::ArithOp::Add),
            ast::MathOp::Sub => Ok(hir::ArithOp::Sub),
            ast::MathOp::Mul => Ok(hir::ArithOp::Mul),
            ast::MathOp::Div => Ok(hir::ArithOp::Div),
            ast::MathOp::Neg => Ok(hir::ArithOp::Neg),
            ast::MathOp::Pow => Ok(hir::ArithOp::Pow),
            ast::MathOp::Sqrt => Ok(hir::ArithOp::Sqrt),
            ast::MathOp::FAbs => Ok(hir::ArithOp::Abs),
            ast::MathOp::FMax => Ok(hir::ArithOp::Max),
            ast::MathOp::FMin => Ok(hir::ArithOp::Min),
            _ => Err(()),
        }
    }
}

impl TryFrom<ast::MathOp> for hir::SollyaFn {
    type Error = ();

    fn try_from(value: ast::MathOp) -> Result<Self, Self::Error> {
        match value {
            ast::MathOp::Sin => Ok(hir::SollyaFn::Sin),
            ast::MathOp::Cos => Ok(hir::SollyaFn::Cos),
            ast::MathOp::Tan => Ok(hir::SollyaFn::Tan),
            ast::MathOp::Sinh => Ok(hir::SollyaFn::Sinh),
            ast::MathOp::Cosh => Ok(hir::SollyaFn::Cosh),
            ast::MathOp::Tanh => Ok(hir::SollyaFn::Tanh),
            ast::MathOp::ASin => Ok(hir::SollyaFn::ASin),
            ast::MathOp::ACos => Ok(hir::SollyaFn::ACos),
            ast::MathOp::ATan => Ok(hir::SollyaFn::ATan),
            ast::MathOp::ASinh => Ok(hir::SollyaFn::ASinh),
            ast::MathOp::ACosh => Ok(hir::SollyaFn::ACosh),
            ast::MathOp::ATanh => Ok(hir::SollyaFn::ATanh),
            ast::MathOp::Exp => Ok(hir::SollyaFn::Exp),
            ast::MathOp::ExpM1 => Ok(hir::SollyaFn::ExpM1),
            ast::MathOp::Log => Ok(hir::SollyaFn::Log),
            ast::MathOp::Log2 => Ok(hir::SollyaFn::Log2),
            ast::MathOp::Log10 => Ok(hir::SollyaFn::Log10),
            ast::MathOp::Log1P => Ok(hir::SollyaFn::Log1P),
            ast::MathOp::Erf => Ok(hir::SollyaFn::Erf),
            ast::MathOp::ErfC => Ok(hir::SollyaFn::ErfC),
            ast::MathOp::Sqrt => Ok(hir::SollyaFn::Sqrt),
            _ => Err(()),
        }
    }
}
