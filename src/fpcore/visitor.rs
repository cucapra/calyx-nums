//! AST traversal.

use super::ast;

pub trait Visitor<'ast> {
    type Error;

    fn visit_benchmarks<I>(&mut self, defs: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = &'ast ast::BenchmarkDef>,
    {
        visit_benchmarks(self, defs)
    }

    fn visit_benchmark(
        &mut self,
        def: &'ast ast::BenchmarkDef,
    ) -> Result<(), Self::Error> {
        visit_benchmark(self, def)
    }

    fn visit_binder(
        &mut self,
        binder: &'ast ast::Binder,
    ) -> Result<(), Self::Error> {
        visit_binder(self, binder)
    }

    fn visit_update(
        &mut self,
        update: &'ast ast::UpdateRule,
    ) -> Result<(), Self::Error> {
        visit_update(self, update)
    }

    fn visit_condition(
        &mut self,
        cond: &'ast ast::Condition,
    ) -> Result<(), Self::Error> {
        visit_condition(self, cond)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<(), Self::Error> {
        visit_expression(self, expr)
    }
}

pub fn visit_benchmarks<'ast, V, I>(v: &mut V, defs: I) -> Result<(), V::Error>
where
    V: Visitor<'ast> + ?Sized,
    I: IntoIterator<Item = &'ast ast::BenchmarkDef>,
{
    for def in defs {
        v.visit_benchmark(def)?;
    }

    Ok(())
}

pub fn visit_benchmark<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    def: &'ast ast::BenchmarkDef,
) -> Result<(), V::Error> {
    v.visit_expression(&def.body)
}

pub fn visit_binder<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    binder: &'ast ast::Binder,
) -> Result<(), V::Error> {
    v.visit_expression(&binder.expr)
}

pub fn visit_update<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    update: &'ast ast::UpdateRule,
) -> Result<(), V::Error> {
    v.visit_expression(&update.init)?;
    v.visit_expression(&update.update)
}

pub fn visit_condition<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    cond: &'ast ast::Condition,
) -> Result<(), V::Error> {
    v.visit_expression(&cond.val)
}

pub fn visit_expression<'ast, V: Visitor<'ast> + ?Sized>(
    v: &mut V,
    expr: &'ast ast::Expression,
) -> Result<(), V::Error> {
    match &expr.kind {
        ast::ExprKind::Num(_) => Ok(()),
        ast::ExprKind::Const(_) => Ok(()),
        ast::ExprKind::Id(_) => Ok(()),
        ast::ExprKind::Op(_, exprs) | ast::ExprKind::Array(exprs) => {
            for expr in exprs {
                v.visit_expression(expr)?;
            }

            Ok(())
        }
        ast::ExprKind::If {
            cond,
            if_true,
            if_false,
        } => {
            v.visit_expression(cond)?;
            v.visit_expression(if_true)?;
            v.visit_expression(if_false)
        }
        ast::ExprKind::Let {
            binders,
            body,
            sequential: _,
        } => {
            for binder in binders {
                v.visit_binder(binder)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::While {
            cond,
            rules,
            body,
            sequential: _,
        } => {
            v.visit_expression(cond)?;

            for rule in rules {
                v.visit_update(rule)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::For {
            conditions,
            rules,
            body,
            sequential: _,
        }
        | ast::ExprKind::TensorStar {
            conditions,
            rules,
            body,
        } => {
            for cond in conditions {
                v.visit_condition(cond)?;
            }

            for rule in rules {
                v.visit_update(rule)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::Tensor { conditions, body } => {
            for cond in conditions {
                v.visit_condition(cond)?;
            }

            v.visit_expression(body)
        }
        ast::ExprKind::Cast(expr) => v.visit_expression(expr),
        ast::ExprKind::Annotation { props: _, body } => {
            v.visit_expression(body)
        }
    }
}
