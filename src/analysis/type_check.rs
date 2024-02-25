use std::collections::HashMap;
use std::ops::Index;

use calyx_utils::{CalyxResult, Error};

use super::context::{Binding, ContextResolution};
use super::passes::{Pass, PassManager};
use crate::fpcore::ast;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Number,
}

pub struct TypeCheck {
    types: HashMap<ast::NodeId, Type>,
}

impl Pass<'_> for TypeCheck {
    fn run(pm: &PassManager) -> CalyxResult<Self> {
        let mut builder = Builder {
            context: pm.get_analysis()?,
            types: HashMap::new(),
        };

        for def in pm.ast() {
            builder.check_definition(def)?;
        }

        Ok(TypeCheck {
            types: builder.types,
        })
    }
}

impl Index<ast::NodeId> for TypeCheck {
    type Output = Type;

    fn index(&self, index: ast::NodeId) -> &Self::Output {
        &self.types[&index]
    }
}

struct Builder<'a> {
    context: &'a ContextResolution<'a>,
    types: HashMap<ast::NodeId, Type>,
}

impl Builder<'_> {
    fn check_definition(&mut self, def: &ast::FPCore) -> CalyxResult<()> {
        self.expect(&def.body, Type::Number)
    }

    fn check_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> CalyxResult<Type> {
        let ty = match &expr.kind {
            ast::ExprKind::Num(_) => Type::Number,
            ast::ExprKind::Const(kind) => match kind {
                ast::Constant::Math(_) => Type::Number,
                ast::Constant::Bool(_) => Type::Boolean,
            },
            ast::ExprKind::Id(_) => match self.context.names[&expr.uid] {
                Binding::Argument(arg) => {
                    if !arg.dims.is_empty() {
                        unimplemented!();
                    }

                    Type::Number
                }
                Binding::Let(binding) => self.types[&binding.expr.uid],
                Binding::Mut(var) => self.types[&var.init.uid],
                Binding::Index(_) => Type::Number,
            },
            ast::ExprKind::Op(op, args) => {
                let (ret_ty, arg_ty, arity) = match op.kind {
                    ast::OpKind::Math(op) => {
                        (Type::Number, Type::Number, op.arity())
                    }
                    ast::OpKind::Test(op) => {
                        let arg_ty = match op {
                            ast::TestOp::And => Type::Boolean,
                            ast::TestOp::Or => Type::Boolean,
                            ast::TestOp::Not => Type::Boolean,
                            _ => Type::Number,
                        };

                        (Type::Boolean, arg_ty, op.arity())
                    }
                    _ => unimplemented!(),
                };

                if args.len() != arity {
                    let plural = if arity == 1 { "" } else { "s" };

                    let msg = format!(
                        "Expected {arity} argument{plural}, got {}",
                        args.len(),
                    );

                    return Err(Error::misc(msg).with_pos(op));
                }

                for arg in args {
                    self.expect(arg, arg_ty)?;
                }

                ret_ty
            }
            ast::ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => {
                self.expect(cond, Type::Boolean)?;

                let ty = self.check_expression(true_branch)?;
                self.expect(false_branch, ty)?;

                ty
            }
            ast::ExprKind::Let { bindings, body, .. } => {
                for binding in bindings {
                    self.check_expression(&binding.expr)?;
                }

                self.check_expression(body)?
            }
            ast::ExprKind::While {
                cond, vars, body, ..
            } => {
                for var in vars {
                    self.check_expression(&var.init)?;
                }

                for var in vars {
                    self.expect(&var.update, self.types[&var.init.uid])?;
                }

                self.expect(cond, Type::Boolean)?;
                self.check_expression(body)?
            }
            ast::ExprKind::Cast(body) => {
                self.expect(body, Type::Number)?;

                Type::Number
            }
            ast::ExprKind::Annotation { body, .. } => {
                self.check_expression(body)?
            }
            _ => unimplemented!(),
        };

        self.types.insert(expr.uid, ty);

        Ok(ty)
    }

    fn expect(&mut self, expr: &ast::Expression, ty: Type) -> CalyxResult<()> {
        if self.check_expression(expr)? != ty {
            let msg = match ty {
                Type::Boolean => "Expected boolean",
                Type::Number => "Expected number",
            };

            return Err(Error::misc(msg).with_pos(expr));
        }

        Ok(())
    }
}
