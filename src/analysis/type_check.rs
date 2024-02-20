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
            ast::ExprKind::Num(_) => Ok(Type::Number),
            ast::ExprKind::Const(kind) => match kind {
                ast::Constant::Math(_) => Ok(Type::Number),
                ast::Constant::Bool(_) => Ok(Type::Boolean),
            },
            ast::ExprKind::Id(_) => match self.context.names[&expr.uid] {
                Binding::Argument(arg) => {
                    arg.dims.is_empty().then_some(Type::Number).ok_or_else(
                        || {
                            Error::misc("Unsupported argument type")
                                .with_pos(&arg.var)
                        },
                    )
                }
                Binding::Let(binding) => Ok(self.types[&binding.expr.uid]),
            },
            ast::ExprKind::Op(op, args) => {
                let (ty, arg_ty, arity) = match op.kind {
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
                    ast::OpKind::Tensor(_) => {
                        return Err(
                            Error::misc("Unsupported operation").with_pos(op)
                        )
                    }
                };

                for arg in args {
                    self.expect(arg, arg_ty)?;
                }

                (args.len() == arity).then_some(ty).ok_or_else(|| {
                    Error::misc(format!(
                        "Expected {arity} argument{}, got {}",
                        if arity == 1 { "" } else { "s" },
                        args.len()
                    ))
                    .with_pos(op)
                })
            }
            ast::ExprKind::If {
                cond,
                true_branch,
                false_branch,
            } => {
                self.expect(cond, Type::Boolean)?;

                let true_ty = self.check_expression(true_branch)?;
                let false_ty = self.check_expression(false_branch)?;

                (true_ty == false_ty).then_some(true_ty).ok_or_else(|| {
                    Error::misc("Branches have incompatible types")
                        .with_pos(false_branch.as_ref())
                })
            }
            ast::ExprKind::Let { bindings, body, .. } => {
                for binding in bindings {
                    self.check_expression(&binding.expr)?;
                }

                self.check_expression(body)
            }
            ast::ExprKind::While { .. } => unimplemented!(),
            ast::ExprKind::For { .. } => unimplemented!(),
            ast::ExprKind::Tensor { .. } | ast::ExprKind::TensorStar { .. } => {
                Err(Error::misc("Unsupported construct `tensor`"))
            }
            ast::ExprKind::Array(..) => {
                Err(Error::misc("Unsupported construct `array`"))
            }
            ast::ExprKind::Cast(body) => self.check_expression(body),
            ast::ExprKind::Annotation { body, .. } => {
                self.check_expression(body)
            }
        }?;

        self.types.insert(expr.uid, ty);

        Ok(ty)
    }

    fn expect(&mut self, expr: &ast::Expression, ty: Type) -> CalyxResult<()> {
        if self.check_expression(expr)? == ty {
            Ok(())
        } else {
            let msg = match ty {
                Type::Boolean => "Expected boolean",
                Type::Number => "Expected number",
            };

            Err(Error::misc(msg).with_pos(expr))
        }
    }
}
