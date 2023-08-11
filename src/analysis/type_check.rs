use std::collections::HashMap;
use std::ops::Index;

use calyx_utils::{CalyxResult, Error};

use super::context::{Binding, ContextResolution};
use crate::fpcore::ast;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Number,
}

pub struct TypeCheck {
    types: HashMap<ast::NodeId, Type>,
}

impl TypeCheck {
    pub fn new(
        defs: &[ast::BenchmarkDef],
        context: &ContextResolution,
    ) -> CalyxResult<TypeCheck> {
        let mut builder = Builder {
            context,
            types: HashMap::new(),
        };

        for def in defs {
            builder.check_expression(&def.body)?;
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
                Binding::Argument(arg) => arg
                    .dims
                    .is_empty()
                    .then_some(Type::Number)
                    .ok_or_else(|| {
                        Error::misc(String::from("Unsupported argument type"))
                            .with_pos(&arg.var)
                    }),
                Binding::Let(binder) => Ok(self.types[&binder.expr.uid]),
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
                        return Err(Error::misc(String::from(
                            "Unsupported operation",
                        ))
                        .with_pos(op))
                    }
                };

                for arg in args {
                    if self.check_expression(arg)? != arg_ty {
                        let err = match arg_ty {
                            Type::Boolean => "Expected boolean",
                            Type::Number => "Expected number",
                        };

                        return Err(
                            Error::misc(String::from(err)).with_pos(arg)
                        );
                    };
                }

                (args.len() == arity).then_some(ty).ok_or_else(|| {
                    Error::misc(format!(
                        "Expected {arity} argument{}",
                        if arity == 1 { "" } else { "s" }
                    ))
                    .with_pos(op)
                })
            }
            ast::ExprKind::If {
                cond,
                if_true,
                if_false,
            } => {
                if self.check_expression(cond)? != Type::Boolean {
                    return Err(Error::misc(String::from("Expected boolean"))
                        .with_pos(cond.as_ref()));
                }

                let true_ty = self.check_expression(if_true)?;
                let false_ty = self.check_expression(if_false)?;

                (true_ty == false_ty).then_some(true_ty).ok_or_else(|| {
                    Error::misc(String::from(
                        "Branches have incompatible types",
                    ))
                    .with_pos(if_false.as_ref())
                })
            }
            ast::ExprKind::Let { binders, body, .. } => {
                for binder in binders {
                    self.check_expression(&binder.expr)?;
                }

                self.check_expression(body)
            }
            ast::ExprKind::While { .. } => unimplemented!(),
            ast::ExprKind::For { .. } => unimplemented!(),
            ast::ExprKind::Tensor { .. } | ast::ExprKind::TensorStar { .. } => {
                Err(Error::misc(String::from("Unsupported construct `tensor`")))
            }
            ast::ExprKind::Array(..) => {
                Err(Error::misc(String::from("Unsupported construct `array`")))
            }
            ast::ExprKind::Cast(body) => self.check_expression(body),
            ast::ExprKind::Annotation { body, .. } => {
                self.check_expression(body)
            }
        }?;

        self.types.insert(expr.uid, ty);

        Ok(ty)
    }
}
