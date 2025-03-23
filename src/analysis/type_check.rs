use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::ops::Index;

use strum_macros::Display;

use super::bindings::{Binding, NameResolution};
use super::passes::{Pass, PassManager};
use crate::fpcore::ast;
use crate::utils::{Diagnostic, Reporter};

#[derive(Clone, Copy, PartialEq, Eq, Display)]
pub enum Type {
    #[strum(to_string = "boolean")]
    Boolean,
    #[strum(to_string = "number")]
    Number,
}

pub struct TypeCheck {
    types: HashMap<ast::NodeId, Type>,
}

impl Pass<'_> for TypeCheck {
    fn run(pm: &PassManager) -> Option<Self> {
        let mut builder = Builder {
            bindings: pm.get_analysis()?,
            reporter: &mut pm.rpt(),
            types: HashMap::new(),
        };

        for def in pm.ast() {
            builder.check_definition(def).ok()?;
        }

        Some(TypeCheck {
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

#[derive(Debug)]
pub struct TypeError;

struct Builder<'p, 'ast> {
    bindings: &'p NameResolution<'ast>,
    reporter: &'p mut Reporter<'ast>,
    types: HashMap<ast::NodeId, Type>,
}

impl Builder<'_, '_> {
    fn check_definition(&mut self, def: &ast::FPCore) -> Result<(), TypeError> {
        for prop in &def.props {
            if let ast::PropKind::Pre(expr) = &prop.kind {
                self.expect(expr, Type::Boolean)?;
            }
        }

        self.expect(&def.body, Type::Number)
    }

    fn check_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<Type, TypeError> {
        let ty = match &expr.kind {
            ast::ExprKind::Num(_) => Type::Number,
            ast::ExprKind::Const(kind) => match kind {
                ast::Constant::Math(_) => Type::Number,
                ast::Constant::Bool(_) => Type::Boolean,
            },
            ast::ExprKind::Id(_) => match self.bindings.names[&expr.uid] {
                Binding::Argument(arg) => {
                    if !arg.dims.is_empty() {
                        self.reporter.emit(
                            &Diagnostic::error()
                                .with_message("tensor arguments not supported")
                                .with_primary(
                                    arg.var.span,
                                    "unsupported argument type",
                                ),
                        );

                        return Err(TypeError);
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
                        (Type::Number, Type::Number, Arity::Fixed(op.arity()))
                    }
                    ast::OpKind::Test(op) => {
                        let arg_ty = match op {
                            ast::TestOp::And => Type::Boolean,
                            ast::TestOp::Or => Type::Boolean,
                            ast::TestOp::Not => Type::Boolean,
                            _ => Type::Number,
                        };

                        let arity = if op.is_variadic() {
                            Arity::Variadic
                        } else {
                            Arity::UNARY
                        };

                        (Type::Boolean, arg_ty, arity)
                    }
                    ast::OpKind::Tensor(_) => {
                        self.reporter.emit(
                            &Diagnostic::error()
                                .with_message("tensor operations not supported")
                                .with_primary(op.span, "unsupported operation"),
                        );

                        return Err(TypeError);
                    }
                };

                let count = args.len();

                if !arity.check(count) {
                    let label = match arity {
                        Arity::Variadic => {
                            format!("expected 2+ arguments, found {count}")
                        }
                        Arity::UNARY => {
                            format!("expected 1 argument, found {count}")
                        }
                        Arity::Fixed(arity) => {
                            format!("expected {arity} arguments, found {count}")
                        }
                    };

                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("type error")
                            .with_primary(op.span, label),
                    );

                    return Err(TypeError);
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

                let true_ty = self.check_expression(true_branch)?;
                let false_ty = self.check_expression(false_branch)?;

                if true_ty != false_ty {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("mismatched types")
                            .with_secondary(
                                expr.span,
                                "`if` branches have incompatible types",
                            )
                            .with_secondary(
                                true_branch.span,
                                format!("this has type `{true_ty}`"),
                            )
                            .with_primary(
                                false_branch.span,
                                format!("expected {true_ty}, found {false_ty}"),
                            ),
                    );

                    return Err(TypeError);
                }

                true_ty
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
                    let init_ty = self.types[&var.init.uid];
                    let update_ty = self.check_expression(&var.update)?;

                    if init_ty != update_ty {
                        let init_label =
                            format!("variable initialized to type `{init_ty}`");
                        let update_label =
                            format!("expected {init_ty}, found {update_ty}");

                        self.reporter.emit(
                            &Diagnostic::error()
                                .with_message("mismatched types")
                                .with_secondary(var.init.span, init_label)
                                .with_primary(var.update.span, update_label),
                        );

                        return Err(TypeError);
                    }
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
            _ => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported expression")
                        .with_primary(expr.span, ""),
                );

                return Err(TypeError);
            }
        };

        self.types.insert(expr.uid, ty);

        Ok(ty)
    }

    fn expect(
        &mut self,
        expr: &ast::Expression,
        ty: Type,
    ) -> Result<(), TypeError> {
        let found = self.check_expression(expr)?;

        if found != ty {
            self.reporter.emit(
                &Diagnostic::error()
                    .with_message("mismatched types")
                    .with_primary(
                        expr.span,
                        format!("expected {ty}, found {found}"),
                    ),
            );

            return Err(TypeError);
        }

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Arity {
    Variadic,
    Fixed(NonZeroUsize),
}

impl Arity {
    const UNARY: Arity =
        Arity::Fixed(unsafe { NonZeroUsize::new_unchecked(1) });

    fn check(self, count: usize) -> bool {
        match self {
            Arity::Variadic => count >= 2,
            Arity::Fixed(arity) => count == arity.get(),
        }
    }
}
