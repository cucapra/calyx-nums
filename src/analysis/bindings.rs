use std::collections::HashMap;

use super::passes::{Pass, PassManager};
use crate::fpcore::{Visitor, ast, metadata, visitor};
use crate::utils::{Diagnostic, Reporter};

#[derive(Clone, Copy)]
pub enum Binding<'ast> {
    Argument(&'ast ast::Argument),
    Let(&'ast ast::Binding),
    Mut(&'ast ast::MutableVar),
    Index(&'ast ast::InductionVar),
}

#[derive(Clone, Copy, Default)]
pub struct Context<'ast> {
    pub domain: Option<&'ast metadata::CalyxDomain>,
    pub strategy: Option<&'ast metadata::CalyxImpl>,
}

pub struct NameResolution<'ast> {
    pub names: HashMap<ast::NodeId, Binding<'ast>>,
    pub props: HashMap<ast::NodeId, Context<'ast>>,
}

impl<'ast> Pass<'ast> for NameResolution<'ast> {
    fn run(pm: &PassManager<'_, 'ast>) -> Option<Self> {
        let mut builder = Builder {
            reporter: &mut pm.rpt(),
            result: NameResolution {
                names: HashMap::new(),
                props: HashMap::new(),
            },
            scopes: Vec::new(),
            parent: Default::default(),
        };

        builder.visit_definitions(pm.ast()).ok()?;

        Some(builder.result)
    }
}

#[derive(Debug)]
pub struct ResolutionError;

struct Builder<'p, 'ast> {
    reporter: &'p mut Reporter<'ast>,
    result: NameResolution<'ast>,
    scopes: Vec<HashMap<ast::Id, Binding<'ast>>>,
    parent: Context<'ast>,
}

impl<'ast> Builder<'_, 'ast> {
    fn update_parent(&mut self, props: &'ast [ast::Property]) {
        for prop in props {
            match &prop.kind {
                ast::PropKind::CalyxDomain(domain) => {
                    self.parent.domain = Some(domain);
                }
                ast::PropKind::CalyxImpl(strategy) => {
                    self.parent.strategy = Some(strategy);
                }
                _ => {
                    self.warn_ignored(prop);
                }
            }
        }
    }

    fn find_name(&self, symbol: ast::Id) -> Option<&Binding<'ast>> {
        for map in self.scopes.iter().rev() {
            if let Some(name) = map.get(&symbol) {
                return Some(name);
            }
        }

        None
    }

    fn warn_ignored(&mut self, prop: &ast::Property) {
        if !is_silent(&prop.kind) {
            self.reporter.emit(
                &Diagnostic::warning()
                    .with_message(format!(
                        "ignoring unsupported property `:{}`",
                        prop.kind.name(),
                    ))
                    .with_primary(prop.span, "unsupported property"),
            );
        }
    }
}

impl<'ast> Visitor<'ast> for Builder<'_, 'ast> {
    type Error = ResolutionError;

    fn visit_definition(
        &mut self,
        def: &'ast ast::FPCore,
    ) -> Result<(), ResolutionError> {
        let mut scope = HashMap::with_capacity(def.args.len());

        for arg in &def.args {
            scope.insert(arg.var.id, Binding::Argument(arg));

            for prop in &arg.props {
                self.warn_ignored(prop);
            }
        }

        self.scopes.clear();
        self.scopes.push(scope);

        self.parent.domain = None;
        self.parent.strategy = None;

        self.update_parent(&def.props);

        for prop in &def.props {
            if let ast::PropKind::Pre(expr) = &prop.kind {
                self.visit_expression(expr)?;
            }
        }

        visitor::visit_definition(self, def)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<(), ResolutionError> {
        self.result.props.insert(expr.uid, self.parent);

        match &expr.kind {
            ast::ExprKind::Num(_) => Ok(()),
            ast::ExprKind::Const(_) => Ok(()),
            ast::ExprKind::Id(sym) => {
                if let Some(binding) = self.find_name(sym.id) {
                    self.result.names.insert(expr.uid, *binding);

                    Ok(())
                } else {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message(format!(
                                "undefined name `{}`",
                                sym.id,
                            ))
                            .with_primary(sym.span, "undefined name"),
                    );

                    Err(ResolutionError)
                }
            }
            ast::ExprKind::Op(..) => visitor::visit_expression(self, expr),
            ast::ExprKind::If { .. } => visitor::visit_expression(self, expr),
            ast::ExprKind::Let {
                bindings,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::with_capacity(bindings.len());

                for binding in bindings {
                    self.visit_expression(&binding.expr)?;

                    scope.insert(binding.var.id, Binding::Let(binding));
                }

                self.scopes.push(scope);
                self.visit_expression(body)?;

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Let {
                bindings,
                body,
                sequential: true,
            } => {
                self.scopes.push(HashMap::new());

                for binding in bindings {
                    self.visit_expression(&binding.expr)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(binding.var.id, Binding::Let(binding));
                }

                self.visit_expression(body)?;

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::While {
                cond,
                vars,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::with_capacity(vars.len());

                for var in vars {
                    self.visit_expression(&var.init)?;

                    scope.insert(var.var.id, Binding::Mut(var));
                }

                self.scopes.push(scope);
                self.visit_expression(cond)?;
                self.visit_expression(body)?;

                for var in vars {
                    self.visit_expression(&var.update)?;
                }

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::While {
                cond,
                vars,
                body,
                sequential: true,
            } => {
                self.scopes.push(HashMap::new());

                for var in vars {
                    self.visit_expression(&var.init)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                self.visit_expression(cond)?;
                self.visit_expression(body)?;

                for var in vars {
                    self.visit_expression(&var.update)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Cast(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Array(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Annotation { props, body } => {
                let old = self.parent;

                self.update_parent(props);
                self.visit_expression(body)?;

                self.parent = old;

                Ok(())
            }
            _ => {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("unsupported expression")
                        .with_primary(expr.span, ""),
                );

                Err(ResolutionError)
            }
        }
    }
}

/// Whether the property can be safely ignored without emitting a warning.
fn is_silent(prop: &ast::PropKind) -> bool {
    matches!(
        prop,
        ast::PropKind::Name(_)
            | ast::PropKind::Description(_)
            | ast::PropKind::Cite(_)
            | ast::PropKind::Pre(_)
            | ast::PropKind::Spec(_)
            | ast::PropKind::Alt(_)
            | ast::PropKind::Example(_)
            | ast::PropKind::CalyxDomain(_)
    )
}
