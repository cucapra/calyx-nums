use std::collections::HashMap;

use calyx_utils::{CalyxResult, Error, Id};

use super::passes::{Pass, PassManager};
use crate::fpcore::{ast, metadata, visitor, Visitor};

#[derive(Clone, Copy)]
pub enum Binding<'ast> {
    Argument(&'ast ast::Argument),
    Let(&'ast ast::Binding),
}

#[derive(Clone, Copy, Default)]
pub struct Context<'ast> {
    pub domain: Option<&'ast metadata::CalyxDomain>,
    pub strategy: Option<&'ast metadata::CalyxImpl>,
}

pub struct ContextResolution<'ast> {
    pub names: HashMap<ast::NodeId, Binding<'ast>>,
    pub props: HashMap<ast::NodeId, Context<'ast>>,
}

impl<'ast> Pass<'ast> for ContextResolution<'ast> {
    fn run(pm: &PassManager<'_, 'ast>) -> CalyxResult<Self> {
        let mut builder = Builder {
            result: ContextResolution {
                names: HashMap::new(),
                props: HashMap::new(),
            },
            scopes: Vec::new(),
            parent: Default::default(),
        };

        builder.visit_definitions(pm.ast())?;

        Ok(builder.result)
    }
}

struct Builder<'ast> {
    result: ContextResolution<'ast>,
    scopes: Vec<HashMap<Id, Binding<'ast>>>,
    parent: Context<'ast>,
}

impl<'ast> Builder<'ast> {
    fn update_parent(&mut self, props: &'ast [ast::Property]) {
        for prop in props {
            match prop {
                ast::Property::CalyxDomain(domain) => {
                    self.parent.domain = Some(domain);
                }
                ast::Property::CalyxImpl(strategy) => {
                    self.parent.strategy = Some(strategy);
                }
                _ => {
                    if !is_silent(prop) {
                        log::warn!("Ignoring property `{}`", prop.name());
                    }
                }
            }
        }
    }

    fn find_name(&self, symbol: Id) -> Option<&Binding<'ast>> {
        for map in self.scopes.iter().rev() {
            if let Some(name) = map.get(&symbol) {
                return Some(name);
            }
        }

        None
    }
}

impl<'ast> Visitor<'ast> for Builder<'ast> {
    type Error = Error;

    fn visit_definition(&mut self, def: &'ast ast::FPCore) -> CalyxResult<()> {
        let mut scope = HashMap::new();

        for arg in &def.args {
            scope.insert(arg.var.id, Binding::Argument(arg));

            for prop in &arg.props {
                if !is_silent(prop) {
                    log::warn!("Ignoring property `{}`", prop.name());
                }
            }
        }

        self.scopes.clear();
        self.scopes.push(scope);

        self.parent.domain = None;
        self.parent.strategy = None;

        self.update_parent(&def.props);

        visitor::visit_definition(self, def)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> CalyxResult<()> {
        self.result.props.insert(expr.uid, self.parent);

        match &expr.kind {
            ast::ExprKind::Num(_) => Ok(()),
            ast::ExprKind::Const(_) => Ok(()),
            ast::ExprKind::Id(sym) => {
                let binding = self.find_name(sym.id).ok_or_else(|| {
                    Error::undefined(sym.id, "variable").with_pos(sym)
                })?;

                self.result.names.insert(expr.uid, *binding);

                Ok(())
            }
            ast::ExprKind::Op(..) => visitor::visit_expression(self, expr),
            ast::ExprKind::If { .. } => visitor::visit_expression(self, expr),
            ast::ExprKind::Let {
                bindings,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::new();

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
            ast::ExprKind::Cast(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Array(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Annotation { props, body } => {
                let old = self.parent;

                self.update_parent(props);
                self.visit_expression(body)?;

                self.parent = old;

                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}

/// Whether the property can be safely ignored without emitting a warning.
fn is_silent(prop: &ast::Property) -> bool {
    matches!(
        prop,
        ast::Property::Name(_)
            | ast::Property::Description(_)
            | ast::Property::Cite(_)
            | ast::Property::Pre(_)
            | ast::Property::Spec(_)
            | ast::Property::Alt(_)
            | ast::Property::Example(_)
            | ast::Property::CalyxDomain(_)
    )
}
