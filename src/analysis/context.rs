use std::collections::HashMap;

use calyx_utils::{CalyxResult, Error, Id};

use crate::fpcore::{ast, metadata, visitor, Visitor};

#[derive(Clone, Copy)]
pub enum Binding<'ast> {
    Argument(&'ast ast::ArgumentDef),
    Let(&'ast ast::Binder),
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

impl<'ast> ContextResolution<'ast> {
    pub fn new(
        defs: &'ast [ast::BenchmarkDef],
    ) -> CalyxResult<ContextResolution<'ast>> {
        let mut builder = Builder {
            result: ContextResolution {
                names: HashMap::new(),
                props: HashMap::new(),
            },
            scopes: Vec::new(),
            parent: Default::default(),
        };

        builder.visit_benchmarks(defs)?;

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

    fn visit_benchmark(
        &mut self,
        def: &'ast ast::BenchmarkDef,
    ) -> CalyxResult<()> {
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

        visitor::visit_benchmark(self, def)
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
                    Error::undefined(sym.id, String::from("variable"))
                        .with_pos(sym)
                })?;

                self.result.names.insert(expr.uid, *binding);

                Ok(())
            }
            ast::ExprKind::Op(..) => visitor::visit_expression(self, expr),
            ast::ExprKind::If { .. } => visitor::visit_expression(self, expr),
            ast::ExprKind::Let {
                binders,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::new();

                for binder in binders {
                    self.visit_expression(&binder.expr)?;

                    scope.insert(binder.var.id, Binding::Let(binder));
                }

                self.scopes.push(scope);
                self.visit_expression(body)?;

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Let {
                binders,
                body,
                sequential: true,
            } => {
                self.scopes.push(HashMap::new());

                for binder in binders {
                    self.visit_expression(&binder.expr)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(binder.var.id, Binding::Let(binder));
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
    match prop {
        ast::Property::Name(_) => true,
        ast::Property::Description(_) => true,
        ast::Property::Cite(_) => true,
        ast::Property::Pre(_) => true,
        ast::Property::Spec(_) => true,
        ast::Property::Alt(_) => true,
        ast::Property::Example(_) => true,
        _ => false,
    }
}
