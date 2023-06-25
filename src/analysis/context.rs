use std::collections::HashMap;

use calyx_utils::{CalyxResult, Error};

use crate::fpcore::{ast, metadata, visitor, Visitor};

#[derive(Clone, Copy, Default)]
pub struct Context<'ast> {
    pub domain: Option<&'ast metadata::CalyxDomain>,
    pub strategy: Option<&'ast metadata::CalyxImpl>,
}

pub struct ContextResolution<'ast> {
    pub map: HashMap<ast::NodeId, Context<'ast>>,
}

impl<'ast> ContextResolution<'ast> {
    pub fn new(
        defs: &'ast [ast::BenchmarkDef],
    ) -> CalyxResult<ContextResolution<'ast>> {
        let mut builder = Builder {
            result: ContextResolution {
                map: HashMap::new(),
            },
            parent: Default::default(),
        };

        builder.visit_benchmarks(defs)?;

        Ok(builder.result)
    }
}

struct Builder<'ast> {
    result: ContextResolution<'ast>,
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
                    log::warn!("Ignoring property `{}`", prop.name());
                }
            }
        }
    }
}

impl<'ast> Visitor<'ast> for Builder<'ast> {
    type Error = Error;

    fn visit_benchmark(
        &mut self,
        def: &'ast ast::BenchmarkDef,
    ) -> CalyxResult<()> {
        self.parent.domain = None;
        self.parent.strategy = None;

        self.update_parent(&def.props);

        visitor::visit_benchmark(self, def)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> CalyxResult<()> {
        self.result.map.insert(expr.uid, self.parent);

        match &expr.kind {
            ast::ExprKind::Annotation { props, body } => {
                let old = self.parent;

                self.update_parent(props);
                self.visit_expression(body)?;

                self.parent = old;

                Ok(())
            }
            _ => visitor::visit_expression(self, expr),
        }
    }
}
