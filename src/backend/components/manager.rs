use std::collections::HashSet;

use calyx_ir as ir;

use crate::utils::Diagnostic;

pub trait PrimitiveBuilder {
    fn name(&self) -> ir::Id;

    fn build(&self, name: ir::Id) -> Result<ir::Primitive, Diagnostic>;
}

pub trait ComponentBuilder {
    fn name(&self) -> ir::Id;

    fn signature(&self) -> Vec<ir::PortDef<u64>>;

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Component, Diagnostic>;
}

pub struct ComponentManager {
    components: Vec<ir::Component>,
    generated: HashSet<ir::Id>,
}

impl ComponentManager {
    pub fn new() -> ComponentManager {
        ComponentManager {
            components: Vec::new(),
            generated: HashSet::new(),
        }
    }

    pub fn get_primitive<B: PrimitiveBuilder>(
        &mut self,
        builder: &B,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Id, Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            lib.add_inline_primitive(builder.build(name)?).set_source();
        }

        Ok(name)
    }

    pub fn get<B: ComponentBuilder>(
        &mut self,
        builder: &B,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<(ir::Id, Vec<ir::PortDef<u64>>), Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            let component = builder.build(name, self, lib)?;

            self.components.push(component);
        }

        Ok((name, builder.signature()))
    }

    pub fn add(&mut self, component: ir::Component) {
        self.components.push(component);
    }

    pub fn into_components(self) -> Vec<ir::Component> {
        self.components
    }
}
