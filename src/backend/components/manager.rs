use std::collections::HashSet;

use calyx_ir as ir;
use calyx_utils::CalyxResult;

pub trait ComponentBuilder {
    fn name(&self) -> ir::Id;

    fn signature(&self) -> Vec<ir::PortDef<u64>>;

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<ir::Component>;
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

    pub fn get<B: ComponentBuilder>(
        &mut self,
        builder: &B,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<(ir::Id, Vec<ir::PortDef<u64>>)> {
        let name = builder.name();

        if self.generated.insert(name) {
            let component = builder.build(name, self, lib)?;

            self.components.push(component);
        }

        Ok((name, builder.signature()))
    }

    pub fn into_components(self) -> Vec<ir::Component> {
        self.components
    }
}
