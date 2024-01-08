//! Glue code for polynomials.

use calyx_ir::{self as ir, build_assignments};

use super::{ComponentBuilder, ComponentManager, Horner, LookupTable};
use crate::utils::mangling::mangle;

pub struct PiecewisePoly<'a>(pub LookupTable<'a>);

impl ComponentBuilder for PiecewisePoly<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "poly",
            self.0.function.kind(),
            self.0.format,
            self.0.domain,
            self.0.degree,
            self.0.size,
        ))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        let mut stable = ir::Attributes::default();
        stable.insert(ir::Attribute::Bool(ir::BoolAttr::Stable), 1);

        vec![
            ir::PortDef::new(
                "in",
                u64::from(self.0.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.0.format.width),
                ir::Direction::Output,
                stable,
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> calyx_utils::CalyxResult<ir::Component> {
        let horner = Horner {
            format: self.0.format,
            degree: self.0.degree,
        };

        let (lookup, lookup_ports) = cm.get(&self.0, lib)?;
        let (horner, horner_ports) = cm.get(&horner, lib)?;

        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, true, false, None);
        let mut builder = ir::Builder::new(&mut component, lib);

        let lookup =
            builder.add_component(ir::Id::new("lookup"), lookup, lookup_ports);
        let horner =
            builder.add_component(ir::Id::new("horner"), horner, horner_ports);

        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            lookup["in"] = ? signature["in"];
            horner["in"] = ? signature["in"];
            horner["lut"] = ? lookup["out"];
            signature["out"] = ? horner["out"];
        );

        builder.component.continuous_assignments.extend(assigns);

        *component.control.borrow_mut() =
            ir::Control::invoke(horner, Vec::new(), Vec::new());

        Ok(component)
    }
}
