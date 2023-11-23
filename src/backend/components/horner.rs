//! Horner evaluation of polynomials.

use std::iter;

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::CalyxResult;

use super::{ComponentBuilder, ComponentManager};
use crate::backend::builtins;
use crate::backend::primitives::select;
use crate::format::Format;
use crate::utils::mangling::mangle;

pub struct Horner<'a> {
    pub format: &'a Format,
    pub degree: u32,
}

impl ComponentBuilder for Horner<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!("horner", self.format, self.degree))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        let mut stable = ir::Attributes::default();
        stable.insert(ir::Attribute::Bool(ir::BoolAttr::Stable), 1);

        vec![
            ir::PortDef::new(
                "in",
                self.format.width,
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "lut",
                u64::from(self.degree + 1) * self.format.width,
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                self.format.width,
                ir::Direction::Output,
                stable,
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        _cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<ir::Component> {
        let select = ir::Id::new("select");

        if lib.find_primitive(select).is_none() {
            let primitive = select::compile_select(select);

            lib.add_inline_primitive(primitive).set_source();
        }

        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, true, false, None);
        let mut builder = ir::Builder::new(&mut component, lib);

        structure!(builder;
            let acc = prim std_reg(self.format.width);
            let add = prim std_add(self.format.width);
            let high = constant(1, 1);
        );

        let mul = {
            let decl = builtins::mul(self.format);

            builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(self.format),
            )
        };

        let coefficients: Vec<_> = (0..=self.degree)
            .map(|i| {
                let params = [
                    u64::from(self.degree + 1) * self.format.width,
                    self.format.width,
                    u64::from(i) * self.format.width,
                ];

                builder.add_primitive("sel", select, &params)
            })
            .collect();

        let (leading, addends) = coefficients.split_first().unwrap();

        let init = {
            let group = builder.add_group("init");

            let assigns = build_assignments!(builder;
                acc["in"] = ? leading["out"];
                acc["write_en"] = ? high["out"];
                group["done"] = ? acc["done"];
            );

            group.borrow_mut().assignments.extend(assigns);

            group
        };

        let mads: Vec<_> = addends
            .iter()
            .map(|addend| {
                let mad = builder.add_group("mad");

                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    mul["left"] = ? signature["in"];
                    mul["right"] = ? acc["out"];
                    mul["go"] = ? high["out"];
                    add["left"] = ? addend["out"];
                    add["right"] = ? mul["out"];
                    acc["in"] = ? add["out"];
                    acc["write_en"] = ? mul["done"];
                    mad["done"] = ? acc["done"];
                );

                mad.borrow_mut().assignments.extend(assigns);

                mad
            })
            .collect();

        let signature = &builder.component.signature;

        for cell in coefficients {
            let [assign] = build_assignments!(builder;
                cell["in"] = ? signature["lut"];
            );

            builder.component.continuous_assignments.push(assign);
        }

        let [assign] = build_assignments!(builder;
            signature["out"] = ? acc["out"];
        );

        builder.component.continuous_assignments.push(assign);

        *component.control.borrow_mut() = ir::Control::seq(
            iter::once(init)
                .chain(mads)
                .map(ir::Control::enable)
                .collect(),
        );

        Ok(component)
    }
}