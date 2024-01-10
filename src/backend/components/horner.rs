//! Horner evaluation of polynomials.

use std::iter;

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::CalyxResult;

use super::{ComponentBuilder, ComponentManager};
use crate::backend::builtins;
use crate::backend::primitives::{NamedPrimitive, PartSelect};
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
                u64::from(self.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "lut",
                u64::from(self.degree + 1) * u64::from(self.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.format.width),
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
        let select = PartSelect::add(lib);
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, true, false, None);
        let mut builder = ir::Builder::new(&mut component, lib).not_generated();

        structure!(builder;
            let acc = prim std_reg(u64::from(self.format.width));
            let add = prim std_add(u64::from(self.format.width));
        );

        let mul = {
            let decl = builtins::mul(self.format);

            builder.add_primitive(
                decl.prefix_hint,
                decl.name,
                &decl.build_params(self.format),
            )
        };

        let selects: Vec<_> = (0..=self.degree)
            .map(|i| {
                let params = [
                    u64::from(self.degree + 1) * u64::from(self.format.width),
                    u64::from(self.format.width),
                    u64::from(i) * u64::from(self.format.width),
                ];

                builder.add_primitive("sel", select, &params)
            })
            .collect();

        let (leading, addends) = selects.split_first().unwrap();

        let [in_, out, left, right] =
            ["in", "out", "left", "right"].map(ir::Id::new);

        let init = ir::Control::invoke(
            acc.clone(),
            vec![(in_, leading.borrow().get(out))],
            vec![],
        );

        let multiplies: Vec<_> = iter::repeat_with(|| {
            let signature = &builder.component.signature;

            ir::Control::invoke(
                mul.clone(),
                vec![
                    (left, signature.borrow().get(in_)),
                    (right, acc.borrow().get(out)),
                ],
                vec![],
            )
        })
        .take(addends.len())
        .collect();

        let accumulates: Vec<_> = addends
            .iter()
            .map(|addend| {
                let group = builder.add_comb_group("addend");

                let assigns = build_assignments!(builder;
                    add[left] = ? addend[out];
                    add[right] = ? mul[out];
                );

                group.borrow_mut().assignments.extend(assigns);

                ir::Control::Invoke(ir::Invoke {
                    comp: acc.clone(),
                    inputs: vec![(in_, add.borrow().get(out))],
                    outputs: vec![],
                    attributes: Default::default(),
                    comb_group: Some(group),
                    ref_cells: vec![],
                })
            })
            .collect();

        let signature = &builder.component.signature;

        for cell in selects {
            let [assign] = build_assignments!(builder;
                cell[in_] = ? signature["lut"];
            );

            builder.component.continuous_assignments.push(assign);
        }

        let [assign] = build_assignments!(builder;
            signature[out] = ? acc[out];
        );

        builder.component.continuous_assignments.push(assign);

        *component.control.borrow_mut() = ir::Control::seq(
            iter::once(init)
                .chain(itertools::interleave(multiplies, accumulates))
                .collect(),
        );

        Ok(component)
    }
}
