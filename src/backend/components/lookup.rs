//! Coefficient lookup tables.

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::{CalyxResult, Error};

use super::{ComponentBuilder, ComponentManager};
use crate::backend::libm::Function;
use crate::backend::primitives::lut;
use crate::format::Format;
use crate::functions::addressing::{AddressSpec, TableDomain};
use crate::functions::remez;
use crate::utils::mangling::mangle;

pub struct LookupTable<'a> {
    pub function: &'a Function,
    pub format: &'a Format,
    pub spec: &'a AddressSpec,
    pub domain: &'a TableDomain,
    pub degree: u32,
    pub size: u32,
}

impl LookupTable<'_> {
    fn build_primitive(
        &self,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<ir::Id> {
        let table = remez::build_table(
            self.function.kind(),
            self.degree,
            &self.domain.left,
            &self.domain.right,
            self.size,
            self.format,
        )?;

        let format_error = |post| {
            Error::misc(format!("Error in implementation of {}", self.function))
                .with_pos(self.function)
                .with_post_msg(Some(post))
        };

        let values: Vec<_> = table
            .iter()
            .map(|row| {
                itertools::process_results(
                    row.iter().map(|value| {
                        value.to_format(self.format).ok_or_else(|| {
                            format_error(format!(
                                "Generated constant {value} is not \
                                 representable in the given format"
                            ))
                        })
                    }),
                    |bits| lut::pack(bits, self.format.width),
                )
            })
            .collect::<CalyxResult<_>>()?;

        let name = ir::Id::new(mangle!(
            "lut",
            self.function.kind(),
            self.format,
            self.domain,
            self.degree,
            self.size,
        ));

        let width = u64::from(self.degree + 1) * u64::from(self.format.width);

        let primitive =
            lut::compile_lut(name, self.spec.idx_width, width, &values);

        lib.add_inline_primitive(primitive).set_source();

        Ok(name)
    }
}

impl ComponentBuilder for LookupTable<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "lookup",
            self.function.kind(),
            self.format,
            self.domain,
            self.degree,
            self.size,
        ))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        vec![
            ir::PortDef::new(
                "in",
                u64::from(self.format.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.degree + 1) * u64::from(self.format.width),
                ir::Direction::Output,
                Default::default(),
            ),
            ir::PortDef::new(
                "arg",
                u64::from(self.format.width),
                ir::Direction::Output,
                Default::default(),
            ),
        ]
    }

    fn build(
        &self,
        name: ir::Id,
        _cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<ir::Component> {
        let lut = self.build_primitive(lib)?;
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, false, true, None);
        let mut builder = ir::Builder::new(&mut component, lib).not_generated();

        let primitive = builder.add_primitive("lut", lut, &[]);

        let global = u64::from(self.format.width);
        let spec = self.spec;

        structure!(builder;
            let sub = prim std_sub(global);
            let slice = prim std_bit_slice(
                global,
                spec.idx_lsb,
                spec.idx_lsb + spec.idx_width - 1,
                spec.idx_width
            );
            let left = constant(spec.subtrahend, global);
        );

        let [in_, out] = ["in", "out"].map(ir::Id::new);
        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            sub["left"] = ? signature[in_];
            sub["right"] = ? left[out];
            slice[in_] = ? sub[out];
            primitive["idx"] = ? slice[out];
            signature[out] = ? primitive[out];
        );

        builder.component.continuous_assignments.extend(assigns);

        if spec.idx_lsb == 0 {
            let zero = builder.add_constant(0, global);
            let signature = &builder.component.signature;

            let [assign] = build_assignments!(builder;
                signature["arg"] = ? zero[out];
            );

            builder.component.continuous_assignments.push(assign);
        } else {
            structure!(builder;
                let slice = prim std_slice(global, spec.idx_lsb);
                let pad = prim std_pad(spec.idx_lsb, global);
            );

            let signature = &builder.component.signature;

            let assigns = build_assignments!(builder;
                slice[in_] = ? sub[out];
                pad[in_] = ? slice[out];
                signature["arg"] = ? pad[out];
            );

            builder.component.continuous_assignments.extend(assigns);
        }

        Ok(component)
    }
}
