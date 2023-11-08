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

        let primitive = lut::compile_lut(name, &values);

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
                self.format.width,
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.degree + 1) * self.format.width,
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
        let mut builder = ir::Builder::new(&mut component, lib);

        let width = u64::from(self.degree + 1) * self.format.width;
        let primitive =
            builder.add_primitive("lut", lut, &[width, self.spec.idx_width]);

        structure!(builder;
            let sub = prim std_sub(self.format.width);
            let rsh = prim std_rsh(self.format.width);
            let slice = prim std_slice(self.format.width, self.spec.idx_width);
            let left = constant(self.spec.subtrahend, self.format.width);
            let shift = constant(self.spec.idx_lsb, self.format.width);
        );

        let signature = &builder.component.signature;

        let assigns = build_assignments!(builder;
            sub["left"] = ? signature["in"];
            sub["right"] = ? left["out"];
            rsh["left"] = ? sub["out"];
            rsh["right"] = ? shift["out"];
            slice["in"] = ? rsh["out"];
            primitive["idx"] = ? slice["out"];
            signature["out"] = ? primitive["out"];
        );

        builder.component.continuous_assignments.extend(assigns);

        Ok(component)
    }
}
