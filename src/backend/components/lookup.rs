//! Coefficient lookup tables.

use std::{cmp, iter};

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::{CalyxResult, Error};

use super::{ComponentBuilder, ComponentManager};
use crate::backend::primitives::lut;
use crate::format::Format;
use crate::fpcore::ast::{Rational, Span};
use crate::functions::AddressSpec;
use crate::utils::mangling::{mangle, Mangle};

pub struct TableData<'a> {
    pub values: &'a [Vec<Rational>],
    pub formats: &'a [Format],
    pub spec: &'a dyn Mangle,
}

impl TableData<'_> {
    pub fn widths(&self) -> impl Iterator<Item = u32> + '_ {
        self.formats.iter().map(|format| format.width)
    }

    pub fn width(&self) -> u32 {
        self.widths().sum()
    }
}

pub struct LookupTable<'a> {
    pub data: TableData<'a>,
    pub format: &'a Format,
    pub spec: &'a AddressSpec,
    pub span: Span,
}

impl LookupTable<'_> {
    fn build_primitive(
        &self,
        lib: &mut ir::LibrarySignatures,
    ) -> CalyxResult<ir::Id> {
        let format_error = |post| {
            Error::misc("Implementation error")
                .with_pos(&self.span)
                .with_post_msg(Some(post))
        };

        let values: Vec<_> = self
            .data
            .values
            .iter()
            .map(|row| {
                itertools::process_results(
                    iter::zip(row, self.data.formats).map(|(value, format)| {
                        value.to_format(format).ok_or_else(|| {
                            format_error(format!(
                                "Generated constant {value} is not \
                                 representable in the given format"
                            ))
                        })
                    }),
                    |bits| lut::pack(bits, self.data.widths()),
                )
            })
            .collect::<CalyxResult<_>>()?;

        let name = ir::Id::new(mangle!(
            "lut",
            self.data.spec,
            self.data.formats,
            self.format,
            self.spec,
        ));

        let out_width = u64::from(self.data.width());

        let primitive =
            lut::compile_lut(name, self.spec.idx_width, out_width, &values);

        lib.add_inline_primitive(primitive).set_source();

        Ok(name)
    }
}

impl ComponentBuilder for LookupTable<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "lookup",
            self.data.spec,
            self.data.formats,
            self.format,
            self.spec,
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
                u64::from(self.data.width()),
                ir::Direction::Output,
                Default::default(),
            ),
            ir::PortDef::new(
                "arg",
                cmp::max(self.spec.idx_lsb, 1),
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
            let zero = builder.add_constant(0, 1);
            let signature = &builder.component.signature;

            let [assign] = build_assignments!(builder;
                signature["arg"] = ? zero[out];
            );

            builder.component.continuous_assignments.push(assign);
        } else {
            let msb = spec.idx_lsb - 1;

            structure!(builder;
                let high = prim std_bit_slice(global, msb, msb, 1);
                let com = prim std_not(1);
            );

            if spec.idx_lsb == 1 {
                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    signature["arg"] = ? com[out];
                );

                builder.component.continuous_assignments.extend(assigns);
            } else {
                structure!(builder;
                    let low = prim std_slice(global, msb);
                    let cat = prim std_cat(1, msb, spec.idx_lsb);
                );

                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    low[in_] = ? sub[out];
                    cat["left"] = ? com[out];
                    cat["right"] = ? low[out];
                    signature["arg"] = ? cat[out];
                );

                builder.component.continuous_assignments.extend(assigns);
            }
        }

        Ok(component)
    }
}
