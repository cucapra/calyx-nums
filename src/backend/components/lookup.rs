//! Coefficient lookup tables.

use std::{cmp, iter};

use calyx_ir::{self as ir, build_assignments, structure};

use super::{ComponentBuilder, ComponentManager};
use crate::approx::AddressSpec;
use crate::backend::IRBuilder;
use crate::backend::primitives::lut;
use crate::fpcore::ast::{Rational, Span};
use crate::utils::mangling::{Mangle, mangle};
use crate::utils::rational::FixedPoint;
use crate::utils::{Diagnostic, Format};

pub struct TableData<'a> {
    pub values: &'a [Vec<Rational>],
    pub formats: &'a [Format],
    pub spec: &'a dyn Mangle,
}

impl TableData<'_> {
    pub fn widths(&self) -> impl Iterator<Item = u32> {
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
    ) -> Result<ir::Id, Diagnostic> {
        let diagnostic = |value| {
            Diagnostic::error()
                .with_message("implementation error")
                .with_secondary(self.span, "while compiling this operator")
                .with_note(format!(
                    "generated constant {value} overflows the target format"
                ))
        };

        let values: Vec<_> = self
            .data
            .values
            .iter()
            .map(|row| {
                itertools::process_results(
                    iter::zip(row, self.data.formats).map(|(value, format)| {
                        value
                            .to_fixed_point(format)
                            .ok_or_else(|| diagnostic(value))
                    }),
                    |bits| lut::pack(bits, self.data.widths()),
                )
            })
            .collect::<Result<_, _>>()?;

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
    ) -> Result<ir::Component, Diagnostic> {
        let lut = self.build_primitive(lib)?;
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, false, true, None);
        let mut builder = IRBuilder::new(&mut component, lib);

        let primitive = builder.add_primitive("lut", lut, &[]);

        let width_error = |_| {
            Diagnostic::error()
                .with_message("code generation failed")
                .with_secondary(self.span, "while compiling this operator")
                .with_note("calyx doesn't support constants wider than 64 bits")
        };

        let global = u64::from(self.format.width);
        let left = u64::try_from(&self.spec.subtrahend).map_err(width_error)?;

        structure!(builder;
            let sub = prim std_sub(global);
            let slice = prim std_bit_slice(
                global,
                self.spec.idx_lsb,
                self.spec.idx_lsb + self.spec.idx_width - 1,
                self.spec.idx_width
            );
            let left = constant(left, global);
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

        builder.add_continuous_assignments(assigns);

        if self.spec.idx_lsb == 0 {
            let zero = builder.add_constant(0, 1);
            let signature = &builder.component.signature;

            let [assign] = build_assignments!(builder;
                signature["arg"] = ? zero[out];
            );

            builder.add_continuous_assignment(assign);
        } else {
            let msb = self.spec.idx_lsb - 1;

            structure!(builder;
                let high = prim std_bit_slice(global, msb, msb, 1);
                let com = prim std_not(1);
            );

            if self.spec.idx_lsb == 1 {
                let signature = &builder.component.signature;

                let assigns = build_assignments!(builder;
                    high[in_] = ? sub[out];
                    com[in_] = ? high[out];
                    signature["arg"] = ? com[out];
                );

                builder.add_continuous_assignments(assigns);
            } else {
                structure!(builder;
                    let low = prim std_slice(global, msb);
                    let cat = prim std_cat(1, msb, self.spec.idx_lsb);
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

                builder.add_continuous_assignments(assigns);
            }
        }

        Ok(component)
    }
}
