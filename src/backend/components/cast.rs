//! Numeric casts.

use calyx_ir as ir;
use calyx_utils::CalyxResult;

use super::{ComponentBuilder, ComponentManager};
use crate::format::Format;
use crate::utils::mangling::mangle;

pub struct Cast<'a> {
    pub from: &'a Format,
    pub to: &'a Format,
}

impl ComponentBuilder for Cast<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!("cast", self.from, self.to))
    }

    fn signature(&self) -> Vec<ir::PortDef<u64>> {
        vec![
            ir::PortDef::new(
                "in",
                u64::from(self.from.width),
                ir::Direction::Input,
                Default::default(),
            ),
            ir::PortDef::new(
                "out",
                u64::from(self.to.width),
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
        let ports = self.signature();

        let mut component = ir::Component::new(name, ports, false, true, None);
        let mut builder = ir::Builder::new(&mut component, lib).not_generated();

        let (msb_in, lsb_in) = self.from.vhdl();
        let (msb_out, lsb_out) = self.to.vhdl();

        let [in_, out] = ["in", "out"].map(ir::Id::new);

        let (cell, port, width) = if msb_out > msb_in {
            let in_width = u64::from(self.from.width);
            let out_width = in_width + msb_out.abs_diff(msb_in);

            let (prefix, prim) = if self.from.is_signed {
                ("ext", "std_signext")
            } else {
                ("pad", "std_pad")
            };

            let params = [in_width, out_width];
            let prim = builder.add_primitive(prefix, prim, &params);

            let signature = &builder.component.signature;

            builder.component.continuous_assignments.push(
                builder.build_assignment(
                    prim.borrow().get(in_),
                    signature.borrow().get(in_),
                    ir::Guard::True,
                ),
            );

            (prim, out, out_width)
        } else {
            let signature = &builder.component.signature;

            (signature.clone(), in_, self.from.width.into())
        };

        let (cell, port, width) = if lsb_out < lsb_in {
            let in_width = width;
            let out_width = in_width + lsb_in.abs_diff(lsb_out);

            let params = [in_width, out_width];
            let prim = builder.add_primitive("pad", "num_rpad", &params);

            builder.component.continuous_assignments.push(
                builder.build_assignment(
                    prim.borrow().get(in_),
                    cell.borrow().get(port),
                    ir::Guard::True,
                ),
            );

            (prim, out, out_width)
        } else {
            (cell, port, width)
        };

        let (cell, port) = if msb_out < msb_in || lsb_out > lsb_in {
            let in_width = width;
            let out_width = u64::from(self.to.width);

            let lsb = if lsb_out > lsb_in {
                lsb_out.abs_diff(lsb_in)
            } else {
                0
            };

            let params = [in_width, lsb, lsb + out_width - 1, out_width];
            let prim = builder.add_primitive("slice", "std_bit_slice", &params);

            builder.component.continuous_assignments.push(
                builder.build_assignment(
                    prim.borrow().get(in_),
                    cell.borrow().get(port),
                    ir::Guard::True,
                ),
            );

            (prim, out)
        } else {
            (cell, port)
        };

        let signature = &builder.component.signature;

        builder.component.continuous_assignments.push(
            builder.build_assignment(
                signature.borrow().get(out),
                cell.borrow().get(port),
                ir::Guard::True,
            ),
        );

        Ok(component)
    }
}
