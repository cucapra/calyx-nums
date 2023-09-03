//! Table lookup operations.

use calyx_ir::{self as ir, build_assignments, structure};
use calyx_utils::Id;

use super::addressing::AddressSpec;
use crate::format::Format;

/// Returns the signature for a table-lookup component.
pub fn signature(cols: u32, format: &Format) -> Vec<ir::PortDef<u64>> {
    vec![
        ir::PortDef::new(
            "in",
            format.width,
            ir::Direction::Input,
            Default::default(),
        ),
        ir::PortDef::new(
            "out",
            u64::from(cols) * format.width,
            ir::Direction::Output,
            Default::default(),
        ),
    ]
}

pub fn compile_lookup(
    name: Id,
    lut: Id,
    cols: u32,
    format: &Format,
    spec: &AddressSpec,
    lib: &ir::LibrarySignatures,
) -> ir::Component {
    let width = u64::from(cols) * format.width;

    let ports = signature(cols, format);

    let mut component = ir::Component::new(name, ports, false, true, None);
    let mut builder = ir::Builder::new(&mut component, lib);

    let prim = builder.add_primitive("lut", lut, &[width, spec.idx_width]);

    structure!(builder;
        let sub = prim std_sub(format.width);
        let rsh = prim std_rsh(format.width);
        let slice = prim std_slice(format.width, spec.idx_width);
        let left = constant(spec.subtrahend, format.width);
        let shift = constant(spec.idx_lsb, format.width);
    );

    let signature = &builder.component.signature;

    let assigns = Vec::from(build_assignments!(builder;
        sub["left"] = ? signature["in"];
        sub["right"] = ? left["out"];
        rsh["left"] = ? sub["out"];
        rsh["right"] = ? shift["out"];
        slice["in"] = ? rsh["out"];
        prim["idx"] = ? slice["out"];
        signature["out"] = ? prim["out"];
    ));

    builder.add_continuous_assignments(assigns);

    component
}
