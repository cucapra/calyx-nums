//! Implementation of lookup tables as Verilog primitives.

use std::fmt::Write;

use calyx_frontend::{Direction, PortDef, Primitive, Width};
use calyx_utils::Id;

pub fn compile_lut(name: Id, values: &[u64]) -> Primitive {
    let width = Id::new("WIDTH");
    let idx_size = Id::new("IDX_SIZE");

    Primitive {
        name,
        params: vec![width, idx_size],
        signature: vec![
            PortDef {
                name: Id::new("idx"),
                width: Width::Param { value: idx_size },
                direction: Direction::Input,
                attributes: Default::default(),
            },
            PortDef {
                name: Id::new("out"),
                width: Width::Param { value: width },
                direction: Direction::Output,
                attributes: Default::default(),
            },
        ],
        attributes: Default::default(),
        is_comb: true,
        latency: None,
        body: Some(format_body(values)),
    }
}

fn format_body(values: &[u64]) -> String {
    let mut body = String::from("\n  always_comb begin\n    case (idx)\n");

    for (i, val) in values.iter().enumerate() {
        writeln!(body, "      'd{i} : out = 'h{val:x};").unwrap();
    }

    body.push_str("    endcase\n  end\n");

    body
}
