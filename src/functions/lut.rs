//! Implementation of lookup tables as Verilog primitives.

use std::fmt::{LowerHex, Write};

use calyx_frontend::{Direction, PortDef, Primitive, Width};
use calyx_utils::Id;
use num::{BigUint, Zero};

/// Packs a sequence of values, each of the given width, into a single bit
/// vector. The first element of the sequence occupies the most-significant
/// position.
pub fn pack<I>(values: I, width: u64) -> BigUint
where
    I: IntoIterator<Item = BigUint>,
{
    values
        .into_iter()
        .fold(Zero::zero(), |acc, value| (acc << width) | value)
}

pub fn compile_lut<T: LowerHex>(name: Id, values: &[T]) -> Primitive {
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

fn format_body<T: LowerHex>(values: &[T]) -> String {
    let mut body = String::from("\n  always_comb begin\n    case (idx)\n");

    for (i, val) in values.iter().enumerate() {
        writeln!(body, "      'd{i} : out = 'h{val:x};").unwrap();
    }

    body.push_str("    endcase\n  end\n");

    body
}
