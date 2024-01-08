//! Implementation of lookup tables as Verilog primitives.

use std::fmt::{LowerHex, Write};

use calyx_ir as ir;
use num::{BigUint, Zero};

/// Packs a sequence of values, each of the given width, into a single bit
/// vector. The first element of the sequence occupies the most-significant
/// position.
pub fn pack<I>(values: I, width: u32) -> BigUint
where
    I: IntoIterator<Item = BigUint>,
{
    values
        .into_iter()
        .fold(Zero::zero(), |acc, value| (acc << width) | value)
}

pub fn compile_lut<T: LowerHex>(name: ir::Id, values: &[T]) -> ir::Primitive {
    let width = ir::Id::new("WIDTH");
    let idx_size = ir::Id::new("IDX_SIZE");

    let mut data = ir::Attributes::default();
    let mut share = ir::Attributes::default();

    data.insert(ir::Attribute::Bool(ir::BoolAttr::Data), 1);
    share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

    ir::Primitive {
        name,
        params: vec![width, idx_size],
        signature: vec![
            ir::PortDef::new(
                "idx",
                ir::Width::Param { value: idx_size },
                ir::Direction::Input,
                data,
            ),
            ir::PortDef::new(
                "out",
                ir::Width::Param { value: width },
                ir::Direction::Output,
                Default::default(),
            ),
        ],
        attributes: share,
        is_comb: true,
        latency: None,
        body: Some(format_body(values)),
    }
}

fn format_body<T: LowerHex>(values: &[T]) -> String {
    let mut body = String::from("always_comb begin\n    case (idx)\n");

    for (i, val) in values.iter().enumerate() {
        writeln!(body, "      'd{i} : out = 'h{val:x};").unwrap();
    }

    body.push_str("    endcase\n  end");

    body
}
