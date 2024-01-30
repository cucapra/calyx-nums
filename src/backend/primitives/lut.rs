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

pub fn compile_lut<T: LowerHex>(
    name: ir::Id,
    idx_width: u64,
    out_width: u64,
    values: &[T],
) -> ir::Primitive {
    let mut data = ir::Attributes::default();
    let mut share = ir::Attributes::default();

    data.insert(ir::Attribute::Bool(ir::BoolAttr::Data), 1);
    share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

    ir::Primitive {
        name,
        params: vec![],
        signature: vec![
            ir::PortDef::new(
                "idx",
                ir::Width::Const { value: idx_width },
                ir::Direction::Input,
                data,
            ),
            ir::PortDef::new(
                "out",
                ir::Width::Const { value: out_width },
                ir::Direction::Output,
                Default::default(),
            ),
        ],
        attributes: share,
        is_comb: true,
        latency: None,
        body: Some(format_body(idx_width, out_width, values)),
    }
}

fn format_body<T: LowerHex>(
    idx_width: u64,
    out_width: u64,
    values: &[T],
) -> String {
    let mut body = String::from(concat!(
        "always_comb begin\n",
        "    unique case (idx)\n",
    ));

    for (i, val) in values.iter().enumerate() {
        writeln!(body, "      {idx_width}'d{i}: out = {out_width}'h{val:x};")
            .unwrap();
    }

    body.push_str(concat!(
        "      default out = 'x;\n",
        "    endcase\n",
        "  end",
    ));

    body
}
