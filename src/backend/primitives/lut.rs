//! Implementation of lookup tables as Verilog primitives.

use std::fmt::{LowerHex, Write};
use std::iter;

use calyx_ir as ir;
use malachite::num::basic::traits::Zero;
use malachite::Natural;

/// Packs a sequence of values into a single bit vector. The first element of
/// the sequence occupies the most-significant position.
pub fn pack<V, W>(values: V, widths: W) -> Natural
where
    V: IntoIterator<Item = Natural>,
    W: IntoIterator<Item = u32>,
{
    iter::zip(values, widths)
        .fold(Natural::ZERO, |acc, (value, width)| (acc << width) | value)
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
