//! General part-select.

use calyx_ir as ir;

const BODY: &str = "assign out = in[OUT_WIDTH+BEGIN_INDEX-1:BEGIN_INDEX];";

pub fn compile_select(name: ir::Id) -> ir::Primitive {
    let in_width = ir::Id::new("IN_WIDTH");
    let out_width = ir::Id::new("OUT_WIDTH");
    let begin_idx = ir::Id::new("BEGIN_INDEX");

    let mut data = ir::Attributes::default();
    let mut share = ir::Attributes::default();

    data.insert(ir::Attribute::Bool(ir::BoolAttr::Data), 1);
    share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

    ir::Primitive {
        name,
        params: vec![in_width, out_width, begin_idx],
        signature: vec![
            ir::PortDef::new(
                "in",
                ir::Width::Param { value: in_width },
                ir::Direction::Input,
                data,
            ),
            ir::PortDef::new(
                "out",
                ir::Width::Param { value: out_width },
                ir::Direction::Output,
                Default::default(),
            ),
        ],
        attributes: share,
        is_comb: true,
        latency: None,
        body: Some(String::from(BODY)),
    }
}
