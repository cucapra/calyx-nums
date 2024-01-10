//! General part-select.

use calyx_ir as ir;

use super::NamedPrimitive;

pub struct PartSelect;

impl NamedPrimitive for PartSelect {
    fn name() -> ir::Id {
        ir::Id::new("select")
    }

    fn build() -> ir::Primitive {
        let in_width = ir::Id::new("IN_WIDTH");
        let out_width = ir::Id::new("OUT_WIDTH");
        let lsb = ir::Id::new("LSB");

        let mut data = ir::Attributes::default();
        let mut share = ir::Attributes::default();

        data.insert(ir::Attribute::Bool(ir::BoolAttr::Data), 1);
        share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

        ir::Primitive {
            name: Self::name(),
            params: vec![in_width, out_width, lsb],
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
            body: Some(String::from("assign out = in[LSB+OUT_WIDTH-1:LSB];")),
        }
    }
}
