pub mod lut;
mod select;

pub use select::PartSelect;

use calyx_ir as ir;

pub trait NamedPrimitive {
    fn name() -> ir::Id;

    fn build() -> ir::Primitive;

    fn add(lib: &mut ir::LibrarySignatures) -> ir::Id {
        let name = Self::name();

        if lib.find_primitive(name).is_none() {
            lib.add_inline_primitive(Self::build()).set_source();
        }

        name
    }
}
