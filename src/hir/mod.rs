mod context;
mod index;
mod ir;
mod lowering;
pub mod visitor;

pub mod arena {
    pub use cranelift_entity::{packed_option::*, *};
}

pub use arena::{EntityList, PackedOption};
pub use context::{Context, Metadata, Pool};
pub use index::*;
pub use ir::*;
pub use lowering::lower_ast;
pub use visitor::Visitor;
