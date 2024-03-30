mod bindings;
mod passes;
mod ranges;
mod type_check;

pub use bindings::{Binding, NameResolution};
pub use ranges::RangeAnalysis;
pub use type_check::{Type, TypeCheck};

pub use passes::PassManager;
