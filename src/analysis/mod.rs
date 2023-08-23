mod context;
mod domains;
mod passes;
mod type_check;

pub use context::{Binding, ContextResolution};
pub use domains::DomainInference;
pub use type_check::{Type, TypeCheck};

pub use passes::PassManager;
