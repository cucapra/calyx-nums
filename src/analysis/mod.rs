//! Semantic analysis.

mod bindings;
mod call_graph;
mod domain;
mod passes;
mod ranges;
mod type_check;

pub use bindings::{Binding, NameResolution};
pub use call_graph::CallGraph;
pub use domain::Precondition;
pub use ranges::RangeAnalysis;
pub use type_check::{Type, TypeCheck};

pub use passes::PassManager;
