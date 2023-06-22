//! FPCore front end.

pub mod ast;
mod constants;
mod literals;
pub mod metadata;
mod parser;
pub mod visitor;

pub use parser::FPCoreParser;
pub use visitor::Visitor;
