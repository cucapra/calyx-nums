//! FPCore front end.

pub mod ast;
mod constants;
mod literals;
pub mod metadata;
mod parser;

pub use parser::FPCoreParser;
