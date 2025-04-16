mod builder;
mod compile;
mod components;
mod libm;
mod primitives;
mod stdlib;

use builder::IRBuilder;

pub use compile::{Program, compile_fpcore};
pub use stdlib::{ImportPaths, build_library};
