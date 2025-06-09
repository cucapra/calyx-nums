mod builder;
mod compile;
mod components;
mod libm;
mod stdlib;

use builder::IrBuilder;
use stdlib::Import;

pub use compile::{Program, compile_fpcore};
pub use stdlib::{ImportPaths, build_library};
