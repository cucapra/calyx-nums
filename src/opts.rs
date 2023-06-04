use argh::FromArgs;
use std::path::PathBuf;

use crate::format::Format;

/// Calyx back end for FPCore.
#[derive(FromArgs)]
pub struct Opts {
    /// input FPCore benchmark
    #[argh(positional)]
    pub file: PathBuf,

    /// path to the primitives library
    #[argh(option, short = 'l')]
    pub lib_path: PathBuf,

    /// output file
    #[argh(option, short = 'o')]
    pub output: Option<PathBuf>,

    /// global numeric format
    #[argh(option, default = "Default::default()")]
    pub format: Format,
}

impl Opts {
    /// Parse options from `env::args`.
    pub fn parse() -> Opts {
        argh::from_env()
    }
}
