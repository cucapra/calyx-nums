use argh::FromArgs;
use std::path::PathBuf;

/// Calyx back end for FPCore.
#[derive(FromArgs)]
pub struct Opts {
    /// input FPCore benchmark
    #[argh(positional)]
    pub file: PathBuf,
}

impl Opts {
    /// Parse options from `env::args`.
    pub fn parse() -> Opts {
        argh::from_env()
    }
}
