use std::path::PathBuf;

use argh::FromArgs;
use log::LevelFilter;

use crate::format::Format;

/// FPCore frontend for Calyx.
#[derive(FromArgs)]
pub struct Opts {
    /// input file
    #[argh(positional)]
    pub file: Option<PathBuf>,

    /// add directory to library search path
    #[argh(option, short = 'l')]
    pub lib_path: Vec<PathBuf>,

    /// output file
    #[argh(option, short = 'o')]
    pub output: Option<PathBuf>,

    /// global numeric format
    #[argh(option, default = "Default::default()")]
    pub format: Format,

    /// enable domain inference
    #[argh(switch)]
    pub infer_domains: bool,

    /// logging level
    #[argh(option, long = "log", default = "LevelFilter::Warn")]
    pub log_level: LevelFilter,
}

impl Opts {
    /// Parse options from `env::args`.
    pub fn parse() -> Opts {
        argh::from_env()
    }
}
