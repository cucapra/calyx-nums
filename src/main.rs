use std::fs::{self, File};
use std::io::{self, Write};

use calyx_frontend::Workspace;
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use calyx_nums::fpcore::FPCoreParser;
use calyx_nums::irgen;
use calyx_nums::opts::Opts;

fn main() -> CalyxResult<()> {
    let opts = Opts::parse();

    env_logger::Builder::new()
        .format_timestamp(None)
        .filter_level(log::LevelFilter::Warn)
        .target(env_logger::Target::Stderr)
        .init();

    let path = opts.file.to_string_lossy();
    let file = fs::read_to_string(&opts.file)?;

    let benchmarks =
        FPCoreParser::parse_file(path.to_string(), file).map_err(|err| {
            Error::misc(format!("Syntax error:\n{}", err.with_path(&path)))
        })?;

    let workspace = Workspace::construct(
        &Some(opts.lib_path.join(irgen::stdlib::math::IMPORT)),
        &opts.lib_path,
    )?;

    let ctx =
        irgen::compile_fpcore(&benchmarks, &opts.format, workspace.externs)?;

    let mut out: Box<dyn Write> = if let Some(path) = opts.output {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    ir::Printer::write_context(&ctx, false, &mut out)?;

    Ok(())
}
