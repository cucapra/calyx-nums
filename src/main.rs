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

    let file = fs::read_to_string(&opts.file)?;

    let benchmarks = FPCoreParser::parse_file(&file).map_err(|err| {
        let path = opts.file.to_string_lossy();

        Error::misc(format!(
            "Failed to parse `{}`:\n{}",
            path,
            err.with_path(&path)
        ))
    })?;

    let workspace = Workspace::construct(
        &Some(opts.lib_path.join(irgen::stdlib::math::IMPORT)),
        &opts.lib_path,
    )?;

    let lib = ir::LibrarySignatures::from(workspace.externs);

    let ctx = irgen::compile_fpcore(&benchmarks, &opts.format, lib)?;

    let mut out: Box<dyn Write> = if let Some(path) = opts.output {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    ir::Printer::write_context(&ctx, false, &mut out)?;

    Ok(())
}
