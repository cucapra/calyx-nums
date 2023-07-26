use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;

use calyx_frontend::{parser::CalyxParser, Workspace};
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use calyx_nums::fpcore::FPCoreParser;
use calyx_nums::irgen;
use calyx_nums::opts::Opts;

const STD_IMPORTS: &[&str] = &[
    irgen::stdlib::compile::IMPORT,
    irgen::stdlib::core::IMPORT,
    irgen::stdlib::binary_operators::IMPORT,
    irgen::stdlib::math::IMPORT,
];

fn build_workspace(
    imports: &[&str],
    lib_path: &Path,
) -> CalyxResult<Workspace> {
    let mut workspace = Workspace::default();
    let mut imported = HashSet::new();

    let mut deps: Vec<_> =
        imports.iter().map(|import| lib_path.join(import)).collect();

    while let Some(dep) = deps.pop() {
        if imported.contains(&dep) {
            continue;
        }

        let parent = dep.parent().unwrap();
        let namespace = CalyxParser::parse_file(&dep)?;

        let next = workspace
            .merge_namespace(namespace, false, parent, true, lib_path)?;

        deps.extend(next.into_iter().map(|(source, _)| source));
        imported.insert(dep);
    }

    Ok(workspace)
}

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

    let workspace = build_workspace(STD_IMPORTS, &opts.lib_path)?;

    let ctx = irgen::compile_fpcore(&benchmarks, &opts.format, workspace.lib)?;

    let mut out: Box<dyn Write> = if let Some(path) = opts.output {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    for import in STD_IMPORTS {
        writeln!(out, "import \"{}\";", import)?;
    }

    ir::Printer::write_context(&ctx, true, &mut out)?;

    Ok(())
}
