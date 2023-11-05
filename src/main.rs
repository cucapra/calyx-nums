use std::borrow::Cow;
use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;

use calyx_frontend::{parser::CalyxParser, Workspace};
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use calyx_nums::backend;
use calyx_nums::fpcore::FPCoreParser;
use calyx_nums::opts::Opts;

const STD_IMPORTS: &[&str] = &[
    backend::stdlib::compile::IMPORT,
    backend::stdlib::core::IMPORT,
    backend::stdlib::binary_operators::IMPORT,
    backend::stdlib::math::IMPORT,
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
        .filter_level(opts.log_level)
        .target(env_logger::Target::Stderr)
        .init();

    let (filename, src) = match &opts.file {
        Some(file) => {
            let filename = file.to_string_lossy();
            let src = fs::read_to_string(file)?;

            (filename, src)
        }
        None => {
            let filename = Cow::from("<stdin>");
            let src = io::read_to_string(io::stdin())?;

            (filename, src)
        }
    };

    let benchmarks = FPCoreParser::parse_file(filename.to_string(), src)
        .map_err(|err| {
            Error::misc(format!("Syntax error:\n{}", err.with_path(&filename)))
        })?;

    let workspace = build_workspace(STD_IMPORTS, &opts.lib_path)?;
    let ctx = backend::compile_fpcore(&benchmarks, &opts, workspace.lib)?;

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
