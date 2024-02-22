use std::borrow::Cow;
use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, Write};
use std::iter;
use std::path::PathBuf;

use calyx_frontend::{parser::CalyxParser, Workspace};
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use calyx_nums::backend;
use calyx_nums::fpcore::FPCoreParser;
use calyx_nums::opts::Opts;

const NUMERICS_LIB: &str = "primitives/numbers.futil";

const IMPORTS: &[&str] = &[
    backend::stdlib::compile::IMPORT,
    backend::stdlib::core::IMPORT,
    backend::stdlib::binary_operators::IMPORT,
    backend::stdlib::math::IMPORT,
    NUMERICS_LIB,
];

fn build_workspace(
    imports: &[&str],
    search_paths: &[PathBuf],
) -> CalyxResult<Workspace> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let mut stack: Vec<_> = imports
        .iter()
        .map(|import| {
            itertools::chain(search_paths, iter::once(&root))
                .find_map(|lib_path| {
                    let import = lib_path.join(import).canonicalize().ok()?;

                    import.exists().then_some((import, lib_path))
                })
                .ok_or(Error::invalid_file(format!(
                    "Unresolved import `{import}`"
                )))
        })
        .collect::<CalyxResult<_>>()?;

    let mut workspace = Workspace::default();
    let mut imported = HashSet::new();

    workspace.original_imports = stack
        .iter()
        .map(|(import, _)| import.to_string_lossy().into_owned())
        .collect();

    while let Some((import, lib_path)) = stack.pop() {
        if imported.contains(&import) {
            continue;
        }

        let namespace = CalyxParser::parse_file(&import)?;
        let parent = import.parent().unwrap();

        let next = workspace
            .merge_namespace(namespace, false, parent, true, lib_path)?;

        stack.extend(next.into_iter().map(|(import, _)| (import, lib_path)));
        imported.insert(import);
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
            Error::misc(format!("Syntax error\n{}", err.with_path(&filename)))
        })?;

    let workspace = build_workspace(IMPORTS, &opts.lib_path)?;
    let ctx = backend::compile_fpcore(&benchmarks, &opts, workspace.lib)?;

    let mut out: Box<dyn Write> = if let Some(path) = opts.output {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    for import in workspace.original_imports {
        writeln!(out, "import \"{}\";", import)?;
    }

    ir::Printer::write_context(&ctx, true, &mut out)?;

    Ok(())
}
