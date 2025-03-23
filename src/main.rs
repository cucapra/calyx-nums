use std::borrow::Cow;
use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, Write};
use std::iter;
use std::path::PathBuf;
use std::process::ExitCode;

use calyx_frontend::{Workspace, parser::CalyxParser};
use calyx_ir as ir;
use calyx_utils::{CalyxResult, Error};

use calyx_nums::backend;
use calyx_nums::fpcore::{FPCoreParser, ast::Span};
use calyx_nums::opts::Opts;
use calyx_nums::utils::{Diagnostic, Reporter};

const IMPORTS: &[&str] = &[
    backend::stdlib::compile::IMPORT,
    backend::stdlib::core::IMPORT,
    backend::stdlib::binary_operators::IMPORT,
    backend::stdlib::math::IMPORT,
    backend::stdlib::numbers::IMPORT,
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

fn read_input(file: &Option<PathBuf>) -> io::Result<(Cow<'_, str>, String)> {
    match file {
        Some(file) => {
            let filename = file.to_string_lossy();
            let src = fs::read_to_string(file)?;

            Ok((filename, src))
        }
        None => {
            let filename = Cow::from("<stdin>");
            let src = io::read_to_string(io::stdin())?;

            Ok((filename, src))
        }
    }
}

fn write_output(
    imports: &[String],
    ctx: &ir::Context,
    file: &Option<PathBuf>,
) -> io::Result<()> {
    let mut out: Box<dyn Write> = if let Some(path) = file {
        Box::new(File::create(path)?)
    } else {
        Box::new(io::stdout())
    };

    for import in imports {
        writeln!(out, "import \"{}\";", import)?;
    }

    ir::Printer::write_context(ctx, true, &mut out)
}

fn main() -> ExitCode {
    let opts = Opts::parse();

    let (filename, src) = match read_input(&opts.file) {
        Ok(result) => result,
        Err(err) => {
            Reporter::new("", "").emit(&Diagnostic::from(err));

            return ExitCode::FAILURE;
        }
    };

    let mut rpt = Reporter::new(&filename, &src);

    let defs = match FPCoreParser::parse_file(&src) {
        Ok(result) => result,
        Err(err) => {
            rpt.emit(
                &Diagnostic::error()
                    .with_message("syntax error")
                    .with_primary(
                        Span::from(err.location),
                        err.variant.message(),
                    ),
            );

            return ExitCode::FAILURE;
        }
    };

    let (lib, imports) = match build_workspace(IMPORTS, &opts.lib_path) {
        Ok(workspace) => (workspace.lib, workspace.original_imports),
        Err(err) => {
            eprintln!("error: {err:?}");

            return ExitCode::FAILURE;
        }
    };

    let Some(ctx) = backend::compile_fpcore(&defs, &opts, &mut rpt, lib) else {
        return ExitCode::FAILURE;
    };

    if let Err(err) = write_output(&imports, &ctx, &opts.output) {
        rpt.emit(&Diagnostic::from(err));

        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
