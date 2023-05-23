use calyxlibm::fpcore::FPCoreParser;
use calyxlibm::opts::Opts;
use std::fs::{self, File};
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let opts = Opts::parse();
    let file = fs::read_to_string(&opts.file)?;

    let benchmarks = match FPCoreParser::parse_file(&file) {
        Ok(benchmarks) => benchmarks,
        Err(err) => {
            eprintln!(
                "Failed to parse `{}`:\n{}",
                opts.file.to_string_lossy(),
                err
            );
            return Ok(());
        }
    };

    if let Some(path) = opts.output {
        let mut file = File::create(path)?;
        writeln!(file, "{:?}", benchmarks)?;
    } else {
        println!("{:?}", benchmarks)
    }

    Ok(())
}
