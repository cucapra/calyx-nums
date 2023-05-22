use calyxlibm::fpcore::FPCoreParser;
use calyxlibm::opts::Opts;
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let opts = Opts::parse();
    let file = fs::read_to_string(&opts.file)?;

    match FPCoreParser::parse_file(&file) {
        Ok(benchmarks) => println!("{:?}", benchmarks),
        Err(err) => eprintln!(
            "Failed to parse `{}`:\n{}",
            opts.file.to_string_lossy(),
            err
        ),
    }

    Ok(())
}
