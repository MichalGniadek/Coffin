use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use coffin2::*;
use std::{fs, path::PathBuf, process::exit};
use structopt::StructOpt;

/// Coffin compiler
#[derive(StructOpt, Debug, Clone)]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Output file
    #[structopt(parse(from_os_str))]
    output: Option<PathBuf>,

    /// Validate spirv output
    #[structopt(long, short)]
    validate: bool,

    /// Dissasemble spirv output
    #[structopt(long, short)]
    disassemble: bool,
}

fn main() {
    let opt = Opt::from_args();
    let spirv = match compile_file(&opt.input) {
        Ok(s) => s,
        Err(err) => {
            let file = SimpleFile::new(
                opt.input.file_name().unwrap().to_str().unwrap().to_string(),
                fs::read_to_string(opt.input).unwrap_or("".to_string()),
            );
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            term::emit(&mut writer.lock(), &config, &file, &err.report()).unwrap();
            exit(1);
        }
    };

    if opt.validate {
        validate_spirv(&spirv).unwrap();
    }

    let mut out = opt.output.clone().unwrap_or(opt.input.clone());
    out.set_extension("spv");

    write_spirv_binary(&spirv, &out).unwrap();

    if opt.disassemble {
        out.set_extension("spv_diss");
        write_spirv_diss(&spirv, &out).unwrap();
    }
}
