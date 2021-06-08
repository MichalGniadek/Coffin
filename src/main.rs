use coffin2::*;
use std::{path::PathBuf, process::exit};
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
            show_err(&opt.input, err);
            exit(1);
        }
    };

    if opt.validate {
        validate_spirv(&spirv).unwrap();
    }

    let mut out = opt.output.clone().unwrap_or(opt.input.clone());
    out.set_extension("spv");

    // write_spirv_binary(&spirv, &out).unwrap();

    if opt.disassemble {
        out.set_extension("spv_diss");
        // write_spirv_diss(&spirv, &out).unwrap();
    }
}
