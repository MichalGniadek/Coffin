use codespan_reporting::{
    files::{Files, SimpleFile},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use coffin2::{
    debug_print, error::CoffinError, lexer, name_resolution, parser, spirv_generation,
    type_resolution, validate_spirv,
};
use rspirv::binary::{Assemble, Disassemble};
use std::{fs, path::PathBuf};
use structopt::StructOpt;

/// Coffin compiler. Use -h for help.
#[derive(StructOpt, Debug, Clone)]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,

    /// Optional output file
    #[structopt(parse(from_os_str))]
    #[allow(unused)]
    output: Option<PathBuf>,

    /// Validate spirv output
    #[structopt(long, short)]
    validate: bool,

    /// Dissasemble spirv output
    #[structopt(long, short)]
    disassemble: bool,

    /// Debug print the AST
    #[structopt(long, short)]
    print: bool,
}

fn main() {
    std::panic::set_hook(Box::new(|info| {
        println!("Internal compiler error: {}", info)
    }));

    let opt = Opt::from_args();

    let src = match fs::read_to_string(&opt.input) {
        Ok(src) => src,
        Err(err) => {
            exit_with_errors(&SimpleFile::new("", ""), &[CoffinError::IOError(err)]);
        }
    };

    let mut ast = parser::parse(lexer::lex(&src));
    let variables = name_resolution::visit(&mut ast);
    let types = type_resolution::visit(&mut ast, &variables);

    if opt.print {
        println!(
            "{}",
            debug_print::visit(&ast, false, Some(&variables), Some(&types))
        );
    }

    let module = match spirv_generation::visit(&mut ast, &variables, &types) {
        Ok(module) => module,
        Err(err) => {
            let file = SimpleFile::new(
                opt.input.to_str().unwrap_or("<Path is not valid UTF-8>"),
                src,
            );
            exit_with_errors(&file, &err);
        }
    };

    if opt.disassemble {
        println!("{}", module.disassemble());
    }

    let code = module.assemble();
    if opt.validate {
        if let Err(err) = validate_spirv(&code) {
            println!("Validator error: {}", err);
        }
    }
}

fn exit_with_errors<'f>(files: &'f impl Files<'f, FileId = ()>, errors: &[CoffinError]) -> ! {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    for err in errors {
        term::emit(&mut writer.lock(), &config, files, &err.report()).unwrap();
    }
    std::process::exit(1);
}
