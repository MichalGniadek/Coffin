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
    let opt = Opt::from_args();

    let src = match fs::read_to_string(&opt.input) {
        Ok(src) => src,
        Err(err) => {
            let file = SimpleFile::new("", "");
            exit_with_errors(&file, &[CoffinError::IOError(err)]);
        }
    };

    let lexer = lexer::lex(&src);
    let mut ast = parser::parse(lexer);
    let variables = name_resolution::visit(&mut ast);
    let types = type_resolution::visit(&mut ast, &variables);

    if opt.print {
        println!(
            "{}",
            debug_print::visit(&ast, true, false, Some(&variables), Some(&types))
        );
    }

    let module = if !ast.errors.is_empty() {
        let file = SimpleFile::new(
            opt.input.to_str().unwrap_or("<Path is not valid UTF-8>"),
            src,
        );
        exit_with_errors(&file, &ast.errors);
    } else {
        match spirv_generation::visit(&mut ast, &variables, &types) {
            Ok(module) => module,
            Err(_) => {
                let file = SimpleFile::new(
                    opt.input.to_str().unwrap_or("<Path is not valid UTF-8>"),
                    src,
                );
                exit_with_errors(&file, &ast.errors);
            }
        }
    };

    if opt.disassemble {
        println!("{}", module.disassemble());
    }

    let code = module.assemble();
    if opt.validate {
        if let Err(err) = validate_spirv(&code) {
            println!(
                "Internal compiler validator error, this is a bug please report it.\n{}",
                err
            );
        }
    }
}

fn exit_with_errors<'f, F>(files: &'f F, errors: &[CoffinError]) -> !
where
    F: Files<'f, FileId = ()>,
{
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    for err in errors {
        term::emit(&mut writer.lock(), &config, files, &err.report()).unwrap();
    }
    std::process::exit(1);
}
