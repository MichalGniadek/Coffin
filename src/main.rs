use codespan_reporting::{
    files::{Files, SimpleFile},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use coffin2::{
    error::CoffinError, lexer, name_resolution, parser, spirv_generation, type_resolution,
};
use rspirv::binary::Disassemble;
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
}

fn main() {
    let opt = Opt::from_args();

    let src = match fs::read_to_string(&opt.input) {
        Ok(src) => src,
        Err(err) => {
            let file = SimpleFile::new("", "");
            let err = CoffinError::from(err);
            exit_with_errors(&file, &[err]);
        }
    };

    let lexer = lexer::lex(&src);
    let mut ast = parser::parse(lexer);
    let variables = name_resolution::visit(&mut ast);
    let types = type_resolution::visit(&mut ast, &variables);

    if !ast.errors.is_empty() {
        let file = SimpleFile::new(
            opt.input.to_str().unwrap_or("<Path is not valid UTF-8>"),
            src,
        );
        exit_with_errors(&file, &ast.errors);
    } else {
        let module = spirv_generation::visit(&mut ast, &variables, &types).unwrap();
        println!("{}", module.disassemble())
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
