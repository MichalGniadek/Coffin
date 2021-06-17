use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use coffin2::{
    error::CoffinError, lexer, name_resolution, parser, spirv_generation, type_resolution,
};
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
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            let err = CoffinError::from(err);
            term::emit(&mut writer.lock(), &config, &file, &err.report()).unwrap();
            std::process::exit(1);
        }
    };

    let lexer = lexer::lex(&src);
    let (ast, spans, resolver, parser_errors) = parser::parse(lexer);
    let (variables, name_errors) = name_resolution::visit(&ast, &spans);
    let (types, type_errors) = type_resolution::visit(&ast, &variables, &resolver, &spans);

    let mut errors = vec![];
    errors.extend(parser_errors);
    errors.extend(name_errors);
    errors.extend(type_errors);

    if !errors.is_empty() {
        let file = SimpleFile::new(
            opt.input.to_str().unwrap_or("Path is not valid UTF-8."),
            src,
        );
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = Config::default();
        for err in errors {
            term::emit(&mut writer.lock(), &config, &file, &err.report()).unwrap();
        }
        std::process::exit(1);
    } else {
        let (_module, _errors) = spirv_generation::visit(&ast, &variables, &types, &spans);
        // println!("{}", module.disassemble())
    }

    // if opt.validate {
    //     validate_spirv(&spirv).unwrap();
    // }

    // let mut out = opt.output.clone().unwrap_or(opt.input.clone());
    // out.set_extension("spv");

    // // write_spirv_binary(&spirv, &out).unwrap();

    // if opt.disassemble {
    //     out.set_extension("spv_diss");
    //     // write_spirv_diss(&spirv, &out).unwrap();
    // }
}
