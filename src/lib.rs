#![feature(iter_intersperse)]
pub mod ast;
pub mod ast_span;
pub mod error;
pub mod lexer;
pub mod name_resolution;
pub mod parser;
pub mod debug_print;
pub mod type_resolution;

use assembler::{Assembler, DisassembleOptions};
use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use error::CoffinError;
use lexer::Token;
use logos::Logos;
use name_resolution::NameResolution;
use spirv_tools::{
    assembler,
    val::{self, Validator},
};
use std::{fs, path::PathBuf};

pub fn show_err(path: &PathBuf, err: CoffinError) {
    let file = SimpleFile::new(
        path.to_str().unwrap(),
        fs::read_to_string(path).unwrap_or("".to_string()),
    );
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();
    term::emit(&mut writer.lock(), &config, &file, &err.report()).unwrap();
}

pub fn compile_file(path: &PathBuf) -> Result<Vec<u32>, CoffinError> {
    let code = fs::read_to_string(path)?;
    let lexer = Token::lexer(&code);
    let (ast, _spans, _rodeo) = parser::parse(lexer);
    let _vars = NameResolution::visit(&ast);
    // let _print = pretty_print::PrettyPrint::new();
    // for err in errors{
    //     show_err(path, err.into());
    // }
    // println!(
    //     "{}",
    //     pretty_print::PrettyPrint::new().visit_expr(&ast.temp_root)
    // );
    // let spirv_gen = SpirvGen::visit_root(scanner, ast);

    let spirv = Vec::<u32>::new();
    // spirv_gen.write_spirv(&mut spirv);

    Ok(spirv)
}

pub fn write_spirv_binary(spirv: &Vec<u32>, path: &PathBuf) -> Result<(), CoffinError> {
    let (head, body, tail) = unsafe { spirv.align_to::<u8>() };
    // Make sure alignment is correct.
    assert!(head.is_empty() && tail.is_empty());
    fs::write(path, body).unwrap();
    Ok(())
}

pub fn write_spirv_diss(spirv: &Vec<u32>, path: &PathBuf) -> Result<(), CoffinError> {
    let ass = assembler::create(Some(spirv_tools::TargetEnv::Universal_1_5));
    let diss = ass
        .disassemble(spirv, DisassembleOptions::default())
        .unwrap();
    fs::write(path, diss).unwrap();
    Ok(())
}

pub fn validate_spirv(spirv: &Vec<u32>) -> Result<(), CoffinError> {
    let val = val::create(None);
    match val.validate(spirv, Some(val::ValidatorOptions::default())) {
        Ok(_) => {}
        Err(e) => println!("{}", e),
    }
    Ok(())
}
