mod ast;
mod error;
mod lexer;
mod parser;
mod pretty_print;

use assembler::{Assembler, DisassembleOptions};
use error::CoffinError;
use lexer::Token;
use logos::Logos;
use spirv_tools::{
    assembler,
    val::{self, Validator},
};
use std::{fs, path::PathBuf};

pub fn compile_file(path: &PathBuf) -> Result<Vec<u32>, CoffinError> {
    let code = fs::read_to_string(path)?;
    let lexer = Token::lexer(&code);
    let (_ast, errors) = parser::parse(lexer);
    let _print = pretty_print::PrettyPrint::new();
    println!("{:?}", errors);
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
