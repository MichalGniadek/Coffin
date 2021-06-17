#![feature(iter_intersperse)]
pub mod ast;
pub mod ast_span;
pub mod debug_print;
pub mod error;
pub mod lexer;
pub mod name_resolution;
pub mod parser;
pub mod spirv_generation;
pub mod type_resolution;

use assembler::{Assembler, DisassembleOptions};
use error::CoffinError;
use spirv_tools::{
    assembler,
    val::{self, Validator},
};
use std::{fs, path::PathBuf};

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
