pub mod ast;
pub mod ast_span;
pub mod debug_print;
pub mod error;
pub mod lexer;
pub mod name_resolution;
pub mod parser;
pub mod spirv_generation;
pub mod type_id;
pub mod type_resolution;

use assembler::{Assembler, DisassembleOptions};
use error::CoffinError;
use rspirv::binary::Assemble;
use spirv_tools::{
    assembler,
    val::{self, Validator},
};
use std::{fs, path::Path};

pub use spirv_tools::error::Error as ValidatorError;

pub fn compile_file(path: &Path) -> Result<Vec<u32>, Vec<CoffinError>> {
    let src = match fs::read_to_string(path) {
        Ok(src) => src,
        Err(err) => {
            return Err(vec![err.into()]);
        }
    };

    let lexer = lexer::lex(&src);
    let mut ast = parser::parse(lexer);
    let variables = name_resolution::visit(&mut ast);
    let types = type_resolution::visit(&mut ast, &variables);

    match spirv_generation::visit(&mut ast, &variables, &types) {
        Ok(module) => Ok(module.assemble()),
        Err(_) => Err(ast.errors),
    }
}

pub fn get_disassembly(spirv: &[u32]) -> Result<String, spirv_tools::error::Error> {
    let ass = assembler::create(Some(spirv_tools::TargetEnv::Universal_1_5));
    ass.disassemble(spirv, DisassembleOptions::default())
}

pub fn validate_spirv(spirv: &[u32]) -> Result<(), ValidatorError> {
    let val = val::create(None);
    val.validate(spirv, Some(val::ValidatorOptions::default()))
}
