use coffin2::{
    ast::Ast,
    debug_print::DebugPrint,
    lexer::Token,
    name_resolution::NameResolution,
    parser::{self, spans_table::SpansTable},
};
use insta::{assert_snapshot, glob};
use lasso::RodeoResolver;
use logos::Logos;
use std::{fs, path::Path};

fn get_ast(path: &Path) -> (Ast, SpansTable, RodeoResolver) {
    let code = fs::read_to_string(path).unwrap();
    let lexer = Token::lexer(&code);
    parser::parse(lexer)
}

#[test]
fn insta() {
    glob!(r"insta_parser\*.coff", |path| {
        let (ast, _, rodeo) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(&ast, Some(&rodeo), None, None));
    });
    glob!(r"insta_parser_spans\*.coff", |path| {
        let (ast, spans, rodeo) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(&ast, Some(&rodeo), Some(&spans), None));
    });
    glob!(r"insta_variables\*.coff", |path| {
        let (ast, _, rodeo) = get_ast(path);
        let vars = NameResolution::visit(&ast);
        assert_snapshot!(DebugPrint::visit(&ast, Some(&rodeo), None, Some(&vars)));
    });
}
