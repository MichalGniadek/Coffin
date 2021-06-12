use coffin2::{
    ast::Ast,
    debug_print::DebugPrint,
    error::CoffinError,
    lexer::Token,
    name_resolution::NameResolution,
    parser::{self, spans_table::SpansTable},
    type_resolution::TypeResolution,
};
use insta::{assert_snapshot, glob};
use lasso::RodeoResolver;
use logos::Logos;
use std::{fs, path::Path};

fn get_ast(path: &Path) -> (Ast, SpansTable, RodeoResolver, Vec<CoffinError>) {
    let code = fs::read_to_string(path).unwrap();
    let lexer = Token::lexer(&code);
    parser::parse(lexer)
}

#[test]
fn insta() {
    glob!(r"insta_parser\*.coff", |path| {
        let (ast, _, rodeo, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            None,
            None,
            Some(errors)
        ));
    });
    glob!(r"insta_parser_spans\*.coff", |path| {
        let (ast, spans, rodeo, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            Some(&spans),
            None,
            None,
            Some(errors)
        ));
    });
    glob!(r"insta_variables\*.coff", |path| {
        let (ast, spans, rodeo, mut errors0) = get_ast(path);
        let (vars, errors1) = NameResolution::visit(&ast, &spans);
        errors0.extend(errors1);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            Some(&vars),
            None,
            Some(errors0)
        ));
    });
    glob!(r"insta_types\*.coff", |path| {
        let (ast, spans, rodeo, mut errors0) = get_ast(path);
        let (vars, errors1) = NameResolution::visit(&ast, &spans);
        let types = TypeResolution::visit(&ast, &vars);
        errors0.extend(errors1);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            None,
            Some(&types),
            Some(errors0)
        ));
    });
}
