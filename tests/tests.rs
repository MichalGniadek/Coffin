use coffin2::{
    ast::Ast,
    debug_print::DebugPrint,
    error::CoffinError,
    lexer,
    name_resolution::{self, VariableTable},
    parser::{self, spans_table::SpanTable},
    type_resolution::{self, types::TypeTable},
};
use insta::{assert_snapshot, glob};
use lasso::RodeoResolver;
use std::{fs, path::Path};

fn get_ast(
    path: &Path,
) -> (
    Ast,
    SpanTable,
    RodeoResolver,
    VariableTable,
    TypeTable,
    Vec<CoffinError>,
) {
    let code = fs::read_to_string(path).unwrap();
    let lexer = lexer::lex(&code);
    let (ast, spans, rodeo, parser_errors) = parser::parse(lexer);
    let (vars, name_errors) = name_resolution::visit(&ast, &spans);
    let (types, type_errors) = type_resolution::visit(&ast, &vars, &rodeo, &spans);
    let mut errors = vec![];
    errors.extend(parser_errors);
    errors.extend(name_errors);
    errors.extend(type_errors);
    (ast, spans, rodeo, vars, types, errors)
}

#[test]
fn parser() {
    glob!(r"insta_parser\*.coff", |path| {
        let (ast, _, rodeo, _, _, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            None,
            None,
            Some(errors)
        ));
    });
}

#[test]
fn parser_spans() {
    glob!(r"insta_parser_spans\*.coff", |path| {
        let (ast, spans, rodeo, _, _, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            Some(&spans),
            None,
            None,
            Some(errors)
        ));
    });
}

#[test]
fn variables() {
    glob!(r"insta_variables\*.coff", |path| {
        let (ast, _, rodeo, vars, _, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            Some(&vars),
            None,
            Some(errors)
        ));
    });
}

#[test]
fn types() {
    glob!(r"insta_types\*.coff", |path| {
        let (ast, _, rodeo, _, types, errors) = get_ast(path);
        assert_snapshot!(DebugPrint::visit(
            &ast,
            Some(&rodeo),
            None,
            None,
            Some(&types),
            Some(errors)
        ));
    });
}
