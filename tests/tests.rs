use coffin2::{
    ast::Ast,
    debug_print, lexer,
    name_resolution::{self, VariableTable},
    parser,
    type_resolution::{self, types::TypeTable},
};
use insta::{assert_snapshot, glob};
use std::{fs, path::Path};

fn get_ast(path: &Path) -> (Ast, VariableTable, TypeTable) {
    let code = fs::read_to_string(path).unwrap();
    let lexer = lexer::lex(&code);
    let mut ast = parser::parse(lexer);
    let vars = name_resolution::visit(&mut ast);
    let types = type_resolution::visit(&mut ast, &vars);
    (ast, vars, types)
}

#[test]
fn parser() {
    glob!(r"insta_parser\*.coff", |path| {
        let (ast, _, _) = get_ast(path);
        assert_snapshot!(debug_print::visit(&ast, true, false, None, None));
    });
}

#[test]
fn parser_spans() {
    glob!(r"insta_parser_spans\*.coff", |path| {
        let (ast, _, _) = get_ast(path);
        assert_snapshot!(debug_print::visit(&ast, true, true, None, None));
    });
}

#[test]
fn variables() {
    glob!(r"insta_variables\*.coff", |path| {
        let (ast, vars, _) = get_ast(path);
        assert_snapshot!(debug_print::visit(&ast, true, false, Some(&vars), None));
    });
}

#[test]
fn types() {
    glob!(r"insta_types\*.coff", |path| {
        let (ast, _, types) = get_ast(path);
        assert_snapshot!(debug_print::visit(&ast, true, false, None, Some(&types),));
    });
}
