use coffin2::parser;
use coffin2::{ast::Visitor, lexer::Token, pretty_print::PrettyPrint};
use insta::{assert_snapshot, glob};
use logos::Logos;
use std::fs;
use std::path::Path;

#[test]
fn insta() {
    glob!(r"shaders\*.coff", |path| { test_file(path, false) });
}

fn test_file(path: &Path, with_spans: bool) {
    let code = fs::read_to_string(path).unwrap();
    let lexer = Token::lexer(&code);
    let ast = parser::parse(lexer);
    let spans = if with_spans { Some(&ast.spans) } else { None };
    let mut print = PrettyPrint::new(ast.rodeo, spans);
    assert_snapshot!(ast
        .items
        .iter()
        .map(|i| print.visit_item(i))
        .reduce(|a, b| format!("{}\n{}", a, b))
        .unwrap());
}
