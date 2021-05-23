use coffin2::parser;
use coffin2::{ast::Visitor, lexer::Token, pretty_print::PrettyPrint};
use insta::{assert_snapshot, glob};
use logos::Logos;
use std::fs;

#[test]
fn insta() {
    glob!(r"shaders\*.coff", |path| {
        let code = fs::read_to_string(path).unwrap();
        let lexer = Token::lexer(&code);
        let ast = parser::parse(lexer);
        let mut print = PrettyPrint::new(&ast.spans);
        assert_snapshot!(ast
            .items
            .iter()
            .map(|i| print.visit_item(i))
            .reduce(|a, b| format!("{}\n{}", a, b))
            .unwrap());
    });
}
