use coffin2::{debug_print, lexer, name_resolution, parser, type_resolution};
use insta::{assert_snapshot, glob};
use std::{fs, path::Path};

fn test_util(path: &Path, spans: bool, variables: bool, types: bool) -> String {
    let code = fs::read_to_string(path).unwrap();
    let lexer = lexer::lex(&code);
    let mut ast = parser::parse(lexer);

    let variables = if variables | types {
        Some(name_resolution::visit(&mut ast))
    } else {
        None
    };

    let types = if types {
        Some(type_resolution::visit(
            &mut ast,
            variables.as_ref().unwrap(),
        ))
    } else {
        None
    };

    // let _module = spirv_generation::visit(&mut ast, &vars, &type_table);

    debug_print::visit(&ast, true, spans, variables.as_ref(), types.as_ref())
}

#[test]
fn parser() {
    glob!(r"insta_parser\*.coff", |path| {
        assert_snapshot!(test_util(path, false, false, false));
    });
}

#[test]
fn parser_spans() {
    glob!(r"insta_parser_spans\*.coff", |path| {
        assert_snapshot!(test_util(path, true, false, false));
    });
}

#[test]
fn variables() {
    glob!(r"insta_variables\*.coff", |path| {
        assert_snapshot!(test_util(path, false, true, false));
    });
}

#[test]
fn types() {
    glob!(r"insta_types\*.coff", |path| {
        assert_snapshot!(test_util(path, false, false, true));
    });
}
