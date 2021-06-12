use crate::lexer::Token;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Spur;
use logos::Span;
use std::io;

#[derive(Debug, thiserror::Error)]
pub enum CoffinError {
    #[error("IO error \"{0}\"")]
    IOError(#[from] io::Error),
    #[error("{0} [{1:?}]")]
    ParserError(ParserErrorKind, Span),
    #[error("Undeclared variable. [{0:?}]")]
    NameResolutionError(Span),
}

impl CoffinError {
    pub fn report(&self) -> Diagnostic<()> {
        match self {
            CoffinError::IOError(_) => Diagnostic::error().with_message(format!("{}", self)),
            CoffinError::ParserError(kind, span) => Diagnostic::error()
                .with_message(format!("{}", kind))
                .with_labels(vec![
                    Label::primary((), span.clone()).with_message(format!("{}", kind))
                ]),
            CoffinError::NameResolutionError(span) => Diagnostic::error()
                .with_message("Undeclared variable.")
                .with_labels(vec![
                    Label::primary((), span.clone()).with_message("Undeclared variable.")
                ]),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ParserErrorKind {
    #[error("Expected prefix token.")]
    ExpectedPrefixToken,
    #[error("Expected token {0}.")]
    ExpectedToken(Token),
    #[error("Expression not assignable.")]
    ExpressionNotAssignable,
}

impl ParserErrorKind {
    pub fn expected_identifier() -> Self {
        Self::ExpectedToken(Token::Identifier(Spur::default()))
    }
}
