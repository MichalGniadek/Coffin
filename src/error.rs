use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Spur;
use logos::Span;
use std::io;

use crate::lexer::Token;

#[derive(Debug, thiserror::Error)]
pub enum CoffinError {
    #[error("IO error \"{0}\"")]
    IOError(#[from] io::Error),
    #[error(transparent)]
    ParserError(#[from] ParserError),
}

impl CoffinError {
    pub fn report(&self) -> Diagnostic<()> {
        let text = format!("{}", self);
        match self {
            CoffinError::IOError(_) => Diagnostic::error().with_message(text),
            CoffinError::ParserError(err) => Diagnostic::error()
                .with_message(text.clone())
                .with_labels(vec![Label::primary((), err.1.clone()).with_message(text)]),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("{0}")]
pub struct ParserError(#[source] pub ParserErrorKind, pub Span);

#[derive(Debug, Clone, thiserror::Error)]
pub enum ParserErrorKind {
    #[error("Expected prefix token.")]
    ExpectedPrefixToken,
    #[error("Expected token {0}.")]
    ExpectedToken(Token),
}

impl ParserErrorKind {
    pub fn expected_identifier() -> Self {
        Self::ExpectedToken(Token::Identifier(Spur::default()))
    }
}
