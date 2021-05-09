use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::Span;
use std::io;

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

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct ParserError(#[source] pub ParserErrorKind, pub Span);

#[derive(Debug, thiserror::Error)]
pub enum ParserErrorKind {
    #[error("Unexpected token while trying to parse an item.")]
    UnexpectedTokenWhileParsingItem,
    #[error("Unfinished expression.")]
    UnfinishedExpression,
    #[error("Token isn't a prefix token.")]
    TokenIsntAPrefixToken,
    #[error("Token isn't an infix token.")]
    TokenIsntAnInfixToken,
    #[error("Attribute argument must be an int.")]
    AttributeArgumentMustBeAnInt,
    #[error("Attribute must be an identifier.")]
    AttributeMustBeAnIdentifier,
    #[error("Missing attribute.")]
    MissingAttribute,
    #[error("TODO Error")]
    TODOError,
}
