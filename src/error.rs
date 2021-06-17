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
    UndeclaredVariable(Span),

    #[error("Undeclared type. [{0:?}]")]
    UndeclaredType(Span),
    #[error("Mismatched type. Expected {expected}, got {got}. [{span:?}]")]
    MismatchedTypes {
        span: Span,
        expected: String,
        got: String,
    },
    #[error(
        "Wrong types for '{op}', {left_type} [{left_span:?}] and {right_type} [{right_span:?}]."
    )]
    WrongTypesForOperator {
        op: String,
        left_span: Span,
        left_type: String,
        right_span: Span,
        right_type: String,
    },
}

impl CoffinError {
    pub fn report(&self) -> Diagnostic<()> {
        match self {
            CoffinError::IOError(err) => Diagnostic::error().with_message(format!("{}", err)),
            CoffinError::ParserError(kind, span) => Diagnostic::error()
                .with_message(format!("{}", kind))
                .with_labels(vec![
                    Label::primary((), span.clone()).with_message(format!("{}", kind))
                ]),
            CoffinError::UndeclaredVariable(span) => Diagnostic::error()
                .with_message("Undeclared variable.")
                .with_labels(vec![
                    Label::primary((), span.clone()).with_message("Undeclared variable.")
                ]),
            CoffinError::UndeclaredType(span) => Diagnostic::error()
                .with_message("Undeclared type.")
                .with_labels(vec![
                    Label::primary((), span.clone()).with_message("Undeclared type.")
                ]),
            CoffinError::MismatchedTypes {
                span,
                expected,
                got,
            } => {
                let message = format!("Mismatched type. Expected {}, got {}.", expected, got);
                Diagnostic::error()
                    .with_message(message.clone())
                    .with_labels(vec![Label::primary((), span.clone()).with_message(message)])
            }
            CoffinError::WrongTypesForOperator {
                op,
                left_span,
                left_type,
                right_span,
                right_type,
            } => {
                let message = format!(
                    "Wrong types for '{}', {} and {}.",
                    op, left_type, right_type
                );
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![Label::primary((), left_span.clone())
                        .with_message(format!("{}", left_type))])
                    .with_labels(vec![Label::primary((), right_span.clone())
                        .with_message(format!("{}", right_type))])
            }
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ParserErrorKind {
    #[error("Expected prefix token.")]
    ExpectedPrefixToken,
    #[error("Expected {0}.")]
    ExpectedToken(Token),
    #[error("Expression not assignable.")]
    ExpressionNotAssignable,
}

impl ParserErrorKind {
    pub fn expected_identifier() -> Self {
        Self::ExpectedToken(Token::Identifier(Spur::default()))
    }
}
