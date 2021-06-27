use crate::lexer::Token;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Spur;
use logos::Span;
use std::{error::Error, fmt::Display, io};

#[derive(Debug)]
pub enum CoffinError {
    IOError(io::Error),

    ParserError(ParserErrorKind, Span),

    UndeclaredVariable(Span),

    UndeclaredType(Span),
    MismatchedType {
        span: Span,
        expected: String,
        got: String,
    },
    WrongTypesForOperator {
        op: String,
        left_span: Span,
        left_type: String,
        right_span: Span,
        right_type: String,
    },
    ComputeFunctionMustHaveOnlyOneParameterOfTypeId(Span),
    MoreThanOneAttribute(String, Span),

    SwizzleNotAtTheEnd(Span),
    TypeDoesntHaveFields(Span),
    TypeCantBeIndexed(Span),
    IncorrectVectorFields(Span),
}

impl CoffinError {
    fn get_msg(&self) -> (String, Option<&Span>, Vec<(String, &Span)>) {
        match self {
            CoffinError::IOError(err) => (format!("IO Error: '{}'", err), None, vec![]),
            CoffinError::ParserError(kind, span) => match kind {
                ParserErrorKind::ExpectedPrefixToken => (
                    "Expected prefix token.".into(),
                    Some(span),
                    vec![("here".into(), span)],
                ),
                ParserErrorKind::ExpectedToken(token) => (
                    format!("Expected {}.", token),
                    Some(span),
                    vec![("here".into(), span)],
                ),
                ParserErrorKind::ExpressionNotAssignable => (
                    "Expression not assignable.".into(),
                    Some(span),
                    vec![("here".into(), span)],
                ),
                ParserErrorKind::ExpectedItem => (
                    "Expected item.".into(),
                    Some(span),
                    vec![("here".into(), span)],
                ),
            },
            CoffinError::UndeclaredVariable(span) => (
                "Undeclared variable.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::UndeclaredType(span) => (
                "Undeclared type.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::MismatchedType {
                span,
                expected,
                got,
            } => (
                format!("Mismatched type. Expected: {}, got: {}.", expected, got),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::WrongTypesForOperator {
                op,
                left_span,
                left_type,
                right_span,
                right_type,
            } => (
                format!("Wrong types for '{}': {} and {}", op, left_type, right_type),
                Some(left_span),
                vec![
                    (format!("{}", left_type), left_span),
                    (format!("{}", right_type), right_span),
                ],
            ),
            CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(span) => (
                "Compute function must have only one paramter of type Id.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::MoreThanOneAttribute(string, span) => (
                format!("More than one {} attribute", string),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::SwizzleNotAtTheEnd(span) => (
                "Swizzle not at the end.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::TypeDoesntHaveFields(span) => (
                "Type doesn't have fields.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::TypeCantBeIndexed(span) => (
                "Type can't be indexed.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
            CoffinError::IncorrectVectorFields(span) => (
                "Incorrect vector fields.".into(),
                Some(span),
                vec![("here".into(), span)],
            ),
        }
    }

    pub fn report(&self) -> Diagnostic<()> {
        let (msg, _, labels) = self.get_msg();

        let labels = labels
            .iter()
            .map(|(msg, span)| Label::primary((), (*span).clone()).with_message(msg))
            .collect();

        Diagnostic::error().with_message(msg).with_labels(labels)
    }
}

impl Display for CoffinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, span, _) = self.get_msg();
        let span = match span {
            Some(span) => format!(" [{:?}]", span),
            None => format!(""),
        };
        write!(f, "{}{}", msg, span)
    }
}

impl Error for CoffinError {}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    ExpectedPrefixToken,
    ExpectedToken(Token),
    ExpressionNotAssignable,
    ExpectedItem,
}

impl ParserErrorKind {
    pub fn expected_identifier() -> Self {
        Self::ExpectedToken(Token::Identifier(Spur::default()))
    }
}

pub fn internal_error(msg: &str) -> ! {
    panic!(
        "Internal compiler error. This is a bug please report it.\n{}",
        msg
    )
}

pub trait InternalError<T> {
    fn ie_expect(self, msg: &str) -> T;
}

impl<T> InternalError<T> for Option<T> {
    fn ie_expect(self, msg: &str) -> T {
        match self {
            Some(val) => val,
            None => internal_error(msg),
        }
    }
}
