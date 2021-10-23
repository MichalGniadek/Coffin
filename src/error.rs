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
    ConditionIsntABool(Span, String),
    IfAndElseHaveIncompatibleTypes {
        left_span: Span,
        left_type: String,
        right_span: Span,
        right_type: String,
    },

    SwizzleNotAtTheEnd(Span),
    TypeDoesntHaveFields(Span),
    TypeCantBeIndexed(Span),
    IncorrectVectorFields(Span),
}

struct ErrorMessage<'a> {
    msg: String,
    main_span: Option<&'a Span>,
    labels: Vec<(String, &'a Span)>,
}

impl CoffinError {
    fn get_msg(&self) -> ErrorMessage {
        match self {
            CoffinError::IOError(err) => ErrorMessage {
                msg: format!("IO Error: '{}'", err),
                main_span: None,
                labels: vec![],
            },
            CoffinError::ParserError(kind, span) => ErrorMessage {
                msg: kind.to_string(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::UndeclaredVariable(span) => ErrorMessage {
                msg: "Undeclared variable.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::UndeclaredType(span) => ErrorMessage {
                msg: "Undeclared type.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::MismatchedType {
                span,
                expected,
                got,
            } => ErrorMessage {
                msg: format!("Mismatched type. Expected: {}, got: {}.", expected, got),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::WrongTypesForOperator {
                op,
                left_span,
                left_type,
                right_span,
                right_type,
            } => ErrorMessage {
                msg: format!("Wrong types for '{}': {} and {}", op, left_type, right_type),
                main_span: Some(left_span),
                labels: vec![
                    (left_type.to_string(), left_span),
                    (right_type.to_string(), right_span),
                ],
            },
            CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(span) => ErrorMessage {
                msg: "Compute function must have only one paramter of type Id.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::MoreThanOneAttribute(string, span) => ErrorMessage {
                msg: format!("More than one {} attribute", string),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::ConditionIsntABool(span, r#type) => ErrorMessage {
                msg: format!(
                    "Condition doesn't have a bool type. Instead got: {}",
                    r#type
                ),
                main_span: Some(span),
                labels: vec![],
            },
            CoffinError::IfAndElseHaveIncompatibleTypes {
                left_span,
                left_type,
                right_span,
                right_type,
            } => ErrorMessage {
                msg: format!("If and else have incompatible types."),
                main_span: None,
                labels: vec![
                    (left_type.clone(), left_span),
                    (right_type.clone(), right_span),
                ],
            },
            CoffinError::SwizzleNotAtTheEnd(span) => ErrorMessage {
                msg: "Swizzle not at the end.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::TypeDoesntHaveFields(span) => ErrorMessage {
                msg: "Type doesn't have fields.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::TypeCantBeIndexed(span) => ErrorMessage {
                msg: "Type can't be indexed.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
            CoffinError::IncorrectVectorFields(span) => ErrorMessage {
                msg: "Incorrect vector fields.".into(),
                main_span: Some(span),
                labels: vec![("here".into(), span)],
            },
        }
    }

    pub fn report(&self) -> Diagnostic<()> {
        let ErrorMessage { msg, labels, .. } = self.get_msg();

        let labels = labels
            .iter()
            .map(|(msg, span)| Label::primary((), (*span).clone()).with_message(msg))
            .collect();

        Diagnostic::error().with_message(msg).with_labels(labels)
    }
}

impl Display for CoffinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ErrorMessage { msg, main_span, .. } = self.get_msg();
        let span = match main_span {
            Some(span) => format!(" [{:?}]", span),
            None => format!(""),
        };
        write!(f, "{}{}", msg, span)
    }
}

impl Error for CoffinError {}

impl From<std::io::Error> for CoffinError {
    fn from(err: std::io::Error) -> Self {
        Self::IOError(err)
    }
}

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

impl Display for ParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserErrorKind::ExpectedPrefixToken => write!(f, "Expected prefix token."),
            ParserErrorKind::ExpectedToken(token) => write!(f, "Expected {}.", token),
            ParserErrorKind::ExpressionNotAssignable => write!(f, "Expression not assignable."),
            ParserErrorKind::ExpectedItem => write!(f, "Expected item."),
        }
    }
}

impl Error for ParserErrorKind {}
