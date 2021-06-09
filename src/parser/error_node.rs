use crate::{
    ast::{self, Attrs, Id},
    error::ParserError,
};
use super::expr_parsing::ExprResult;

pub struct ErrorNode(pub Id, pub ParserError);

impl From<ErrorNode> for ast::Item {
    fn from(err: ErrorNode) -> Self {
        Self::Error(err.0, err.1)
    }
}

impl From<ErrorNode> for ast::Expr {
    fn from(err: ErrorNode) -> Self {
        Self::Error(err.0, err.1)
    }
}

impl From<ErrorNode> for ExprResult {
    fn from(err: ErrorNode) -> Self {
        Self::PanicMode(err.into())
    }
}

impl From<ErrorNode> for ast::Attrs {
    fn from(err: ErrorNode) -> Self {
        Attrs::Error(err.0, err.1)
    }
}
