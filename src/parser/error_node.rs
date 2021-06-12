use super::expr_parsing::ExprResult;
use crate::ast::{self, Attrs, Id};

pub struct ErrorNode(pub Id);

impl From<ErrorNode> for ast::Item {
    fn from(err: ErrorNode) -> Self {
        Self::Error(err.0)
    }
}

impl From<ErrorNode> for ast::Expr {
    fn from(err: ErrorNode) -> Self {
        Self::Error(err.0)
    }
}

impl From<ErrorNode> for ExprResult {
    fn from(err: ErrorNode) -> Self {
        Self::PanicMode(err.into())
    }
}

impl From<ErrorNode> for ast::Attrs {
    fn from(err: ErrorNode) -> Self {
        Attrs::Error(err.0)
    }
}
