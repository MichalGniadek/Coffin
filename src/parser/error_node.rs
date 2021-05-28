use crate::{
    ast::{self, Id},
    error::ParserError,
};

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
