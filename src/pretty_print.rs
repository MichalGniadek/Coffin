use crate::ast::{self, Visitor};
use ast::{BinOpKind, Expr, Id};
use lasso::Spur;

pub struct PrettyPrint {
    padding: String,
}

impl PrettyPrint {
    pub fn new() -> Self {
        Self {
            padding: String::new(),
        }
    }
}

impl Visitor for PrettyPrint {
    type Out = String;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let symbol = match kind {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Rem => "%",
            BinOpKind::Pow => "**",
        };
        self.padding.push_str("│  ");
        let left = self.visit_expr(left);
        {
            self.padding.pop();
            self.padding.pop();
            self.padding.pop();
        }
        self.padding.push_str("   ");
        let right = self.visit_expr(right);
        {
            self.padding.pop();
            self.padding.pop();
            self.padding.pop();
        }

        format!(
            "{:?} binary: {}\n{}├{}\n{}└{}",
            id, symbol, self.padding, left, self.padding, right
        )
    }

    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);
        format!("{:?} assign to: {}\n└{}", id, left, right)
    }

    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out {
        format!("{:?} identifier: {:?} - {}", id, identifier, "TODO")
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("{:?} float: {}", id, f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("{:?} int: {}", id, i)
    }

    fn error(&mut self, id: Id) -> Self::Out {
        format!("{:?} Error", id)
    }
}
