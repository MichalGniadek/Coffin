use crate::{ast::{self, Visitor}, error::ParserErrorKind};
use ast::{BinOpKind, Expr, Id};
use lasso::Spur;
use logos::Span;

pub struct PrettyPrint {
    padding: String,
    spans: Vec<Span>,
}

impl PrettyPrint {
    pub fn new(spans: Vec<Span>) -> Self {
        Self {
            padding: String::new(),
            spans,
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
            self.spans[id.0 as usize], symbol, self.padding, left, self.padding, right
        )
    }

    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);
        format!(
            "{:?} assign to: {}\n└{}",
            self.spans[id.0 as usize], left, right
        )
    }

    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out {
        format!(
            "{:?} identifier: {:?} - {}",
            self.spans[id.0 as usize], identifier, "TODO"
        )
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("{:?} float: {}", self.spans[id.0 as usize], f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("{:?} int: {}", self.spans[id.0 as usize], i)
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        let span = self.spans[id.0 as usize].clone();
        format!(
            "{:?} Block: {{{}\n}}",
            span,
            exprs.iter().fold("".to_string(), |a, e| format!(
                "{}\n\n{}",
                a,
                self.visit_expr(e)
            ))
        )
    }

    fn error(&mut self, id: Id, kind: &ParserErrorKind) -> Self::Out {
        format!("{:?} Error {:?}", self.spans[id.0 as usize], kind)
    }
}
