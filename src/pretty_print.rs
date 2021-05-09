use crate::{
    ast::{self, Visitor},
    error::ParserErrorKind,
};
use ast::{BinOpKind, Expr, Id, ItemName};
use lasso::Spur;
use logos::Span;

pub struct PrettyPrint {
    spans: Vec<Span>,
    indent: String,
}

impl PrettyPrint {
    pub fn new(spans: Vec<Span>) -> Self {
        Self {
            spans,
            indent: String::new(),
        }
    }

    fn span(&self, id: Id) -> Span {
        self.spans[id.0 as usize].clone()
    }
}

impl Visitor for PrettyPrint {
    type Out = String;

    fn fun(&mut self, id: Id, name: ItemName, expr: &Expr) -> Self::Out {
        format!(
            "[{:?}] fun \"[{:?}] {:?}\"\n{}",
            self.span(id),
            self.span(name.0),
            name.1,
            self.visit_expr(expr)
        )
    }

    fn item_error(&mut self, id: Id, kind: &ParserErrorKind) -> Self::Out {
        format!("[{:?}] item error {:?}", self.span(id), kind)
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let symbol = match kind {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Rem => "%",
            BinOpKind::Pow => "**",
        };

        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        format!("([{:?}] {} {} {})", self.span(id), left, symbol, right)
    }

    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);
        format!("([{:?}] let {} = {})", self.span(id), left, right)
    }

    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out {
        format!("([{:?}] \"{:?}\")", self.span(id), identifier)
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("([{:?}] #{})", self.span(id), f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("([{:?}] #{})", self.span(id), i)
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.indent.push('\t');
        let indent = self.indent.clone();

        let out = format!(
            "[{:?}] {{{}\n{}}}",
            self.span(id),
            exprs.iter().fold(indent.clone(), |a, e| format!(
                "{}\n{}{}",
                a,
                indent,
                self.visit_expr(e)
            )),
            indent
        );

        self.indent.pop();
        out
    }

    fn expr_error(&mut self, id: Id, kind: &ParserErrorKind) -> Self::Out {
        format!("[{:?}] expr error {:?}", self.span(id), kind)
    }
}
