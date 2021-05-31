use crate::{
    ast::{self, Attrs, Field, Spans, Visitor},
    error::ParserError,
};
use ast::{BinOpKind, Expr, Id, Name};
use lasso::Spur;

pub struct PrettyPrint<'a> {
    spans: &'a Spans,
    indent: String,
}

impl<'a> PrettyPrint<'a> {
    #![allow(dead_code)]
    pub fn new(spans: &'a Spans) -> Self {
        Self {
            spans,
            indent: String::new(),
        }
    }
}

impl<'a> Visitor for PrettyPrint<'a> {
    type Out = String;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        name: Name,
        paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let attr_text = match attrs {
            Attrs::Ok(id, attrs) => attrs
                .into_iter()
                .fold(format!("[{:?}]", self.spans[*id]), |a, b| {
                    format!("{}, {:?} {:?}", a, b.0, b.1)
                }),
            Attrs::None => String::new(),
            Attrs::Error(id, err) => format!(
                "[{:?}] attr error [{:?}] \"{}\"",
                self.spans[*id],
                err.1,
                err.to_string(),
            ),
        };

        let ret_text = match ret {
            Some((_, _)) => " -> sth",
            None => "",
        };

        format!(
            "[{:?}] #[{}] fun[{:?}]({:?}){} \"[{:?}] {:?}\"\n{}",
            self.spans[fun_id],
            attr_text,
            self.spans[paren_id],
            params,
            ret_text,
            self.spans[name.0],
            name.1,
            self.visit_expr(body)
        )
    }

    fn item_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!(
            "[{:?}] item error [{:?}] \"{}\"",
            self.spans[id],
            err.1,
            err.to_string(),
        )
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

        format!("([{:?}] {} {} {})", self.spans[id], left, symbol, right)
    }

    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);
        format!("([{:?}] let {} = {})", self.spans[id], left, right)
    }

    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out {
        format!("([{:?}] \"{:?}\")", self.spans[id], identifier)
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("([{:?}] #{})", self.spans[id], f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("([{:?}] #{})", self.spans[id], i)
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.indent.push('\t');
        let indent = self.indent.clone();

        let out = format!(
            "[{:?}] {{{}\n{}}}",
            self.spans[id],
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

    fn expr_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!(
            "[{:?}] expr error [{:?}] \"{}\"",
            self.spans[id],
            err.1,
            err.to_string(),
        )
    }
}
