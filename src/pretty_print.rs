use crate::{
    ast::{self, Attrs, Field, Spans, Visitor},
    error::ParserError,
};
use ast::{BinOpKind, Expr, Id, Name};
use lasso::{RodeoResolver, Spur};

pub struct PrettyPrint<'a> {
    rodeo: RodeoResolver,
    spans: Option<&'a Spans>,
    indent: String,
}

impl<'a> PrettyPrint<'a> {
    #![allow(dead_code)]
    pub fn new(rodeo: RodeoResolver, spans: Option<&'a Spans>) -> Self {
        Self {
            rodeo,
            spans,
            indent: String::new(),
        }
    }

    fn span(&self, id: Id) -> String {
        match self.spans {
            Some(s) => format!("[{:?}] ", s[id]),
            None => "".to_owned(),
        }
    }

    fn ident(&self, spur: Spur) -> &str {
        self.rodeo.resolve(&spur)
    }

    fn err(&self, err: &ParserError) -> String {
        match self.spans {
            Some(_) => format!("[{:?}] {}", err.1, err.to_string()),
            None => err.to_string(),
        }
    }

    fn name(&self, name: Name) -> String {
        format!("{}{}", self.span(name.0), self.ident(name.1))
    }

    fn field(&self, field: &Field) -> String {
        format!(
            "{} {}: {}",
            self.name(field.name),
            self.span(field.colon_id),
            self.name(field.ttpe)
        )
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
            Attrs::Ok(id, attrs) => {
                format!(
                    "{}#[{}]",
                    self.span(*id),
                    // Attribute
                    attrs.iter().fold("".to_owned(), |a, b| {
                        format!(
                            "{}{}({}), ",
                            a,
                            self.name(b.0),
                            // Attribute arguments
                            b.1.iter().fold("".to_owned(), |a, b| format!(
                                "{}{}{} ",
                                a,
                                self.span(b.0),
                                b.1
                            ))
                        )
                    })
                )
            }
            Attrs::None => String::new(),
            Attrs::Error(id, err) => format!("#[{}Err: {}]", self.span(*id), self.err(err)),
        };

        let params_text = params
            .iter()
            .fold("".to_owned(), |a, b| format!("{}{}, ", a, self.field(b)));

        let ret_text = match ret {
            Some((id, name)) => format!("{}-> {}", self.span(*id), self.name(*name)),
            None => "".to_owned(),
        };

        format!(
            "{}{}fun {} {}({}) {} {}",
            attr_text,
            self.span(fun_id),
            self.name(name),
            self.span(paren_id),
            params_text,
            ret_text,
            self.visit_expr(body)
        )
    }

    fn item_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!("{}Err: {}", self.span(id), self.err(err))
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let symbol = match kind {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Rem => "%",
            BinOpKind::Pow => "**",
            BinOpKind::Eq => "==",
        };

        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        format!("({}{} {} {})", self.span(id), left, symbol, right)
    }

    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);
        format!("({}let {} = {})", self.span(id), left, right)
    }

    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out {
        format!("{}${}", self.span(id), self.ident(identifier))
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("{}#{}", self.span(id), f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("{}#{}", self.span(id), i)
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.indent.push('\t');
        let indent = self.indent.clone();
        let body = exprs.iter().fold("".to_owned(), |a, e| {
            format!("{}\n{}{}", a, indent, self.visit_expr(e))
        });
        self.indent.pop();

        format!("{}{{{}\n{}}}", self.span(id), body, self.indent)
    }

    fn expr_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!("{}Err: {}", self.span(id), self.err(err))
    }
}
