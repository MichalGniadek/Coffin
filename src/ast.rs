use crate::{error::ParserError, lexer::Token};
use lasso::Spur;
use std::slice;

pub struct Ast(pub Vec<Item>);

impl<'a> IntoIterator for &'a Ast {
    type Item = &'a Item;
    type IntoIter = slice::Iter<'a, Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct Name(pub Id, pub Spur);

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub name: Name,
    pub colon_id: Id,
    pub ttpe: Name,
}

#[derive(Debug, Clone)]
pub struct Attr(pub Name, pub Vec<(Id, Token)>);

#[derive(Debug, Clone)]
pub enum Attrs {
    Ok(Id, Vec<Attr>),
    None,
    Error(Id, ParserError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Id, BinOpKind, Box<Expr>, Box<Expr>),
    Let {
        let_id: Id,
        mut_id: Option<Id>,
        name: Name,
        eq_id: Id,
        expr: Box<Expr>,
    },
    Assign(Id, Name, Box<Expr>),
    Identifier(Name),
    Float(Id, f32),
    Int(Id, i32),
    Block(Id, Vec<Expr>),
    Error(Id, ParserError),
}

#[derive(Debug)]
pub enum Item {
    Fun {
        fun_id: Id,
        attrs: Attrs,
        name: Name,
        paren_id: Id,
        params: Vec<Field>,
        ret: Option<(Id, Name)>,
        body: Expr,
    },
    Error(Id, ParserError),
}

pub trait Visitor {
    type Out;

    fn visit_item(&mut self, item: &Item) -> Self::Out {
        match item {
            Item::Fun {
                fun_id,
                attrs,
                name,
                paren_id,
                params,
                ret,
                body,
            } => self.fun(*fun_id, attrs, *name, *paren_id, params, ret, body),
            Item::Error(id, kind) => self.item_error(*id, kind),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Out {
        match expr {
            Expr::Binary(id, kind, left, right) => self.binary(*id, *kind, left, right),
            Expr::Let {
                let_id,
                mut_id,
                name,
                eq_id,
                expr,
            } => self.r#let(*let_id, *mut_id, *name, *eq_id, expr),
            Expr::Assign(id, name, right) => self.assign(*id, *name, right),
            Expr::Identifier(name) => self.identifier(*name),
            Expr::Float(id, f) => self.float(*id, *f),
            Expr::Int(id, i) => self.int(*id, *i),
            Expr::Block(id, exprs) => self.block(*id, exprs),
            Expr::Error(id, kind) => self.expr_error(*id, kind),
        }
    }

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        name: Name,
        paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out;
    fn item_error(&mut self, id: Id, kind: &ParserError) -> Self::Out;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out;
    fn r#let(
        &mut self,
        let_id: Id,
        mut_id: Option<Id>,
        name: Name,
        eq_id: Id,
        expr: &Expr,
    ) -> Self::Out;
    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out;
    fn identifier(&mut self, name: Name) -> Self::Out;
    fn float(&mut self, id: Id, f: f32) -> Self::Out;
    fn int(&mut self, id: Id, i: i32) -> Self::Out;
    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out;
    fn expr_error(&mut self, id: Id, kind: &ParserError) -> Self::Out;
}
