use crate::error::ParserErrorKind;
use lasso::Spur;
use logos::Span;
use std::{
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

#[derive(Debug)]
pub struct UntypedAst {
    // TODO
    pub spans: Spans,
}

#[derive(Debug, Clone, Copy)]
pub struct Id(u32);

#[derive(Debug, Clone)]
pub struct Spans(Vec<Span>);

impl Spans {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, value: Span) -> Id {
        self.0.push(value);
        Id(self.0.len() as u32 - 1)
    }
}

impl Index<Id> for Spans {
    type Output = Span;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<Id> for Spans {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpirvId(pub NonZeroU32);

#[derive(Debug)]
pub enum Item {
    Fun(
        Id,
        /* Attrs, */ ItemName,
        /* Args, Return Value,*/ Expr,
    ),
    Error(Id, ParserErrorKind),
}

#[derive(Debug, Clone, Copy)]
pub struct ItemName(pub Id, pub Spur);

#[derive(Debug)]
#[allow(dead_code)]
pub enum Expr {
    Binary(Id, BinOpKind, Box<Expr>, Box<Expr>),
    Assign(Id, Box<Expr>, Box<Expr>),
    Identifier(Id, Spur),
    Float(Id, f32),
    Int(Id, i32),
    Block(Id, Vec<Expr>),
    Error(Id, ParserErrorKind),
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

pub trait Visitor {
    type Out;

    fn visit_item(&mut self, item: &Item) -> Self::Out {
        match item {
            Item::Fun(id, name, expr) => self.fun(*id, *name, expr),
            Item::Error(id, kind) => self.item_error(*id, kind),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Self::Out {
        match expr {
            Expr::Binary(id, kind, left, right) => self.binary(*id, *kind, left, right),
            Expr::Assign(id, left, right) => self.assign(*id, left, right),
            Expr::Identifier(id, identifier) => self.identifier(*id, *identifier),
            Expr::Float(id, f) => self.float(*id, *f),
            Expr::Int(id, i) => self.int(*id, *i),
            Expr::Block(id, exprs) => self.block(*id, exprs),
            Expr::Error(id, kind) => self.expr_error(*id, kind),
        }
    }

    fn fun(&mut self, id: Id, name: ItemName, expr: &Expr) -> Self::Out;
    fn item_error(&mut self, id: Id, kind: &ParserErrorKind) -> Self::Out;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out;
    fn assign(&mut self, id: Id, left: &Expr, right: &Expr) -> Self::Out;
    fn identifier(&mut self, id: Id, identifier: Spur) -> Self::Out;
    fn float(&mut self, id: Id, f: f32) -> Self::Out;
    fn int(&mut self, id: Id, i: i32) -> Self::Out;
    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out;
    fn expr_error(&mut self, id: Id, kind: &ParserErrorKind) -> Self::Out;
}
