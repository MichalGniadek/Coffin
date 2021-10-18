use crate::{error::CoffinError, lexer::Token, parser::spans_table::SpanTable};
use lasso::{RodeoReader, Spur};
use std::fmt::Display;

pub struct Ast {
    pub items: Vec<Item>,
    max_id: Id,

    pub rodeo: RodeoReader,
    pub spans: SpanTable,
    pub errors: Vec<CoffinError>,
}

impl Ast {
    pub fn new(
        items: Vec<Item>,
        max_id: Id,
        rodeo: RodeoReader,
        spans: SpanTable,
        errors: Vec<CoffinError>,
    ) -> Self {
        Self {
            items,
            max_id,
            spans,
            rodeo,
            errors,
        }
    }

    pub fn max_id(&self) -> Id {
        self.max_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl Id {
    pub fn new(n: usize) -> Self {
        Id(n)
    }
}

impl From<Id> for usize {
    fn from(id: Id) -> Self {
        id.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Name {
    pub id: Id,
    pub spur: Spur,
}

#[derive(Debug, Clone, Copy)]
pub struct Field {
    pub name: Name,
    pub colon_id: Id,
    pub r#type: Name,
}

#[derive(Debug, Clone)]
pub struct Attr(pub Name, pub Vec<(Id, Token)>);

#[derive(Debug, Clone)]
pub enum Attrs {
    Ok(Id, Vec<Attr>),
    None,
    Error(Id),
}

impl Attrs {
    pub fn get_attr(&self, name: Option<Spur>) -> Vec<&Attr> {
        let name = match name {
            Some(spur) => spur,
            None => return vec![],
        };
        match self {
            Attrs::None | Attrs::Error(_) => vec![],
            Attrs::Ok(_, attrs) => attrs.iter().filter(|a| a.0.spur == name).collect(),
        }
    }
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

impl Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOpKind::Add => "+",
                BinOpKind::Sub => "-",
                BinOpKind::Mul => "*",
                BinOpKind::Div => "/",
                BinOpKind::Rem => "%",
                BinOpKind::Pow => "**",
                BinOpKind::Eq => "==",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum AccessType {
    Dot(Id, Name),
    Index(Id, Box<Expr>),
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
    Access(Id, Box<Expr>, Vec<AccessType>),
    Assign(Id, Box<Expr>, Vec<AccessType>, Box<Expr>),
    Identifier(Name),
    Float(Id, f32),
    Int(Id, i32),
    Block(Id, Vec<Expr>),
    Convert(Id, Box<Expr>, Name),
    Call(Id, Name, Vec<Expr>),
    If(Id, Box<Expr>, Box<Expr>, Option<(Id, Box<Expr>)>),
    Error(Id),
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
    Uniform(Id, Attrs, Field),
    Error(Id),
}

pub trait ItemVisitor {
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
            Item::Uniform(id, attrs, field) => self.uniform(*id, attrs, field),
            Item::Error(id) => self.item_error(*id),
        }
    }

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        name: Name,
        paren_id: Id,
        params: &[Field],
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out;
    fn uniform(&mut self, unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out;
    fn item_error(&mut self, id: Id) -> Self::Out;
}

pub trait ItemVisitorSimple {
    type Out;

    fn fun(
        &mut self,
        attrs: &Attrs,
        name: Name,
        params: &[Field],
        ret: Option<Name>,
        body: &Expr,
    ) -> Self::Out;
    fn uniform(&mut self, attrs: &Attrs, field: &Field) -> Self::Out;
    fn item_error(&mut self, id: Id) -> Self::Out;
}

impl<V: ItemVisitorSimple> ItemVisitor for V {
    type Out = <V as ItemVisitorSimple>::Out;

    fn fun(
        &mut self,
        _fun_id: Id,
        attrs: &Attrs,
        name: Name,
        _paren_id: Id,
        params: &[Field],
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        self.fun(attrs, name, params, ret.map(|(_, name)| name), body)
    }

    fn uniform(&mut self, _unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out {
        self.uniform(attrs, field)
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.item_error(id)
    }
}

pub trait ExprVisitor {
    type Out;

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
            Expr::Access(id, expr, access) => self.access(*id, expr, access),
            Expr::Assign(id, left, access, right) => self.assign(*id, left, access, right),
            Expr::Identifier(name) => self.identifier(*name),
            Expr::Float(id, f) => self.float(*id, *f),
            Expr::Int(id, i) => self.int(*id, *i),
            Expr::Block(id, exprs) => self.block(*id, exprs),
            Expr::Convert(id, expr, r#type) => self.convert(*id, expr, *r#type),
            Expr::Call(id, name, args) => self.call(*id, *name, args),
            Expr::If(id, condition, block, r#else) => self.r#if(
                *id,
                condition,
                block,
                r#else.as_ref().map(|(id, e)| (*id, &**e)),
            ),
            Expr::Error(id) => self.expr_error(*id),
        }
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out;
    fn r#let(
        &mut self,
        let_id: Id,
        mut_id: Option<Id>,
        name: Name,
        eq_id: Id,
        expr: &Expr,
    ) -> Self::Out;
    fn access(&mut self, id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out;
    fn assign(&mut self, id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out;
    fn identifier(&mut self, name: Name) -> Self::Out;
    fn float(&mut self, id: Id, f: f32) -> Self::Out;
    fn int(&mut self, id: Id, i: i32) -> Self::Out;
    fn block(&mut self, id: Id, exprs: &[Expr]) -> Self::Out;
    fn convert(&mut self, id: Id, expr: &Expr, r#type: Name) -> Self::Out;
    fn call(&mut self, id: Id, name: Name, args: &[Expr]) -> Self::Out;
    fn r#if(
        &mut self,
        id: Id,
        condition: &Expr,
        block: &Expr,
        r#else: Option<(Id, &Expr)>,
    ) -> Self::Out;
    fn expr_error(&mut self, id: Id) -> Self::Out;
}

pub trait ExprVisitorSimple {
    type Out;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out;
    fn r#let(&mut self, id: Id, r#mut: bool, name: Name, expr: &Expr) -> Self::Out;
    fn access(&mut self, id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out;
    fn assign(&mut self, id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out;
    fn identifier(&mut self, name: Name) -> Self::Out;
    fn float(&mut self, id: Id, f: f32) -> Self::Out;
    fn int(&mut self, id: Id, i: i32) -> Self::Out;
    fn block(&mut self, id: Id, exprs: &[Expr]) -> Self::Out;
    fn convert(&mut self, id: Id, expr: &Expr, r#type: Name) -> Self::Out;
    fn call(&mut self, id: Id, name: Name, args: &[Expr]) -> Self::Out;
    fn r#if(&mut self, id: Id, condition: &Expr, block: &Expr, r#else: Option<&Expr>) -> Self::Out;
    fn expr_error(&mut self, id: Id) -> Self::Out;
}

impl<V: ExprVisitorSimple> ExprVisitor for V {
    type Out = <V as ExprVisitorSimple>::Out;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        self.binary(id, kind, left, right)
    }

    fn r#let(
        &mut self,
        let_id: Id,
        mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        self.r#let(let_id, mut_id.is_some(), name, expr)
    }

    fn access(&mut self, id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out {
        self.access(id, expr, access)
    }

    fn assign(&mut self, id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out {
        self.assign(id, left, access, right)
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        self.identifier(name)
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        self.float(id, f)
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        self.int(id, i)
    }

    fn block(&mut self, id: Id, exprs: &[Expr]) -> Self::Out {
        self.block(id, exprs)
    }

    fn convert(&mut self, id: Id, expr: &Expr, r#type: Name) -> Self::Out {
        self.convert(id, expr, r#type)
    }

    fn call(&mut self, id: Id, name: Name, args: &[Expr]) -> Self::Out {
        self.call(id, name, args)
    }

    fn r#if(
        &mut self,
        id: Id,
        condition: &Expr,
        block: &Expr,
        r#else: Option<(Id, &Expr)>,
    ) -> Self::Out {
        self.r#if(id, condition, block, r#else.map(|(_, expr)| expr))
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.expr_error(id)
    }
}

impl Expr {
    // Uses ExprIdGetter to make sure we always use
    // a correct "main" id
    pub fn get_id(&self) -> Id {
        ExprIdGetter.visit_expr(self)
    }
}

struct ExprIdGetter;

impl ExprVisitorSimple for ExprIdGetter {
    type Out = Id;

    fn binary(&mut self, id: Id, _kind: BinOpKind, _left: &Expr, _right: &Expr) -> Self::Out {
        id
    }

    fn r#let(&mut self, id: Id, r#_mut: bool, _name: Name, _expr: &Expr) -> Self::Out {
        id
    }

    fn access(&mut self, id: Id, _expr: &Expr, _access: &[AccessType]) -> Self::Out {
        id
    }

    fn assign(&mut self, id: Id, _left: &Expr, _access: &[AccessType], _right: &Expr) -> Self::Out {
        id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        name.id
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        id
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        id
    }

    fn block(&mut self, id: Id, _exprs: &[Expr]) -> Self::Out {
        id
    }

    fn convert(&mut self, id: Id, _expr: &Expr, r#_type: Name) -> Self::Out {
        id
    }

    fn call(&mut self, id: Id, _name: Name, _args: &[Expr]) -> Self::Out {
        id
    }

    fn r#if(
        &mut self,
        id: Id,
        _condition: &Expr,
        _block: &Expr,
        r#_else: Option<&Expr>,
    ) -> Self::Out {
        id
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        id
    }
}
