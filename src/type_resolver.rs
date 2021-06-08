use crate::{
    ast::{Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    error::ParserError,
};
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType(Vec<Type>);

impl FunType {
    pub fn get_return_type(&self) -> &Type {
        &self.0[0]
    }

    pub fn get_arg_type(&self) -> &[Type] {
        &self.0[1..self.0.len() - 1]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Poisoned,
    Int,
    Float,
    Fun(FunType),
}

#[derive(Debug, Clone)]
pub struct Types(Vec<Type>);

impl Types {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn new_with_size(n: usize, default: Type) {
        let mut v = vec![];
        v.resize(n, default);
        Self(v);
    }

    pub fn push(&mut self, value: Type) -> Id {
        self.0.push(value);
        Id(self.0.len() as usize - 1)
    }
}

impl Index<Id> for Types {
    type Output = Type;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<Id> for Types {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

pub struct TypeResolver {
    types: Types,
    /*variables, scopes, accumulated errors*/
}

impl Visitor for TypeResolver {
    type Out = Id;

    fn fun(
        &mut self,
        __fun_id: Id,
        __attrs: &Attrs,
        __name: Name,
        __paren_id: Id,
        __params: &Vec<Field>,
        __ret: &Option<(Id, Name)>,
        __body: &Expr,
    ) -> Self::Out {
        todo!()
    }

    fn item_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.types[id] = Type::Poisoned;
        id
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        let left = &self.types[left];
        let right = &self.types[right];

        let ttpe = match (left, kind, right) {
            (Type::Int, _, Type::Int) => Type::Int,
            (Type::Float, _, Type::Float) => Type::Float,
            (_, _, _) => Type::Poisoned,
        };
        self.types[id] = ttpe;
        id
    }

    fn r#let(
        &mut self,
        __let_id: Id,
        __mut_id: Option<Id>,
        __name: Name,
        __eq_id: Id,
        __expr: &Expr,
    ) -> Self::Out {
        todo!()
    }

    fn assign(&mut self, __id: Id, __name: Name, __right: &Expr) -> Self::Out {
        todo!()
    }

    fn identifier(&mut self, __name: Name) -> Self::Out {
        todo!()
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types[id] = Type::Float;
        id
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types[id] = Type::Int;
        id
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        let mut ttpe = &Type::Void;
        for e in exprs {
            let expr_id = self.visit_expr(e);
            ttpe = &self.types[expr_id];
        }
        self.types[id] = ttpe.clone();
        id
    }

    fn expr_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.types[id] = Type::Poisoned;
        id
    }
}
