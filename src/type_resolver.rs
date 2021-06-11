use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    error::ParserError,
    name_resolution::VariablesTable,
};
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType(Vec<Type>);

impl FunType {
    pub fn new(return_type: Type, parameters: &[Type]) -> Self {
        let mut v = vec![return_type];
        v.extend_from_slice(parameters);
        Self(v)
    }

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
    Error,
    Int,
    Float,
    Fun(FunType),
}

#[derive(Debug, Clone)]
pub struct TypesTable(Vec<Type>);

impl Index<Id> for TypesTable {
    type Output = Type;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl IndexMut<Id> for TypesTable {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

pub struct TypeResolver<'a> {
    variables: &'a VariablesTable,
    variable_type: Vec<Type>,
    types: TypesTable,
}

impl<'a> TypeResolver<'a> {
    pub fn new(ast: &Ast, variables: &'a VariablesTable) -> TypesTable {
        let mut slf = Self {
            variables,
            variable_type: vec![Type::Error; variables.max_var_id()],
            types: TypesTable(vec![Type::Error; ast.max_id]),
        };

        for item in ast {
            slf.visit_item(item);
        }

        slf.types
    }
}

impl Visitor for TypeResolver<'_> {
    type Out = Id;

    fn fun(
        &mut self,
        __fun_id: Id,
        __attrs: &Attrs,
        name: Name,
        __paren_id: Id,
        params: &Vec<Field>,
        __ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        if !params.is_empty() {
            todo!("Function parameters not implemented")
        };
        self.visit_expr(body);
        name.0
    }

    fn item_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.types[id] = Type::Error;
        id
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        let left = &self.types[left];
        let right = &self.types[right];

        self.types[id] = match (left, kind, right) {
            (Type::Int, _, Type::Int) => Type::Int,
            (Type::Float, _, Type::Float) => Type::Float,
            (Type::Error, _, _) => Type::Error,
            (_, _, Type::Error) => Type::Error,
            _ => Type::Error,
        };

        id
    }

    fn r#let(
        &mut self,
        let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let var_id = self.variables.get(name.0).unwrap();
        let expr_id = self.visit_expr(expr);
        self.variable_type[var_id.0] = self.types[expr_id].clone();

        self.types[let_id] = Type::Void;
        let_id
    }

    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out {
        let expr_id = self.visit_expr(right);
        let expr_type = &self.types[expr_id];
        let var_type = match self.variables.get(name.0) {
            Some(i) => &self.variable_type[i.0],
            None => &Type::Error,
        };

        if expr_type != var_type && expr_type != &Type::Error && var_type != &Type::Error {
            todo!("type check error");
        }

        self.types[id] = Type::Void;
        id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let ttpe = match self.variables.get(name.0) {
            Some(var_id) => self.variable_type[var_id.0].clone(),
            None => Type::Error,
        };
        self.types[name.0] = ttpe;

        name.0
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
        self.types[id] = match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(id) => self.types[id].clone(),
            None => Type::Void,
        };

        id
    }

    fn expr_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.types[id] = Type::Error;
        id
    }
}
