use crate::{ast::Id, name_resolution::VariableId};
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

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

    pub fn get_param_types(&self) -> &[Type] {
        &self.0[1..]
    }
}

impl Display for FunType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: String = self
            .get_param_types()
            .iter()
            .map(|t| format!("{}", t))
            .intersperse(String::from(", "))
            .collect();
        write!(f, "fun({}) -> {}", params, self.get_return_type())
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Error => unreachable!("Display is not implemented for error type."),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Fun(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypesTable(Vec<Type>);

impl TypesTable {
    pub fn new(max_id: Id) -> Self {
        Self(vec![Type::Error; usize::from(max_id)])
    }
}

impl Index<Id> for TypesTable {
    type Output = Type;

    fn index(&self, index: Id) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<Id> for TypesTable {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}

#[derive(Debug, Clone)]
pub struct VariableTypes(Vec<Type>);

impl VariableTypes {
    pub fn new(max_id: VariableId) -> Self {
        Self(vec![Type::Error; usize::from(max_id)])
    }
}

impl Index<VariableId> for VariableTypes {
    type Output = Type;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<VariableId> for VariableTypes {
    fn index_mut(&mut self, index: VariableId) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}
