use crate::{ast::Id, name_resolution::VariableId};
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
        &self.0[1..]
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
