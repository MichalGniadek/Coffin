use crate::{ast::Id, name_resolution::VariableId};
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType(Vec<TypeId>);

impl FunType {
    pub fn new(return_type: TypeId, mut parameters: Vec<TypeId>) -> Self {
        parameters.push(return_type);
        Self(parameters)
    }

    pub fn get_return_type(&self) -> &TypeId {
        self.0
            .last()
            .expect("Interal compiler error: FunType should always have a return type.")
    }

    pub fn get_param_types(&self) -> &[TypeId] {
        &self.0[..self.0.len() - 1]
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
            Type::Fun(_) => write!(f, "fun"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl From<TypeId> for usize {
    fn from(id: TypeId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    types: Vec<Type>,
    type_ids: Vec<TypeId>,
}

impl TypeTable {
    pub const ERROR_ID: TypeId = TypeId(0);
    pub const VOID_ID: TypeId = TypeId(1);
    pub const INT_ID: TypeId = TypeId(2);
    pub const FLOAT_ID: TypeId = TypeId(3);

    pub fn new(max_id: Id) -> Self {
        Self {
            types: vec![Type::Error, Type::Void, Type::Int, Type::Float],
            type_ids: vec![TypeId(0); usize::from(max_id)],
        }
    }

    pub fn new_type(&mut self, ttpe: Type) -> TypeId {
        self.types.push(ttpe);
        TypeId(self.types.len() - 1)
    }

    pub fn set_type_id(&mut self, id: Id, type_id: TypeId) {
        self.type_ids[usize::from(id)] = type_id;
    }

    pub fn type_id(&self, id: Id) -> TypeId {
        self.type_ids[usize::from(id)]
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[usize::from(type_id)]
    }
}

impl Index<Id> for TypeTable {
    type Output = Type;

    fn index(&self, index: Id) -> &Self::Output {
        self.get_type(self.type_id(index))
    }
}

#[derive(Debug, Clone)]
pub struct VariableTypes(Vec<TypeId>);

impl VariableTypes {
    pub fn new(max_id: VariableId) -> Self {
        Self(vec![TypeId(0); usize::from(max_id)])
    }
}

impl Index<VariableId> for VariableTypes {
    type Output = TypeId;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<VariableId> for VariableTypes {
    fn index_mut(&mut self, index: VariableId) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_type_ids() {
        let types = TypeTable::new(Id::new(0));
        assert_eq!(types.get_type(TypeTable::ERROR_ID), &Type::Error);
        assert_eq!(types.get_type(TypeTable::VOID_ID), &Type::Void);
        assert_eq!(types.get_type(TypeTable::INT_ID), &Type::Int);
        assert_eq!(types.get_type(TypeTable::FLOAT_ID), &Type::Float);
    }
}
