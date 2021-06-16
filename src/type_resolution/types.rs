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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl From<TypeId> for usize {
    fn from(id: TypeId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone)]
pub struct TypesTable {
    types: Vec<Type>,
    type_ids: Vec<TypeId>,
}

impl TypesTable {
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

impl Index<Id> for TypesTable {
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
        let types = TypesTable::new(Id::new(0));
        assert_eq!(types.get_type(TypesTable::ERROR_ID), &Type::Error);
        assert_eq!(types.get_type(TypesTable::VOID_ID), &Type::Void);
        assert_eq!(types.get_type(TypesTable::INT_ID), &Type::Int);
        assert_eq!(types.get_type(TypesTable::FLOAT_ID), &Type::Float);
    }
}
