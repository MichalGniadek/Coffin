use crate::{ast::Id, name_resolution::VariableId};
use rspirv::spirv;
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

    pub fn get_return_type(&self) -> TypeId {
        self.0
            .last()
            .expect("Interal compiler error: FunType should always have a return type.")
            .clone()
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
    UInt,
    Float,
    Image(/* TODO */),
    // Should probably be optimized. A Vec for each type
    // with type_ids for every storage type instead of
    // a new Type::Pointer
    Pointer(spirv::StorageClass, TypeId),
    Fun(FunType),
    Vector(Vec<char>, TypeId),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "error"),
            Type::Int => write!(f, "int"),
            Type::UInt => write!(f, "uint"),
            Type::Float => write!(f, "float"),
            Type::Image() => write!(f, "image"),
            Type::Pointer(_, _) => write!(f, "pointer"),
            Type::Fun(_) => write!(f, "fun"),
            Type::Vector(_, _) => write!(f, "vector"),
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
    ast_types: Vec<TypeId>,
    var_types: Vec<TypeId>,
}

impl TypeTable {
    pub const ERROR_ID: TypeId = TypeId(0);
    pub const VOID_ID: TypeId = TypeId(1);
    pub const INT_ID: TypeId = TypeId(2);
    pub const UINT_ID: TypeId = TypeId(3);
    pub const FLOAT_ID: TypeId = TypeId(4);

    pub const IVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(5), TypeId(6), TypeId(7), TypeId(8)];
    pub const UVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(9), TypeId(10), TypeId(11), TypeId(12)];
    pub const FVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(13), TypeId(14), TypeId(15), TypeId(16)];

    pub const ID_ID: TypeId = Self::UVEC_ID[3];

    pub const IMAGE_ID: TypeId = TypeId(17);

    pub fn new(max_id: Id, max_var_id: VariableId) -> Self {
        Self {
            types: vec![
                Type::Error,
                Type::Void,
                Type::Int,
                Type::UInt,
                Type::Float,
                Type::Vector(vec!['x'], Self::INT_ID),
                Type::Vector(vec!['x', 'y'], Self::INT_ID),
                Type::Vector(vec!['x', 'y', 'z'], Self::INT_ID),
                Type::Vector(vec!['x', 'y', 'z', 'w'], Self::INT_ID),
                Type::Vector(vec!['x'], Self::UINT_ID),
                Type::Vector(vec!['x', 'y'], Self::UINT_ID),
                Type::Vector(vec!['x', 'y', 'z'], Self::UINT_ID),
                Type::Vector(vec!['x', 'y', 'z', 'w'], Self::UINT_ID),
                Type::Vector(vec!['x'], Self::FLOAT_ID),
                Type::Vector(vec!['x', 'y'], Self::FLOAT_ID),
                Type::Vector(vec!['x', 'y', 'z'], Self::FLOAT_ID),
                Type::Vector(vec!['x', 'y', 'z', 'w'], Self::FLOAT_ID),
                Type::Image(),
            ],
            ast_types: vec![TypeId(0); usize::from(max_id)],
            var_types: vec![TypeId(0); usize::from(max_var_id)],
        }
    }

    pub fn max_type_id(&self) -> TypeId {
        TypeId(self.types.len())
    }

    pub fn new_type(&mut self, ttpe: Type) -> TypeId {
        self.types.push(ttpe);
        TypeId(self.types.len() - 1)
    }

    pub fn type_id(&self, id: Id) -> TypeId {
        self.ast_types[usize::from(id)]
    }

    pub fn set_type_id(&mut self, id: Id, type_id: TypeId) {
        self.ast_types[usize::from(id)] = type_id;
    }

    pub fn var_type_id(&self, id: VariableId) -> TypeId {
        self.var_types[usize::from(id)]
    }

    pub fn set_var_type_id(&mut self, id: VariableId, type_id: TypeId) {
        self.var_types[usize::from(id)] = type_id;
    }
}

impl Index<TypeId> for TypeTable {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[usize::from(index)]
    }
}

impl IndexMut<TypeId> for TypeTable {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        &mut self.types[usize::from(index)]
    }
}

impl Index<Id> for TypeTable {
    type Output = Type;

    fn index(&self, index: Id) -> &Self::Output {
        &self[self.type_id(index)]
    }
}

impl IndexMut<Id> for TypeTable {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        let type_id = self.type_id(index);
        &mut self[type_id]
    }
}

impl Index<VariableId> for TypeTable {
    type Output = Type;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self[self.var_type_id(index)]
    }
}

impl IndexMut<VariableId> for TypeTable {
    fn index_mut(&mut self, index: VariableId) -> &mut Self::Output {
        let type_id = self.var_type_id(index);
        &mut self[type_id]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_type_ids() {
        let types = TypeTable::new(Id::new(0), VariableId::new(0));
        assert_eq!(types[TypeTable::ERROR_ID], Type::Error);
        assert_eq!(types[TypeTable::VOID_ID], Type::Void);
        assert_eq!(types[TypeTable::INT_ID], Type::Int);
        assert_eq!(types[TypeTable::FLOAT_ID], Type::Float);

        assert_eq!(types[TypeTable::IVEC_ID[0]], Type::Error);
        assert_eq!(
            types[TypeTable::IVEC_ID[1]],
            Type::Vector(vec!['x'], TypeTable::INT_ID)
        );
        assert_eq!(
            types[TypeTable::IVEC_ID[2]],
            Type::Vector(vec!['x', 'y'], TypeTable::INT_ID)
        );
        assert_eq!(
            types[TypeTable::IVEC_ID[3]],
            Type::Vector(vec!['x', 'y', 'z'], TypeTable::INT_ID)
        );
        assert_eq!(
            types[TypeTable::IVEC_ID[4]],
            Type::Vector(vec!['x', 'y', 'z', 'w'], TypeTable::INT_ID)
        );

        assert_eq!(types[TypeTable::UVEC_ID[0]], Type::Error);
        assert_eq!(
            types[TypeTable::UVEC_ID[1]],
            Type::Vector(vec!['x'], TypeTable::UINT_ID)
        );
        assert_eq!(
            types[TypeTable::UVEC_ID[2]],
            Type::Vector(vec!['x', 'y'], TypeTable::UINT_ID)
        );
        assert_eq!(
            types[TypeTable::UVEC_ID[3]],
            Type::Vector(vec!['x', 'y', 'z'], TypeTable::UINT_ID)
        );
        assert_eq!(
            types[TypeTable::UVEC_ID[4]],
            Type::Vector(vec!['x', 'y', 'z', 'w'], TypeTable::UINT_ID)
        );

        assert_eq!(types[TypeTable::FVEC_ID[0]], Type::Error);
        assert_eq!(
            types[TypeTable::FVEC_ID[1]],
            Type::Vector(vec!['x'], TypeTable::FLOAT_ID)
        );
        assert_eq!(
            types[TypeTable::FVEC_ID[2]],
            Type::Vector(vec!['x', 'y'], TypeTable::FLOAT_ID)
        );
        assert_eq!(
            types[TypeTable::FVEC_ID[3]],
            Type::Vector(vec!['x', 'y', 'z'], TypeTable::FLOAT_ID)
        );
        assert_eq!(
            types[TypeTable::FVEC_ID[4]],
            Type::Vector(vec!['x', 'y', 'z', 'w'], TypeTable::FLOAT_ID)
        );

        assert_eq!(
            types[TypeTable::ID_ID],
            Type::Vector(vec!['x', 'y', 'z'], TypeTable::UINT_ID),
        );

        assert_eq!(types[TypeTable::IMAGE_ID], Type::Image(),);

        assert_eq!(
            types.types.len(),
            18,
            "Triggers if someone adds a builtin type without updating the tests."
        );
    }
}
