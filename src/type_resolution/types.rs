use crate::{
    ast::{Ast, Id},
    name_resolution::{NameTable, VariableId},
    type_id::TypeId,
};
use rspirv::spirv::StorageClass;
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType(Vec<TypeId>, TypeId);

impl FunType {
    pub fn new(return_type: TypeId, parameters: Vec<TypeId>) -> Self {
        Self(parameters, return_type)
    }

    pub fn get_return_type(&self) -> TypeId {
        self.1
    }

    pub fn get_param_types(&self) -> &[TypeId] {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Error,
    Int,
    UInt,
    Float,
    Bool,
    Image(/* TODO */),
    Fun(FunType),
    Vector(Vec<char>, TypeId),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Error => write!(f, "error"),
            Type::Int => write!(f, "int"),
            Type::UInt => write!(f, "uint"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Image() => write!(f, "image"),
            Type::Fun(_) => write!(f, "fun"),
            Type::Vector(_, _) => write!(f, "vector"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    types: Vec<Type>,
    max_type_id: TypeId,
    ast_types: Vec<TypeId>,
    var_types: Vec<(TypeId, StorageClass)>,
}

impl TypeTable {
    pub fn new(ast: &Ast, names: &NameTable) -> Self {
        use crate::type_id::builtin::*;
        let mut slf = Self {
            types: vec![Type::Error; usize::from(names.max_type_id())],
            max_type_id: names.max_type_id(),
            ast_types: vec![ERROR_ID; usize::from(ast.max_id())],
            var_types: vec![(ERROR_ID, StorageClass::Uniform); usize::from(names.max_var_id())],
        };

        slf.set_type(ERROR_ID, Type::Error);
        slf.set_type(UNIT_ID, Type::Unit);
        slf.set_type(INT_ID, Type::Int);
        slf.set_type(UINT_ID, Type::UInt);
        slf.set_type(FLOAT_ID, Type::Float);
        slf.set_type(IMAGE_ID, Type::Image());
        slf.set_type(BOOL_ID, Type::Bool);

        let a = ['x', 'y', 'z', 'w'];
        for i in 1..=4 {
            slf.set_type(IVEC_ID[i], Type::Vector(a[0..i].to_owned(), INT_ID));
            slf.set_type(UVEC_ID[i], Type::Vector(a[0..i].to_owned(), UINT_ID));
            slf.set_type(FVEC_ID[i], Type::Vector(a[0..i].to_owned(), FLOAT_ID));
        }

        slf
    }

    pub fn set_type(&mut self, type_id: TypeId, r#type: Type) {
        self.types[usize::from(type_id)] = r#type;
    }

    pub fn new_type(&mut self, r#type: Type) -> TypeId {
        self.types.push(r#type);
        self.max_type_id.increase()
    }

    pub fn type_id(&self, id: Id) -> TypeId {
        self.ast_types[usize::from(id)]
    }

    /// Returns back the passed type_id
    pub fn set_type_id(&mut self, id: Id, type_id: TypeId) -> TypeId {
        self.ast_types[usize::from(id)] = type_id;
        type_id
    }

    pub fn var_type_id(&self, id: VariableId) -> (TypeId, StorageClass) {
        self.var_types[usize::from(id)]
    }

    pub fn set_var_type_id(
        &mut self,
        id: VariableId,
        type_id: TypeId,
        storage_class: StorageClass,
    ) {
        self.var_types[usize::from(id)] = (type_id, storage_class);
    }

    pub fn max_type_id(&self) -> TypeId {
        self.max_type_id
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
