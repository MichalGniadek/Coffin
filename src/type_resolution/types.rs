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
        use crate::type_id::builtin_types::*;
        let mut slf = Self {
            types: vec![Type::Error; usize::from(names.max_type_id())],
            max_type_id: names.max_type_id(),
            ast_types: vec![ERROR_ID; usize::from(ast.max_id())],
            var_types: vec![(ERROR_ID, StorageClass::Uniform); usize::from(names.max_var_id())],
        };

        slf.set_type(ERROR_ID, Type::Error);
        slf.set_type(VOID_ID, Type::Void);
        slf.set_type(INT_ID, Type::Int);
        slf.set_type(UINT_ID, Type::UInt);
        slf.set_type(FLOAT_ID, Type::Float);
        slf.set_type(IMAGE_ID, Type::Image());

        let a = ['x', 'y', 'z', 'w'];
        for i in 1..=4 {
            slf.set_type(IVEC_ID[i], Type::Vector(a[0..i].to_owned(), INT_ID));
            slf.set_type(UVEC_ID[i], Type::Vector(a[0..i].to_owned(), UINT_ID));
            slf.set_type(FVEC_ID[i], Type::Vector(a[0..i].to_owned(), FLOAT_ID));
        }

        slf
    }

    pub fn set_type(&mut self, type_id: TypeId, ttpe: Type) {
        self.types[usize::from(type_id)] = ttpe;
    }

    pub fn new_type(&mut self, ttpe: Type) -> TypeId {
        self.types.push(ttpe);
        self.max_type_id.next()
    }

    pub fn type_id(&self, id: Id) -> TypeId {
        self.ast_types[usize::from(id)]
    }

    pub fn set_type_id(&mut self, id: Id, type_id: TypeId) {
        self.ast_types[usize::from(id)] = type_id;
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
