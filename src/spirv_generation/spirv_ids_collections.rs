use crate::{
    name_resolution::{VariableId, VariableTable},
    type_resolution::types::{TypeId, TypeTable},
};
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct VariableSpirvIds(Vec<u32>);

impl VariableSpirvIds {
    pub fn new(variables: &VariableTable) -> Self {
        Self(vec![0; usize::from(variables.max_var_id())])
    }
}

impl Index<VariableId> for VariableSpirvIds {
    type Output = u32;

    fn index(&self, index: VariableId) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<VariableId> for VariableSpirvIds {
    fn index_mut(&mut self, index: VariableId) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}

#[derive(Debug, Clone)]
pub struct TypeSpirvIds(Vec<u32>);

impl TypeSpirvIds {
    pub fn new(types: &TypeTable) -> Self {
        Self(vec![0; usize::from(types.max_type_id())])
    }
}

impl Index<TypeId> for TypeSpirvIds {
    type Output = u32;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl IndexMut<TypeId> for TypeSpirvIds {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}
