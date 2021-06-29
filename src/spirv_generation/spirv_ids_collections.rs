use crate::{
    name_resolution::{NameTable, VariableId},
    type_id::TypeId,
    type_resolution::types::TypeTable,
};
use rspirv::spirv::StorageClass;
use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone)]
pub struct VariableSpirvIds(Vec<u32>);

impl VariableSpirvIds {
    pub fn new(variables: &NameTable) -> Self {
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
pub struct TypeSpirvIds {
    spirv_ids: Vec<u32>,
    pointer_spirv_ids: HashMap<(TypeId, StorageClass), u32>,
}

impl TypeSpirvIds {
    pub fn new(types: &TypeTable) -> Self {
        Self {
            spirv_ids: vec![0; usize::from(types.max_type_id())],
            pointer_spirv_ids: HashMap::new(),
        }
    }

    pub fn get_pointer(&self, type_id: TypeId, storage_class: StorageClass) -> Option<u32> {
        self.pointer_spirv_ids
            .get(&(type_id, storage_class))
            .cloned()
    }

    pub fn set_pointer(&mut self, type_id: TypeId, storage_class: StorageClass, spirv_id: u32) {
        self.pointer_spirv_ids
            .insert((type_id, storage_class), spirv_id);
    }
}

impl Index<TypeId> for TypeSpirvIds {
    type Output = u32;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.spirv_ids[usize::from(index)]
    }
}

impl IndexMut<TypeId> for TypeSpirvIds {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        &mut self.spirv_ids[usize::from(index)]
    }
}
