use crate::{ast::Name, name_resolution::VariableTable};
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct VariableSpirvIds<'a> {
    variable_table: &'a VariableTable,
    /// Should be indexed with VariableIds
    spirv_ids: Vec<u32>,
}

impl<'a> VariableSpirvIds<'a> {
    pub fn new(variable_table: &'a VariableTable) -> Self {
        Self {
            variable_table,
            spirv_ids: vec![0; usize::from(variable_table.max_var_id())],
        }
    }

    pub fn set_variable_spirv_id(&mut self, name: Name, spirv_id: u32) {
        let var_id = self.variable_table.get(name.id).unwrap();
        self.spirv_ids[usize::from(var_id)] = spirv_id;
    }
}

impl Index<Name> for VariableSpirvIds<'_> {
    type Output = u32;

    fn index(&self, index: Name) -> &Self::Output {
        &self
            .variable_table
            .get(index.id)
            .map(|var_id| &self.spirv_ids[usize::from(var_id)])
            .unwrap_or(&0)
    }
}
