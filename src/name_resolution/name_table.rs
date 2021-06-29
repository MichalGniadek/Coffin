use crate::{
    ast::{Id, Name},
    type_id::{builtin_types, TypeId},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

impl From<VariableId> for usize {
    fn from(id: VariableId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone)]
pub struct NameTable {
    variables: HashMap<Id, VariableId>,
    max_var_id: VariableId,

    types: HashMap<Id, TypeId>,
    max_type_id: TypeId,
}

impl NameTable {
    pub fn var_id(&self, name: Name) -> Option<VariableId> {
        self.variables.get(&name.id).cloned()
    }

    pub fn type_id(&self, name: Name) -> TypeId {
        *self.types.get(&name.id).unwrap_or(&builtin_types::ERROR_ID)
    }

    pub fn max_var_id(&self) -> VariableId {
        self.max_var_id
    }

    pub fn max_type_id(&self) -> TypeId {
        self.max_type_id
    }
}

impl NameTable {
    pub(super) fn new() -> Self {
        Self {
            variables: HashMap::new(),
            max_var_id: VariableId(0),

            types: HashMap::new(),
            max_type_id: TypeId::new(),
        }
    }

    pub(super) fn set_var(&mut self, name: Name, var_id: VariableId) {
        self.variables.insert(name.id, var_id);
    }

    pub(super) fn set_type(&mut self, name: Name, type_id: TypeId) {
        self.types.insert(name.id, type_id);
    }

    pub(super) fn new_variable(&mut self) -> VariableId {
        let out = self.max_var_id;
        self.max_var_id.0 += 1;
        out
    }

    pub(super) fn _new_type(&mut self) -> TypeId {
        self.max_type_id.next()
    }
}
