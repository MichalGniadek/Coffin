use crate::{
    ast::{Id, Name},
    type_id::TypeId,
};
use std::collections::HashMap;

/// Keeps track of VariableIds to TypeIds
/// and maps them to Ast
#[derive(Debug, Clone, Default)]
pub struct NameTable {
    variables: HashMap<Id, VariableId>,
    max_var_id: VariableId,

    types: HashMap<Id, TypeId>,
    max_type_id: TypeId,
}

impl NameTable {
    /// Get VariableId (if it exists) from an ast node
    pub fn var_id(&self, name: Name) -> Option<VariableId> {
        self.variables.get(&name.id).cloned()
    }

    /// Get TypeId (if it exists) from an ast node
    pub fn type_id(&self, name: Name) -> Option<TypeId> {
        self.types.get(&name.id).cloned()
    }

    pub fn max_var_id(&self) -> VariableId {
        self.max_var_id
    }

    pub fn max_type_id(&self) -> TypeId {
        self.max_type_id
    }
}

impl NameTable {
    pub(super) fn set_var(&mut self, name: Name, var_id: VariableId) {
        self.variables.insert(name.id, var_id);
    }

    pub(super) fn set_type(&mut self, name: Name, type_id: TypeId) {
        self.types.insert(name.id, type_id);
    }

    pub(super) fn new_variable(&mut self) -> VariableId {
        self.max_var_id.increase()
    }

    pub(super) fn _new_type(&mut self) -> TypeId {
        self.max_type_id.increase()
    }
}

///Represents a variable taking into an account names and scopes
///```text
/// let a; //0
/// let b; //1
/// {
///     let a; //2
///     a;     //2
///     let a; //3
///     a;     //3
///     b;     //1
/// }
/// a; //0
///```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct VariableId(usize);

impl VariableId {
    pub fn increase(&mut self) -> Self {
        let out = *self;
        self.0 += 1;
        out
    }
}

impl From<VariableId> for usize {
    fn from(id: VariableId) -> Self {
        id.0
    }
}
