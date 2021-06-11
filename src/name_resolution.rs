use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    error::ParserError,
};
use lasso::Spur;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(pub usize);

impl Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct VariablesTable(HashMap<Id, VariableId>, usize);

impl VariablesTable {
    pub fn max_var_id(&self) -> usize {
        self.1
    }

    pub fn get(&self, id: Id) -> Option<VariableId> {
        self.0.get(&id).cloned()
    }

    fn new() -> Self {
        Self(HashMap::new(), 0)
    }

    fn insert(&mut self, id: Id, var_id: VariableId) {
        self.0.insert(id, var_id);
    }

    fn new_variable(&mut self, id: Id) -> VariableId {
        self.1 += 1;
        self.insert(id, VariableId(self.1 - 1));
        VariableId(self.1 - 1)
    }
}

pub struct NameResolution {
    variables: VariablesTable,
    scopes: Vec<HashMap<Spur, VariableId>>,
}

impl NameResolution {
    pub fn visit(ast: &Ast) -> VariablesTable {
        let mut slf = Self {
            variables: VariablesTable::new(),
            scopes: vec![],
        };

        for item in ast {
            slf.visit_item(item);
        }

        slf.variables
    }

    fn new_variable(&mut self, name: Name) {
        let var_id = self.variables.new_variable(name.0);
        self.scopes.last_mut().unwrap().insert(name.1, var_id);
    }
}

impl Visitor for NameResolution {
    type Out = ();

    fn fun(
        &mut self,
        _fun_id: Id,
        _attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        params: &Vec<Field>,
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        self.scopes.push(HashMap::new());
        for param in params {
            self.new_variable(param.name);
        }
        self.visit_expr(body);
        self.scopes.pop();
    }

    fn r#let(
        &mut self,
        _let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        self.visit_expr(expr);
        self.new_variable(name);
    }

    fn assign(&mut self, _id: Id, name: Name, right: &Expr) -> Self::Out {
        self.visit_expr(right);

        for scope in self.scopes.iter().rev() {
            if let Some(&var_id) = scope.get(&name.1) {
                self.variables.insert(name.0, var_id);
                return;
            }
        }
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        for scope in self.scopes.iter().rev() {
            if let Some(&var_id) = scope.get(&name.1) {
                self.variables.insert(name.0, var_id);
                return;
            }
        }
    }

    fn block(&mut self, _id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.scopes.push(HashMap::new());
        for e in exprs {
            self.visit_expr(e);
        }
        self.scopes.pop();
    }

    fn binary(&mut self, _id: Id, _kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        self.visit_expr(left);
        self.visit_expr(right);
    }

    fn item_error(&mut self, _id: Id, _kind: &ParserError) -> Self::Out {}
    fn float(&mut self, _id: Id, _f: f32) -> Self::Out {}
    fn int(&mut self, _id: Id, _i: i32) -> Self::Out {}
    fn expr_error(&mut self, _id: Id, _kind: &ParserError) -> Self::Out {}
}
