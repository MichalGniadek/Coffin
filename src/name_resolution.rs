use crate::{
    ast::{AccessType, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, ItemVisitor, Name},
    error::CoffinError,
    parser::spans_table::SpanTable,
};
use lasso::Spur;
use std::collections::HashMap;

pub fn visit(ast: &mut Ast) -> VariableTable {
    let mut nr = NameResolution {
        variables: VariableTable::new(),
        scopes: vec![HashMap::new()],

        spans: &ast.spans,
        errors: &mut ast.errors,
    };

    for item in &ast.items {
        nr.visit_item(item);
    }

    nr.variables
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(usize);

impl From<VariableId> for usize {
    fn from(id: VariableId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone)]
pub struct VariableTable(HashMap<Id, VariableId>, usize);

impl VariableTable {
    fn new() -> Self {
        Self(HashMap::new(), 0)
    }

    pub fn max_var_id(&self) -> VariableId {
        VariableId(self.1)
    }

    pub fn get(&self, id: Id) -> Option<VariableId> {
        self.0.get(&id).cloned()
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

struct NameResolution<'ast> {
    variables: VariableTable,
    scopes: Vec<HashMap<Spur, VariableId>>,

    spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl NameResolution<'_> {
    fn new_variable(&mut self, name: Name) {
        let var_id = self.variables.new_variable(name.id);
        self.scopes.last_mut().unwrap().insert(name.spur, var_id);
    }
}

impl ItemVisitor for NameResolution<'_> {
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

    fn uniform(&mut self, _unif_id: Id, _attrs: &Attrs, field: &Field) -> Self::Out {
        self.new_variable(field.name);
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {}
}

impl ExprVisitor for NameResolution<'_> {
    type Out = ();
    fn binary(&mut self, _id: Id, _kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        self.visit_expr(left);
        self.visit_expr(right);
    }

    fn let_declaration(
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

    fn access(&mut self, _id: Id, expr: &Expr, access: &Vec<AccessType>) -> Self::Out {
        self.visit_expr(expr);
        for a in access {
            if let AccessType::Index(_, expr) = a {
                self.scopes.push(HashMap::new());
                self.visit_expr(expr);
                self.scopes.pop();
            }
        }
    }

    fn assign(
        &mut self,
        _id: Id,
        left: &Expr,
        access: &Vec<AccessType>,
        right: &Expr,
    ) -> Self::Out {
        self.visit_expr(left);
        for a in access {
            if let AccessType::Index(_, expr) = a {
                self.scopes.push(HashMap::new());
                self.visit_expr(expr);
                self.scopes.pop();
            }
        }
        self.visit_expr(right);
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let mut scopes = self.scopes.iter().rev();
        let found = scopes.find_map(|scope| scope.get(&name.spur));

        match found {
            Some(&var_id) => self.variables.insert(name.id, var_id),
            None => self
                .errors
                .push(CoffinError::UndeclaredVariable(self.spans[name.id].clone())),
        }
    }

    fn float(&mut self, _id: Id, _f: f32) -> Self::Out {}
    fn int(&mut self, _id: Id, _i: i32) -> Self::Out {}

    fn block(&mut self, _id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.scopes.push(HashMap::new());
        for e in exprs {
            self.visit_expr(e);
        }
        self.scopes.pop();
    }

    fn convert(&mut self, _id: Id, expr: &Expr, _ttpe: Name) -> Self::Out {
        self.visit_expr(expr)
    }

    fn expr_error(&mut self, _id: Id) -> Self::Out {}
}
