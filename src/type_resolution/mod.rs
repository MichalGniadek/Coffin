pub mod types;

use self::types::{Type, TypesTable, VariableTypes};
use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    name_resolution::VariablesTable,
};
use lasso::RodeoResolver;

pub fn visit(ast: &Ast, variables: &VariablesTable, resolver: &RodeoResolver) -> TypesTable {
    let mut tr = TypeResolution {
        variables,
        variable_type: VariableTypes::new(variables.max_var_id()),
        resolver,
        types: TypesTable::new(ast.max_id()),
    };

    // TODO: do a pass for gathering struct definitions
    for item in ast {
        tr.visit_item(item);
    }

    tr.types
}

struct TypeResolution<'a, 'b> {
    variables: &'a VariablesTable,
    variable_type: VariableTypes,
    resolver: &'b RodeoResolver,
    types: TypesTable,
}

impl Visitor for TypeResolution<'_, '_> {
    type Out = Id;

    fn fun(
        &mut self,
        fun_id: Id,
        _attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        params: &Vec<Field>,
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        for field in params {
            let ttpe = match self.resolver.resolve(&field.ttpe.spur) {
                "void" => Type::Void,
                "int" => Type::Int,
                _ => Type::Error,
            };
            let var_id = self.variables.get(field.name.id).unwrap();
            self.variable_type[var_id] = ttpe;
        }

        self.visit_expr(body);
        fun_id
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types[id] = Type::Error;
        id
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        let left = &self.types[left];
        let right = &self.types[right];

        self.types[id] = match (left, kind, right) {
            (Type::Int, _, Type::Int) => Type::Int,
            (Type::Float, _, Type::Float) => Type::Float,
            (Type::Error, _, _) => Type::Error,
            (_, _, Type::Error) => Type::Error,
            _ => Type::Error,
        };

        id
    }

    fn r#let(
        &mut self,
        let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let var_id = self.variables.get(name.id).unwrap();
        let expr_id = self.visit_expr(expr);
        self.variable_type[var_id] = self.types[expr_id].clone();

        self.types[let_id] = Type::Void;
        let_id
    }

    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out {
        let expr_id = self.visit_expr(right);
        let expr_type = &self.types[expr_id];
        let var_type = match self.variables.get(name.id) {
            Some(i) => &self.variable_type[i],
            None => &Type::Error,
        };

        if expr_type != var_type && expr_type != &Type::Error && var_type != &Type::Error {
            todo!("type check error");
        }

        self.types[id] = Type::Void;
        id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        self.types[name.id] = match self.variables.get(name.id) {
            Some(var_id) => self.variable_type[var_id].clone(),
            None => Type::Error,
        };

        name.id
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types[id] = Type::Float;
        id
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types[id] = Type::Int;
        id
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.types[id] = match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(id) => self.types[id].clone(),
            None => Type::Void,
        };

        id
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types[id] = Type::Error;
        id
    }
}
