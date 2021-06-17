pub mod types;

use self::types::{Type, TypeId, TypeTable, VariableTypes};
use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    ast_span,
    error::CoffinError,
    name_resolution::VariableTable,
    parser::spans_table::SpanTable,
    type_resolution::types::FunType,
};
use lasso::RodeoResolver;

pub fn visit(
    ast: &Ast,
    variables: &VariableTable,
    resolver: &RodeoResolver,
    spans: &SpanTable,
) -> (TypeTable, Vec<CoffinError>) {
    let mut tr = TypeResolution {
        resolver,

        variables,
        variable_type: VariableTypes::new(variables.max_var_id()),

        types: TypeTable::new(ast.max_id()),

        spans,
        errors: vec![],
    };

    // TODO: do a pass for gathering struct definitions
    for item in ast {
        tr.visit_item(item);
    }

    (tr.types, tr.errors)
}

struct TypeResolution<'a, 'b, 'c> {
    resolver: &'b RodeoResolver,

    variables: &'a VariableTable,
    variable_type: VariableTypes,

    types: TypeTable,

    spans: &'c SpanTable,
    errors: Vec<CoffinError>,
}

impl TypeResolution<'_, '_, '_> {
    fn resolve_type(&mut self, name: &Name) -> TypeId {
        match self.resolver.resolve(&name.spur) {
            "void" => TypeTable::VOID_ID,
            "int" => TypeTable::INT_ID,
            _ => {
                let span = self.spans[name.id].clone();
                self.errors.push(CoffinError::UndeclaredType(span));
                TypeTable::ERROR_ID
            }
        }
    }
}

impl Visitor for TypeResolution<'_, '_, '_> {
    type Out = Id;

    fn fun(
        &mut self,
        fun_id: Id,
        _attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let mut param_types = vec![];

        for field in params {
            let ttpe = self.resolve_type(&field.ttpe);
            param_types.push(ttpe.clone());

            let var_id = self.variables.get(field.name.id).unwrap();
            self.variable_type[var_id] = ttpe;
        }

        let return_type = match ret {
            Some((_, ttpe)) => self.resolve_type(ttpe),
            None => TypeTable::VOID_ID,
        };

        let type_id = self
            .types
            .new_type(Type::Fun(FunType::new(return_type, param_types)));

        self.types.set_type_id(fun_id, type_id);

        self.visit_expr(body);
        fun_id
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        id
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let left_id = self.visit_expr(left);
        let right_id = self.visit_expr(right);

        let left_type = &self.types[left_id];
        let right_type = &self.types[right_id];

        let type_id = match (left_type, kind, right_type) {
            (Type::Int, _, Type::Int) => TypeTable::INT_ID,
            (Type::Float, _, Type::Float) => TypeTable::FLOAT_ID,
            (Type::Error, _, _) => TypeTable::ERROR_ID,
            (_, _, Type::Error) => TypeTable::ERROR_ID,
            _ => {
                let left_span = ast_span::get_expr_span(left, self.spans);
                let right_span = ast_span::get_expr_span(right, self.spans);

                self.errors.push(CoffinError::WrongTypesForOperator {
                    op: format!("{}", kind),
                    left_span,
                    left_type: format!("{}", left_type),
                    right_span,
                    right_type: format!("{}", right_type),
                });

                TypeTable::ERROR_ID
            }
        };

        self.types.set_type_id(id, type_id);
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
        self.variable_type[var_id] = self.types.type_id(expr_id);

        self.types.set_type_id(let_id, TypeTable::VOID_ID);
        let_id
    }

    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out {
        let expr_id = self.visit_expr(right);
        let expr_type = &self.types[expr_id];
        let var_type = match self.variables.get(name.id) {
            Some(i) => self.types.get_type(self.variable_type[i]),
            None => &Type::Error,
        };

        if var_type != expr_type && expr_type != &Type::Error && var_type != &Type::Error {
            let span = ast_span::get_expr_span(right, self.spans);
            self.errors.push(CoffinError::MismatchedTypes {
                span,
                expected: format!("{}", var_type),
                got: format!("{}", expr_type),
            });
        }

        self.types.set_type_id(id, TypeTable::VOID_ID);
        id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        match self.variables.get(name.id) {
            Some(var_id) => self.types.set_type_id(name.id, self.variable_type[var_id]),
            None => self.types.set_type_id(name.id, TypeTable::ERROR_ID),
        };

        name.id
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types.set_type_id(id, TypeTable::FLOAT_ID);
        id
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types.set_type_id(id, TypeTable::INT_ID);
        id
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(expr_id) => self.types.set_type_id(id, self.types.type_id(expr_id)),
            None => self.types.set_type_id(id, TypeTable::VOID_ID),
        };

        id
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        id
    }
}
