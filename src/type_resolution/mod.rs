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
use rspirv::spirv::StorageClass;

pub fn visit(ast: &mut Ast, variables: &VariableTable) -> TypeTable {
    let mut tr = TypeResolution {
        variables: VariableTypes::new(variables),
        types: TypeTable::new(ast.max_id()),

        resolver: &ast.resolver,
        spans: &ast.spans,
        errors: &mut ast.errors,
    };

    // TODO: do a pass for gathering struct definitions
    for item in &ast.items {
        tr.visit_item(item);
    }

    tr.types
}

struct TypeResolution<'ast, 'vars> {
    variables: VariableTypes<'vars>,
    types: TypeTable,

    resolver: &'ast RodeoResolver,
    spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl TypeResolution<'_, '_> {
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

impl Visitor for TypeResolution<'_, '_> {
    type Out = TypeId;

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
            let type_id = self.resolve_type(&field.ttpe);
            param_types.push(type_id.clone());

            self.variables.set_variable_type_id(field.name, type_id);
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
        type_id
    }

    fn uniform(&mut self, unif_id: Id, _attrs: &Attrs, field: &Field) -> Self::Out {
        let type_id = self.resolve_type(&field.ttpe);
        self.variables.set_variable_type_id(field.name, type_id);

        let pointer_type = self
            .types
            .new_type(Type::Pointer(StorageClass::UniformConstant, type_id));
        self.types.set_type_id(unif_id, pointer_type);

        TypeTable::VOID_ID
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        TypeTable::ERROR_ID
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
        type_id
    }

    fn let_declaration(
        &mut self,
        let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let expr_id = self.visit_expr(expr);
        self.variables.set_variable_type_id(name, expr_id);

        let pointer_type = self
            .types
            .new_type(Type::Pointer(StorageClass::UniformConstant, expr_id));
        self.types.set_type_id(let_id, pointer_type);

        TypeTable::VOID_ID
    }

    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out {
        let expr_type_id = self.visit_expr(right);
        let expr_type = &self.types[expr_type_id];
        let var_type_id = self.variables[name];
        let var_type = &self.types[var_type_id];

        if var_type != expr_type && expr_type != &Type::Error && var_type != &Type::Error {
            let span = ast_span::get_expr_span(right, self.spans);
            self.errors.push(CoffinError::MismatchedTypes {
                span,
                expected: format!("{}", var_type),
                got: format!("{}", expr_type),
            });
        }

        self.types.set_type_id(id, TypeTable::VOID_ID);
        TypeTable::VOID_ID
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let type_id = self.variables[name];
        self.types.set_type_id(name.id, type_id);

        type_id
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types.set_type_id(id, TypeTable::FLOAT_ID);
        TypeTable::FLOAT_ID
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types.set_type_id(id, TypeTable::INT_ID);
        TypeTable::INT_ID
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        let type_id = match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(expr_id) => expr_id,
            None => TypeTable::VOID_ID,
        };
        self.types.set_type_id(id, type_id);
        type_id
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        TypeTable::ERROR_ID
    }
}
