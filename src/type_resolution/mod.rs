pub mod types;

use self::types::{Type, TypeId, TypeTable};
use crate::{
    ast::{self, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, ItemVisitor, Name},
    ast_span,
    error::{internal_error, CoffinError},
    name_resolution::VariableTable,
    parser::spans_table::SpanTable,
    type_resolution::types::FunType,
};
use ast::AccessType;
use lasso::RodeoReader;
use rspirv::spirv::StorageClass;

pub fn visit(ast: &mut Ast, variables: &VariableTable) -> TypeTable {
    let mut tr = TypeResolution {
        variables,
        types: TypeTable::new(ast.max_id(), variables.max_var_id()),

        rodeo: &ast.rodeo,
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
    variables: &'vars VariableTable,
    types: TypeTable,

    rodeo: &'ast RodeoReader,
    spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl TypeResolution<'_, '_> {
    fn resolve_type(&mut self, name: &Name) -> TypeId {
        match self.rodeo.resolve(&name.spur) {
            "void" => TypeTable::VOID_ID,
            "int" => TypeTable::INT_ID,
            "int1" => TypeTable::IVEC_ID[1],
            "int2" => TypeTable::IVEC_ID[2],
            "int3" => TypeTable::IVEC_ID[3],
            "int4" => TypeTable::IVEC_ID[4],
            "float1" => TypeTable::FVEC_ID[1],
            "float2" => TypeTable::FVEC_ID[2],
            "float3" => TypeTable::FVEC_ID[3],
            "float4" => TypeTable::FVEC_ID[4],
            "image2d" => TypeTable::IMAGE_ID,
            "Id" => TypeTable::ID_ID,
            _ => {
                let span = self.spans[name.id].clone();
                self.errors.push(CoffinError::UndeclaredType(span));
                TypeTable::ERROR_ID
            }
        }
    }

    fn access_type(&mut self, access: &Vec<AccessType>, mut type_id: TypeId) -> TypeId {
        for (i, a) in access.iter().enumerate() {
            match a {
                AccessType::Dot(id, member) => {
                    match &self.types[type_id] {
                        Type::Vector(members, vec_type) => {
                            if i != access.len() - 1 {
                                self.errors.push(CoffinError::SwizzleNotAtTheEnd(
                                    self.spans[member.id].clone(),
                                ));
                                return TypeTable::ERROR_ID;
                            }

                            let member_str = self.rodeo.resolve(&member.spur);

                            if member_str.chars().all(|c| members.contains(&c)) {
                                if vec_type == &TypeTable::INT_ID {
                                    type_id = TypeTable::IVEC_ID[member_str.chars().count()]
                                } else if vec_type == &TypeTable::FLOAT_ID {
                                    type_id = TypeTable::FVEC_ID[member_str.chars().count()]
                                } else if vec_type == &TypeTable::UINT_ID {
                                    type_id = TypeTable::UVEC_ID[member_str.chars().count()]
                                } else {
                                    todo!()
                                }
                            } else {
                                self.errors.push(CoffinError::IncorrectVectorFields(
                                    self.spans[member.id].clone(),
                                ));
                                return TypeTable::ERROR_ID;
                            }
                        }

                        _ => {
                            self.errors.push(CoffinError::TypeDoesntHaveFields(
                                self.spans[member.id].clone(),
                            ));
                            return TypeTable::ERROR_ID;
                        }
                    };
                    self.types.set_type_id(*id, type_id);
                }
                AccessType::Index(id, expr) => {
                    let expr_type = self.visit_expr(expr);

                    match &self.types[type_id] {
                        &Type::Vector(_, vec_type) => {
                            if expr_type == TypeTable::INT_ID {
                                type_id = vec_type;
                            } else {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(&expr, &self.spans),
                                    expected: format!("{}", self.types[TypeTable::INT_ID]),
                                    got: format!("{}", self.types[expr_type]),
                                });
                                return TypeTable::ERROR_ID;
                            }
                        }
                        Type::Image() => {
                            if expr_type != TypeTable::IVEC_ID[2] {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(&expr, &self.spans),
                                    expected: format!("{}", self.types[TypeTable::IVEC_ID[2]]),
                                    got: format!("{}", self.types[expr_type]),
                                });
                                return TypeTable::ERROR_ID;
                            } else {
                                type_id = TypeTable::FVEC_ID[4];
                            }
                        }
                        _ => {
                            self.errors
                                .push(CoffinError::TypeCantBeIndexed(self.spans[*id].clone()));
                            return TypeTable::ERROR_ID;
                        }
                    };
                    self.types.set_type_id(*id, type_id);
                }
            }
        }
        type_id
    }
}

impl ItemVisitor for TypeResolution<'_, '_> {
    type Out = TypeId;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        _name: Name,
        paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let mut param_types = vec![];

        let compute = attrs.get_attr(self.rodeo.get("compute"));
        if compute.len() == 0 {
            for field in params {
                let type_id = self.resolve_type(&field.ttpe);
                param_types.push(type_id);

                let pointer_type = self
                    .types
                    .new_type(Type::Pointer(StorageClass::Function, type_id));

                let var_id = self.variables.var_id(field.name);
                self.types.set_var_type_id(var_id, pointer_type);
            }
        } else if compute.len() == 1 {
            if params.len() != 1 {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans[paren_id].clone(),
                    ),
                );
                return TypeTable::ERROR_ID;
            }
            let id_param = params[0];

            let type_id = self.resolve_type(&id_param.ttpe);
            if type_id != TypeTable::ID_ID {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans[id_param.ttpe.id].clone(),
                    ),
                );
                return TypeTable::ERROR_ID;
            }

            let pointer_type = self
                .types
                .new_type(Type::Pointer(StorageClass::Input, type_id));

            let var_id = self.variables.var_id(id_param.name);
            self.types.set_var_type_id(var_id, pointer_type);
        } else if compute.len() > 1 {
            self.errors.push(CoffinError::MoreThanOneAttribute(
                "compute".into(),
                self.spans[compute[1].0.id].clone(),
            ));
            return TypeTable::ERROR_ID;
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
        let pointer_type = self
            .types
            .new_type(Type::Pointer(StorageClass::UniformConstant, type_id));

        let var_id = self.variables.var_id(field.name);
        self.types.set_var_type_id(var_id, pointer_type);

        self.types.set_type_id(unif_id, TypeTable::VOID_ID);
        TypeTable::VOID_ID
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        TypeTable::ERROR_ID
    }
}

impl ExprVisitor for TypeResolution<'_, '_> {
    type Out = TypeId;

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
        let pointer_type = self
            .types
            .new_type(Type::Pointer(StorageClass::Function, expr_id));

        let var_id = self.variables.var_id(name);
        self.types.set_var_type_id(var_id, pointer_type);

        self.types.set_type_id(let_id, TypeTable::VOID_ID);
        TypeTable::VOID_ID
    }

    fn access(&mut self, id: Id, expr: &Expr, access: &Vec<AccessType>) -> Self::Out {
        let type_id = self.visit_expr(expr);
        let type_id = self.access_type(access, type_id);
        self.types.set_type_id(id, type_id);
        type_id
    }

    fn assign(&mut self, id: Id, left: &Expr, access: &Vec<AccessType>, right: &Expr) -> Self::Out {
        let expr_type_id = self.visit_expr(right);

        let mut var_type_id = self.visit_expr(left);
        if let Expr::Identifier(name) = left {
            let var_id = self.variables.var_id(*name);
            var_type_id = self.types.var_type_id(var_id);
        }

        let var_type_id = if let Type::Pointer(_, type_id) = &self.types[var_type_id] {
            *type_id
        } else {
            TypeTable::ERROR_ID
        };

        let var_type_id = self.access_type(access, var_type_id);

        if var_type_id != expr_type_id
            && expr_type_id != TypeTable::ERROR_ID
            && var_type_id != TypeTable::ERROR_ID
        {
            let span = ast_span::get_expr_span(right, self.spans);
            self.errors.push(CoffinError::MismatchedType {
                span,
                expected: format!("{}", self.types[var_type_id]),
                got: format!("{}", self.types[expr_type_id]),
            });
        }

        self.types.set_type_id(id, TypeTable::VOID_ID);
        TypeTable::VOID_ID
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let var_id = self.variables.var_id(name);
        let pointer_id = self.types.var_type_id(var_id);
        let type_id = match self.types[pointer_id] {
            Type::Pointer(_, type_id) => type_id,
            Type::Error => TypeTable::ERROR_ID,
            _ => internal_error("Variable types must be pointers."),
        };

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

    fn convert(&mut self, id: Id, expr: &Expr, ttpe: Name) -> Self::Out {
        let expr_id = self.visit_expr(expr);
        let type_id = self.resolve_type(&ttpe);

        let expr_type = &self.types[expr_id];
        let after_type = &self.types[type_id];

        match (expr_type, after_type) {
            (Type::Vector(mem0, _), Type::Vector(mem1, _)) if mem0.len() == mem1.len() => {
                self.types.set_type_id(id, type_id);
                type_id
            }
            _ => todo!("Convert type error"),
        }
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, TypeTable::ERROR_ID);
        TypeTable::ERROR_ID
    }
}
