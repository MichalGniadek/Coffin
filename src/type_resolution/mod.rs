pub mod types;

use self::types::{Type, TypeTable};
use crate::{
    ast::{self, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, ItemVisitor, Name},
    ast_span,
    error::{CoffinError, InternalError},
    name_resolution::NameTable,
    parser::spans_table::SpanTable,
    type_id::{builtin_types, TypeId},
    type_resolution::types::FunType,
};
use ast::AccessType;
use lasso::RodeoReader;
use rspirv::spirv::StorageClass;

pub fn visit(ast: &mut Ast, names: &NameTable) -> TypeTable {
    let mut tr = TypeResolution {
        names,
        types: TypeTable::new(ast, names),

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
    names: &'vars NameTable,
    types: TypeTable,

    rodeo: &'ast RodeoReader,
    spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl TypeResolution<'_, '_> {
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
                                return builtin_types::ERROR_ID;
                            }

                            let member_str = self.rodeo.resolve(&member.spur);

                            if member_str.chars().all(|c| members.contains(&c)) {
                                if member_str.chars().count() == 1 {
                                    type_id = *vec_type
                                } else if vec_type == &builtin_types::INT_ID {
                                    type_id = builtin_types::IVEC_ID[member_str.chars().count()]
                                } else if vec_type == &builtin_types::FLOAT_ID {
                                    type_id = builtin_types::FVEC_ID[member_str.chars().count()]
                                } else if vec_type == &builtin_types::UINT_ID {
                                    type_id = builtin_types::UVEC_ID[member_str.chars().count()]
                                } else {
                                    todo!()
                                }
                            } else {
                                self.errors.push(CoffinError::IncorrectVectorFields(
                                    self.spans[member.id].clone(),
                                ));
                                return builtin_types::ERROR_ID;
                            }
                        }

                        _ => {
                            self.errors.push(CoffinError::TypeDoesntHaveFields(
                                self.spans[member.id].clone(),
                            ));
                            return builtin_types::ERROR_ID;
                        }
                    };
                    self.types.set_type_id(*id, type_id);
                }
                AccessType::Index(id, expr) => {
                    let expr_type = self.visit_expr(expr);

                    match &self.types[type_id] {
                        &Type::Vector(_, vec_type) => {
                            if expr_type == builtin_types::INT_ID {
                                type_id = vec_type;
                            } else {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(&expr, &self.spans),
                                    expected: format!("{}", self.types[builtin_types::INT_ID]),
                                    got: format!("{}", self.types[expr_type]),
                                });
                                return builtin_types::ERROR_ID;
                            }
                        }
                        Type::Image() => {
                            if expr_type != builtin_types::IVEC_ID[2] {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(&expr, &self.spans),
                                    expected: format!("{}", self.types[builtin_types::IVEC_ID[2]]),
                                    got: format!("{}", self.types[expr_type]),
                                });
                                return builtin_types::ERROR_ID;
                            } else {
                                type_id = builtin_types::FVEC_ID[4];
                            }
                        }
                        _ => {
                            self.errors
                                .push(CoffinError::TypeCantBeIndexed(self.spans[*id].clone()));
                            return builtin_types::ERROR_ID;
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
        name: Name,
        paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let mut param_types = vec![];

        let compute = attrs.get_attr(self.rodeo.get("compute"));
        if compute.len() == 0 {
            for field in params {
                let type_id = self.names.type_id(field.ttpe);
                param_types.push(type_id);

                if let Some(var_id) = self.names.var_id(field.name) {
                    self.types
                        .set_var_type_id(var_id, type_id, StorageClass::Function);
                }
            }
        } else if compute.len() == 1 {
            if params.len() != 1 {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans[paren_id].clone(),
                    ),
                );
                return builtin_types::ERROR_ID;
            }
            let id_param = params[0];

            let type_id = self.names.type_id(id_param.ttpe);
            if type_id != builtin_types::ID_ID {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans[id_param.ttpe.id].clone(),
                    ),
                );
                return builtin_types::ERROR_ID;
            }

            if let Some(var_id) = self.names.var_id(id_param.name) {
                self.types
                    .set_var_type_id(var_id, type_id, StorageClass::Input);
            }
        } else if compute.len() > 1 {
            self.errors.push(CoffinError::MoreThanOneAttribute(
                "compute".into(),
                self.spans[compute[1].0.id].clone(),
            ));
            return builtin_types::ERROR_ID;
        }

        let return_type = match ret {
            Some((_, ttpe)) => self.names.type_id(*ttpe),
            None => builtin_types::VOID_ID,
        };

        let type_id = self
            .types
            .new_type(Type::Fun(FunType::new(return_type, param_types)));

        let var_id = self
            .names
            .var_id(name)
            .ice_expect("No variable id for fun name.");

        // Storage class here is hacky
        self.types
            .set_var_type_id(var_id, type_id, StorageClass::UniformConstant);

        self.types.set_type_id(fun_id, builtin_types::VOID_ID);
        self.visit_expr(body);
        builtin_types::VOID_ID
    }

    fn uniform(&mut self, unif_id: Id, _attrs: &Attrs, field: &Field) -> Self::Out {
        let type_id = self.names.type_id(field.ttpe);
        if let Some(var_id) = self.names.var_id(field.name) {
            self.types
                .set_var_type_id(var_id, type_id, StorageClass::UniformConstant);
        }

        self.types.set_type_id(unif_id, builtin_types::VOID_ID);
        builtin_types::VOID_ID
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, builtin_types::ERROR_ID);
        builtin_types::ERROR_ID
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
            (Type::Int, _, Type::Int) => builtin_types::INT_ID,
            (Type::Float, _, Type::Float) => builtin_types::FLOAT_ID,
            (Type::Vector(vl, tl), BinOpKind::Add | BinOpKind::Sub, Type::Vector(vr, tr))
                if vl.len() == vr.len() && tr == tl =>
            {
                left_id
            }
            (Type::Error, _, _) => builtin_types::ERROR_ID,
            (_, _, Type::Error) => builtin_types::ERROR_ID,
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

                builtin_types::ERROR_ID
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
        let type_id = self.visit_expr(expr);
        if let Some(var_id) = self.names.var_id(name) {
            self.types
                .set_var_type_id(var_id, type_id, StorageClass::Function);
        }

        self.types.set_type_id(let_id, builtin_types::VOID_ID);
        builtin_types::VOID_ID
    }

    fn access(&mut self, id: Id, expr: &Expr, access: &Vec<AccessType>) -> Self::Out {
        let type_id = self.visit_expr(expr);
        let type_id = self.access_type(access, type_id);
        self.types.set_type_id(id, type_id);
        type_id
    }

    fn assign(&mut self, id: Id, left: &Expr, access: &Vec<AccessType>, right: &Expr) -> Self::Out {
        let expr_type_id = self.visit_expr(right);
        let var_type_id = self.visit_expr(left);
        let var_type_id = self.access_type(access, var_type_id);

        if var_type_id != expr_type_id
            && expr_type_id != builtin_types::ERROR_ID
            && var_type_id != builtin_types::ERROR_ID
        {
            let span = ast_span::get_expr_span(right, self.spans);
            self.errors.push(CoffinError::MismatchedType {
                span,
                expected: format!("{}", self.types[var_type_id]),
                got: format!("{}", self.types[expr_type_id]),
            });
        }

        self.types.set_type_id(id, builtin_types::VOID_ID);
        builtin_types::VOID_ID
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        if let Some(var_id) = self.names.var_id(name) {
            let (type_id, _) = self.types.var_type_id(var_id);
            self.types.set_type_id(name.id, type_id);
            type_id
        } else {
            builtin_types::ERROR_ID
        }
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types.set_type_id(id, builtin_types::FLOAT_ID);
        builtin_types::FLOAT_ID
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types.set_type_id(id, builtin_types::INT_ID);
        builtin_types::INT_ID
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        let type_id = match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(expr_id) => expr_id,
            None => builtin_types::VOID_ID,
        };
        self.types.set_type_id(id, type_id);
        type_id
    }

    fn convert(&mut self, id: Id, expr: &Expr, ttpe: Name) -> Self::Out {
        let expr_id = self.visit_expr(expr);
        let type_id = self.names.type_id(ttpe);

        let expr_type = &self.types[expr_id];
        let after_type = &self.types[type_id];

        match (expr_type, after_type) {
            (Type::Int, Type::Float) | (Type::UInt, Type::Float) => {
                self.types.set_type_id(id, type_id);
                type_id
            }
            (Type::Vector(mem0, _), Type::Vector(mem1, _)) if mem0.len() == mem1.len() => {
                self.types.set_type_id(id, type_id);
                type_id
            }
            _ => todo!(
                "Convert type error, before: {:?}, after: {:?}",
                expr_type,
                after_type
            ),
        }
    }

    fn call(&mut self, id: Id, name: Name, args: &Vec<Expr>) -> Self::Out {
        if let Some(_var_id) = self.names.var_id(name) {
            todo!("Function calls not supported")
        } else if self.names.type_id(name) != builtin_types::ERROR_ID {
            let type_id = self.names.type_id(name);
            let args: Vec<TypeId> = args.iter().map(|e| self.visit_expr(e)).collect();
            match &self.types[type_id] {
                Type::Vector(members, inner) => {
                    if args.len() != members.len() {
                        todo!("err")
                    }

                    if args.iter().any(|t| t != inner) {
                        todo!("err")
                    }
                }
                _ => todo!(),
            }

            self.types.set_type_id(id, type_id);
            type_id
        } else {
            todo!("err");
        }
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, builtin_types::ERROR_ID);
        builtin_types::ERROR_ID
    }
}
