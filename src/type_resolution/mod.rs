pub mod types;

use self::types::{Type, TypeTable};
use crate::{
    ast::{
        self, Ast, Attrs, BinOpKind, Expr, ExprVisitor, ExprVisitorSimple, Field, Id, ItemVisitor,
        ItemVisitorSimple, Name,
    },
    ast_span,
    error::{CoffinError, InternalError},
    name_resolution::NameTable,
    parser::spans_table::SpanTable,
    type_id::{builtin, TypeId},
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
    fn access_type(&mut self, access: &[AccessType], mut type_id: TypeId) -> TypeId {
        for (i, a) in access.iter().enumerate() {
            match a {
                AccessType::Dot(id, member) => {
                    match &self.types[type_id] {
                        Type::Vector(members, vec_type) => {
                            if i != access.len() - 1 {
                                self.errors.push(CoffinError::SwizzleNotAtTheEnd(
                                    self.spans.get(member.id),
                                ));
                                return builtin::ERROR_ID;
                            }

                            let member_str = self.rodeo.resolve(&member.spur);

                            if member_str.chars().all(|c| members.contains(&c)) {
                                type_id = if member_str.chars().count() == 1 {
                                    *vec_type
                                } else {
                                    match *vec_type {
                                        builtin::INT_ID => {
                                            builtin::IVEC_ID[member_str.chars().count()]
                                        }
                                        builtin::FLOAT_ID => {
                                            builtin::FVEC_ID[member_str.chars().count()]
                                        }
                                        builtin::UINT_ID => {
                                            builtin::UVEC_ID[member_str.chars().count()]
                                        }
                                        _ => todo!(),
                                    }
                                };
                            } else {
                                self.errors.push(CoffinError::IncorrectVectorFields(
                                    self.spans.get(member.id),
                                ));
                                return builtin::ERROR_ID;
                            }
                        }

                        _ => {
                            self.errors
                                .push(CoffinError::TypeDoesntHaveFields(self.spans.get(member.id)));
                            return builtin::ERROR_ID;
                        }
                    };
                    self.types.set_type_id(*id, type_id);
                }
                AccessType::Index(id, expr) => {
                    let expr_type = self.visit_expr(expr);

                    match &self.types[type_id] {
                        &Type::Vector(_, vec_type) => {
                            if expr_type == builtin::INT_ID {
                                type_id = vec_type;
                            } else {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(expr, self.spans),
                                    expected: self.types[builtin::INT_ID].to_string(),
                                    got: self.types[expr_type].to_string(),
                                });
                                return builtin::ERROR_ID;
                            }
                        }
                        Type::Image() => {
                            if expr_type != builtin::IVEC_ID[2] {
                                self.errors.push(CoffinError::MismatchedType {
                                    span: ast_span::get_expr_span(expr, self.spans),
                                    expected: self.types[builtin::IVEC_ID[2]].to_string(),
                                    got: self.types[expr_type].to_string(),
                                });
                                return builtin::ERROR_ID;
                            } else {
                                type_id = builtin::FVEC_ID[4];
                            }
                        }
                        _ => {
                            self.errors
                                .push(CoffinError::TypeCantBeIndexed(self.spans.get(*id)));
                            return builtin::ERROR_ID;
                        }
                    };
                    self.types.set_type_id(*id, type_id);
                }
            }
        }
        type_id
    }
}

impl ItemVisitorSimple for TypeResolution<'_, '_> {
    type Out = ();

    fn fun(
        &mut self,
        attrs: &Attrs,
        name: Name,
        params: &[Field],
        ret: Option<Name>,
        body: &Expr,
    ) -> Self::Out {
        let mut param_types = vec![];

        let compute = attrs.get_attr(self.rodeo.get("compute"));
        if compute.is_empty() {
            for param in params {
                let type_id = self
                    .names
                    .type_id(param.r#type)
                    .unwrap_or(builtin::ERROR_ID);
                param_types.push(type_id);

                if let Some(var_id) = self.names.var_id(param.name) {
                    self.types
                        .set_var_type_id(var_id, type_id, StorageClass::Function);
                }
            }
        } else if compute.len() == 1 {
            if params.len() != 1 {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans.get(name.id),
                    ),
                );
                return;
            }
            let id_param = params[0];

            let type_id = self
                .names
                .type_id(id_param.r#type)
                .unwrap_or(builtin::ERROR_ID);

            if type_id != builtin::ID_ID {
                self.errors.push(
                    CoffinError::ComputeFunctionMustHaveOnlyOneParameterOfTypeId(
                        self.spans.get(id_param.r#type.id),
                    ),
                );
                return;
            }

            if let Some(var_id) = self.names.var_id(id_param.name) {
                self.types
                    .set_var_type_id(var_id, type_id, StorageClass::Input);
            }
        } else if compute.len() > 1 {
            self.errors.push(CoffinError::MoreThanOneAttribute(
                "compute".into(),
                self.spans.get(compute[1].0.id),
            ));
            return;
        }

        let return_type = match ret {
            Some(r#type) => self.names.type_id(r#type).unwrap_or(builtin::ERROR_ID),
            None => builtin::UNIT_ID,
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

        self.types.set_type_id(name.id, type_id);
        self.visit_expr(body);
    }

    fn uniform(&mut self, _attrs: &Attrs, field: &Field) -> Self::Out {
        let type_id = self
            .names
            .type_id(field.r#type)
            .unwrap_or(builtin::ERROR_ID);
        if let Some(var_id) = self.names.var_id(field.name) {
            self.types
                .set_var_type_id(var_id, type_id, StorageClass::UniformConstant);
        }

        self.types.set_type_id(field.name.id, builtin::UNIT_ID);
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, builtin::ERROR_ID);
    }
}

impl ExprVisitorSimple for TypeResolution<'_, '_> {
    type Out = TypeId;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let left_id = self.visit_expr(left);
        let right_id = self.visit_expr(right);

        let left_type = &self.types[left_id];
        let right_type = &self.types[right_id];

        let type_id = match (left_type, kind, right_type) {
            (Type::Int, _, Type::Int) => builtin::INT_ID,
            (Type::Float, _, Type::Float) => builtin::FLOAT_ID,
            (Type::Vector(vl, tl), BinOpKind::Add | BinOpKind::Sub, Type::Vector(vr, tr))
                if vl.len() == vr.len() && tr == tl =>
            {
                left_id
            }
            (Type::Error, _, _) => builtin::ERROR_ID,
            (_, _, Type::Error) => builtin::ERROR_ID,
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

                builtin::ERROR_ID
            }
        };

        self.types.set_type_id(id, type_id)
    }

    fn r#let(&mut self, id: Id, r#_mut: bool, name: Name, expr: &Expr) -> Self::Out {
        let type_id = self.visit_expr(expr);
        if let Some(var_id) = self.names.var_id(name) {
            self.types
                .set_var_type_id(var_id, type_id, StorageClass::Function);
        }

        self.types.set_type_id(id, builtin::UNIT_ID)
    }

    fn access(&mut self, id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out {
        let type_id = self.visit_expr(expr);
        let type_id = self.access_type(access, type_id);
        self.types.set_type_id(id, type_id)
    }

    fn assign(&mut self, id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out {
        let expr_type_id = self.visit_expr(right);
        let var_type_id = self.visit_expr(left);
        let var_type_id = self.access_type(access, var_type_id);

        if var_type_id != expr_type_id
            && expr_type_id != builtin::ERROR_ID
            && var_type_id != builtin::ERROR_ID
        {
            let span = ast_span::get_expr_span(right, self.spans);
            self.errors.push(CoffinError::MismatchedType {
                span,
                expected: format!("{}", self.types[var_type_id]),
                got: format!("{}", self.types[expr_type_id]),
            });
        }

        self.types.set_type_id(id, builtin::UNIT_ID)
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        if let Some(var_id) = self.names.var_id(name) {
            let (type_id, _) = self.types.var_type_id(var_id);
            self.types.set_type_id(name.id, type_id)
        } else {
            builtin::ERROR_ID
        }
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.types.set_type_id(id, builtin::FLOAT_ID)
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.types.set_type_id(id, builtin::INT_ID)
    }

    fn block(&mut self, id: Id, exprs: &[Expr]) -> Self::Out {
        let type_id = match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(expr_id) => expr_id,
            None => builtin::UNIT_ID,
        };
        self.types.set_type_id(id, type_id)
    }

    fn convert(&mut self, id: Id, expr: &Expr, r#type: Name) -> Self::Out {
        let expr_id = self.visit_expr(expr);
        let type_id = self.names.type_id(r#type).unwrap_or(builtin::ERROR_ID);

        let expr_type = &self.types[expr_id];
        let after_type = &self.types[type_id];

        match (expr_type, after_type) {
            (Type::Int, Type::Float) | (Type::UInt, Type::Float) => {
                self.types.set_type_id(id, type_id)
            }
            (Type::Vector(mem0, _), Type::Vector(mem1, _)) if mem0.len() == mem1.len() => {
                self.types.set_type_id(id, type_id)
            }
            _ => todo!(
                "Convert type error, before: {:?}, after: {:?}",
                expr_type,
                after_type
            ),
        }
    }

    fn call(&mut self, id: Id, name: Name, args: &[Expr]) -> Self::Out {
        if let Some(_var_id) = self.names.var_id(name) {
            todo!("Function calls not supported")
        } else if let Some(type_id) = self.names.type_id(name) {
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

            self.types.set_type_id(id, type_id)
        } else {
            todo!("err");
        }
    }

    fn r#if(
        &mut self,
        id: Id,
        _condition: &Expr,
        _block: &Expr,
        r#_else: Option<&Expr>,
    ) -> Self::Out {
        self.types.set_type_id(id, builtin::UNIT_ID)
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.types.set_type_id(id, builtin::ERROR_ID)
    }
}
