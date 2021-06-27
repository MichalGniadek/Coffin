mod spirv_ids_collections;

use crate::{
    ast::{
        AccessType, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, Item, ItemVisitor, Name,
    },
    error::{internal_error, InternalError},
    lexer::Token,
    name_resolution::VariableTable,
    type_resolution::types::{Type, TypeId, TypeTable},
};
use lasso::RodeoReader;
use rspirv::{
    dr::{self, Builder, Module},
    spirv,
};
use spirv_ids_collections::{TypeSpirvIds, VariableSpirvIds};

pub fn visit(ast: &mut Ast, variables: &VariableTable, types: &TypeTable) -> Result<Module, ()> {
    // You can't generate code if there are any errors
    if !ast.errors.is_empty() {
        return Err(());
    }

    let mut spirv = SpirvGen {
        variables,
        spirv_vars: VariableSpirvIds::new(variables),

        types,
        spirv_types: TypeSpirvIds::new(types),

        rodeo: &ast.rodeo,

        // Will be filled during a prepass
        uniforms: vec![],
        code: Builder::new(),
    };

    spirv.code.set_version(1, 5);
    spirv.code.capability(spirv::Capability::Shader);
    spirv
        .code
        .memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::GLSL450);

    // Uniform collection prepass
    spirv.uniforms = ast
        .items
        .iter()
        .filter_map(|item| {
            if let Item::Uniform(unif_id, attrs, field) = item {
                match spirv.uniform(*unif_id, attrs, field) {
                    Ok(id) => Some(id),
                    Err(err) => internal_error(&format!("Builder error: {}", err)),
                }
            } else {
                None
            }
        })
        .collect();

    // Standard pass without uniforms
    for item in ast
        .items
        .iter()
        .filter(|item| !matches!(item, Item::Uniform(_, _, _)))
    {
        if let Err(err) = spirv.visit_item(item) {
            internal_error(&format!("Builder error: {}", err));
        }
    }

    Ok(spirv.code.module())
}

struct SpirvGen<'ast, 'vars, 'types> {
    variables: &'vars VariableTable,
    spirv_vars: VariableSpirvIds,

    types: &'types TypeTable,
    spirv_types: TypeSpirvIds,

    rodeo: &'ast RodeoReader,

    uniforms: Vec<u32>,
    code: Builder,
}

impl SpirvGen<'_, '_, '_> {
    fn type_id_to_spirv_id(&mut self, type_id: TypeId) -> u32 {
        if self.spirv_types[type_id] == 0 {
            // Spirv type hasn't been created it, so create it.
            let ttpe = &self.types[type_id];

            let spirv_id = match ttpe {
                Type::Void => self.code.type_void(),
                Type::Error => internal_error("Trying to get spirv id for Type::Error"),
                Type::Int => self.code.type_int(32, 1),
                Type::UInt => self.code.type_int(32, 0),
                Type::Float => self.code.type_float(32),
                Type::Image() => {
                    let float_id = self.code.type_float(32);
                    self.code.type_image(
                        float_id,
                        spirv::Dim::Dim2D,
                        0,
                        0,
                        0,
                        2,
                        spirv::ImageFormat::Rgba8,
                        None,
                    )
                }
                Type::Pointer(storage_class, type_id) => {
                    let type_spirv_id = self.type_id_to_spirv_id(*type_id);
                    self.code.type_pointer(None, *storage_class, type_spirv_id)
                }
                Type::Fun(fun) => {
                    let param_types: Vec<_> = fun
                        .get_param_types()
                        .iter()
                        .map(|id| self.type_id_to_spirv_id(*id))
                        .collect();

                    let return_type = self.type_id_to_spirv_id(fun.get_return_type());

                    self.code.type_function(return_type, param_types)
                }
                Type::Vector(names, id) => {
                    let count = names.len();
                    let spirv_type = self.type_id_to_spirv_id(*id);
                    self.code.type_vector(spirv_type, count as u32)
                }
            };

            self.spirv_types[type_id] = spirv_id;
        }
        self.spirv_types[type_id]
    }
}

impl ItemVisitor for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        name: Name,
        _paren_id: Id,
        params: &Vec<Field>,
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let type_id = self.types.type_id(fun_id);
        let fun_type_spirv_id = self.type_id_to_spirv_id(type_id);
        let return_type = match &self.types[type_id] {
            Type::Fun(fun) => self.type_id_to_spirv_id(fun.get_return_type()),
            _ => internal_error("Function must have a Type::Fun type"),
        };

        let fun_spirv_id = self.code.begin_function(
            return_type,
            None,
            spirv::FunctionControl::NONE,
            fun_type_spirv_id,
        )?;

        let compute = attrs.get_attr(self.rodeo.get("compute"));
        if compute.len() == 1 {
            let compute = compute[0];

            // Checked by type resolution
            let id_param = params[0];
            let var_id = self.variables.var_id(id_param.name);
            let type_id = self.types.var_type_id(var_id);
            let spirv_type_id = self.type_id_to_spirv_id(type_id);
            let id_param =
                self.code
                    .variable(spirv_type_id, None, spirv::StorageClass::Input, None);

            self.spirv_vars[var_id] = id_param;

            self.code.decorate(
                id_param,
                spirv::Decoration::BuiltIn,
                [dr::Operand::BuiltIn(spirv::BuiltIn::GlobalInvocationId)],
            );

            self.uniforms.push(id_param);

            self.code.entry_point(
                spirv::ExecutionModel::GLCompute,
                fun_spirv_id,
                self.rodeo.resolve(&name.spur),
                &self.uniforms,
            );

            match compute.1[..] {
                // Ignore delimiters at each end and ids for each token
                [_, (_, Token::Int(x)), (_, Token::Comma), (_, Token::Int(y)), (_, Token::Comma), (_, Token::Int(z)), _] => {
                    if x <= 0 || y <= 0 || z <= 0 {
                        todo!("Compute arguments must be positive.")
                    } else {
                        self.code.execution_mode(
                            fun_spirv_id,
                            spirv::ExecutionMode::LocalSize,
                            &[x as u32, y as u32, z as u32],
                        );
                    }
                }
                _ => todo!("Compute arguments must have 3 ints seperated by commas."),
            };
        }

        self.code.begin_block(None)?;
        self.visit_expr(body)?;
        self.code.ret()?;
        self.code.end_function()?;

        Ok(fun_spirv_id)
    }

    fn uniform(&mut self, _unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out {
        let var_id = self.variables.var_id(field.name);
        let pointer_id = self.type_id_to_spirv_id(self.types.var_type_id(var_id));
        let var = self
            .code
            .variable(pointer_id, None, spirv::StorageClass::UniformConstant, None);
        self.spirv_vars[var_id] = var;

        let binding = attrs.get_attr(self.rodeo.get("binding"));
        if binding.len() != 1 {
            todo!("Binding error.")
        }
        match binding[0].1[..] {
            [_, (_, Token::Int(i)), _] => {
                self.code.decorate(
                    var,
                    spirv::Decoration::Binding,
                    &[dr::Operand::LiteralInt32(i as u32)],
                );
                // Hack
                self.code.decorate(
                    var,
                    spirv::Decoration::DescriptorSet,
                    &[dr::Operand::LiteralInt32(0 as u32)],
                );
            }
            _ => todo!("Binding error."),
        }

        Ok(var)
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {
        internal_error("Spirv generation shouldn't be called with errors.")
    }
}

impl ExprVisitor for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;
    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let type_id = self.types.type_id(id);
        let spiv_type = self.type_id_to_spirv_id(type_id);
        let ttpe = &self.types[id];
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match (kind, ttpe) {
            (BinOpKind::Add, Type::Int) => self.code.i_add(spiv_type, None, left, right),
            (BinOpKind::Sub, Type::Int) => self.code.i_sub(spiv_type, None, left, right),
            (BinOpKind::Mul, Type::Int) => self.code.i_mul(spiv_type, None, left, right),
            (BinOpKind::Div, Type::Int) => self.code.s_div(spiv_type, None, left, right),
            (BinOpKind::Rem, Type::Int) => self.code.s_rem(spiv_type, None, left, right),
            _ => internal_error("Incorrect types and/or operation in binary."),
        }
    }

    fn let_declaration(
        &mut self,
        _let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let var_id = self.variables.var_id(name);
        let pointer_id = self.type_id_to_spirv_id(self.types.var_type_id(var_id));
        let var = self
            .code
            .variable(pointer_id, None, spirv::StorageClass::UniformConstant, None);
        self.spirv_vars[var_id] = var;

        let id = self.visit_expr(expr)?;
        self.code.store(var, id, None, &[])?;

        Ok(0) // TODO: should return void id
    }

    fn access(&mut self, _id: Id, expr: &Expr, access: &Vec<AccessType>) -> Self::Out {
        let mut type_id = self.types.type_id(expr.get_id());
        let mut spirv_id = self.visit_expr(expr)?;
        for a in access {
            match a {
                AccessType::Dot(id, member) => match &self.types[type_id] {
                    Type::Vector(members, _) => {
                        let indices: Vec<u32> = self
                            .rodeo
                            .resolve(&member.spur)
                            .chars()
                            .map(|c| {
                                members
                                    .iter()
                                    .position(|cc| *cc == c)
                                    .ie_expect("No member in vector.")
                                    as u32
                            })
                            .collect();

                        type_id = self.types.type_id(*id);
                        let spirv_type = self.type_id_to_spirv_id(type_id);

                        spirv_id = self
                            .code
                            .vector_shuffle(spirv_type, None, spirv_id, spirv_id, &indices)?;
                    }
                    _ => internal_error("Type without fields."),
                },
                AccessType::Index(_, _) => todo!(),
            }
        }
        Ok(spirv_id)
    }

    fn assign(
        &mut self,
        _id: Id,
        left: &Expr,
        access: &Vec<AccessType>,
        right: &Expr,
    ) -> Self::Out {
        let right = self.visit_expr(right)?;

        let /*mut*/ type_id = self.types.type_id(left.get_id());

        for a in access {
            match a {
                AccessType::Dot(_, _) => todo!(),
                AccessType::Index(_, expr) => match &self.types[type_id] {
                    Type::Image() => {
                        let expr = self.visit_expr(expr)?;
                        let image = self.visit_expr(left)?;
                        self.code.image_write(image, expr, right, None, &[])?;
                        return Ok(0);
                    }
                    _ => internal_error("Type non indexable."),
                },
            }
        }

        // self.code.store(pointer, right, None, &[])?;

        Ok(0) // TODO: should return void id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let type_id = self.types.type_id(name.id);
        let type_id = self.type_id_to_spirv_id(type_id);
        let var_id = self.variables.var_id(name);
        let var = self.spirv_vars[var_id];
        self.code.load(type_id, None, var, None, [])
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        let type_id = self.type_id_to_spirv_id(self.types.type_id(id));
        Ok(self.code.constant_f32(type_id, f))
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        let type_id = self.type_id_to_spirv_id(self.types.type_id(id));
        Ok(self.code.constant_u32(type_id, i as u32))
    }

    fn block(&mut self, _id: Id, exprs: &Vec<Expr>) -> Self::Out {
        match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(id) => id,
            None => Ok(0), // TODO: should return void id
        }
    }

    fn convert(&mut self, id: Id, expr: &Expr, _ttpe: Name) -> Self::Out {
        let type_before = &self.types[expr.get_id()];
        let type_after = &self.types[id];
        let spirv_type_id = self.type_id_to_spirv_id(self.types.type_id(id));

        let expr_id = self.visit_expr(expr)?;

        match (type_before, type_after) {
            (Type::Vector(_, TypeTable::INT_ID), Type::Vector(_, TypeTable::FLOAT_ID)) => {
                self.code.convert_s_to_f(spirv_type_id, None, expr_id)
            }
            (Type::Vector(_, TypeTable::UINT_ID), Type::Vector(_, TypeTable::FLOAT_ID)) => {
                self.code.convert_u_to_f(spirv_type_id, None, expr_id)
            }
            (Type::Vector(_, TypeTable::UINT_ID), Type::Vector(_, TypeTable::INT_ID)) => {
                self.code.bitcast(spirv_type_id, None, expr_id)
            }
            _ => todo!("Conversion error."),
        }
    }

    fn expr_error(&mut self, _id: Id) -> Self::Out {
        internal_error("Spirv generation shouldn't be called with errors.")
    }
}
