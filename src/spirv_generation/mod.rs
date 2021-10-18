mod spirv_ids_collections;

use crate::{
    ast::{
        AccessType, Ast, Attrs, BinOpKind, Expr, ExprVisitor, ExprVisitorSimple, Field, Id, Item,
        ItemVisitor, ItemVisitorSimple, Name,
    },
    error::{internal_error, CoffinError, InternalError},
    lexer::Token,
    name_resolution::NameTable,
    type_id::{builtin_types, TypeId},
    type_resolution::types::{Type, TypeTable},
};
use lasso::RodeoReader;
use rspirv::{
    dr::{self, Builder, Module},
    spirv::{self, StorageClass},
};
use spirv_ids_collections::{TypeSpirvIds, VariableSpirvIds};

pub fn visit<'ast>(
    ast: &'ast mut Ast,
    variables: &'_ NameTable,
    types: &'_ TypeTable,
) -> Result<Module, &'ast Vec<CoffinError>> {
    // You can't generate code if there are any errors
    if !ast.errors.is_empty() {
        return Err(&ast.errors);
    }

    let mut spirv = SpirvGen {
        names: variables,
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
            if let Item::Uniform(_, attrs, field) = item {
                match ItemVisitorSimple::uniform(&mut spirv, attrs, field) {
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
    names: &'vars NameTable,
    spirv_vars: VariableSpirvIds,

    types: &'types TypeTable,
    spirv_types: TypeSpirvIds,

    rodeo: &'ast RodeoReader,

    uniforms: Vec<u32>,
    code: Builder,
}

impl SpirvGen<'_, '_, '_> {
    fn spirv_type_id(&mut self, type_id: TypeId, storage_class: Option<StorageClass>) -> u32 {
        // Check if we already creates spirv id for this type
        if self.spirv_types[type_id] == 0 {
            self.spirv_types[type_id] = match &self.types[type_id] {
                Type::Unit => self.code.type_void(),
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
                Type::Fun(fun) => {
                    let param_types: Vec<_> = fun
                        .get_param_types()
                        .iter()
                        .map(|id| self.spirv_type_id(*id, None))
                        .collect();

                    let return_type = self.spirv_type_id(fun.get_return_type(), None);

                    self.code.type_function(return_type, param_types)
                }
                Type::Vector(names, id) => {
                    let count = names.len();
                    let spirv_type = self.spirv_type_id(*id, None);
                    self.code.type_vector(spirv_type, count as u32)
                }
            };
        }

        match storage_class {
            None => self.spirv_types[type_id],

            Some(storage_class) => match self.spirv_types.get_pointer(type_id, storage_class) {
                Some(id) => id,
                None => {
                    let id = self
                        .code
                        .type_pointer(None, storage_class, self.spirv_types[type_id]);

                    self.spirv_types.set_pointer(type_id, storage_class, id);

                    id
                }
            },
        }
    }
}

impl ItemVisitorSimple for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;

    fn fun(
        &mut self,
        attrs: &Attrs,
        name: Name,
        params: &[Field],
        _ret: Option<Name>,
        body: &Expr,
    ) -> Self::Out {
        let var_id = self
            .names
            .var_id(name)
            .ice_expect("No variableId for function name.");
        let (type_id, _) = self.types.var_type_id(var_id);
        let fun_type_spirv_id = self.spirv_type_id(type_id, None);
        let return_type = match &self.types[type_id] {
            Type::Fun(fun) => self.spirv_type_id(fun.get_return_type(), None),
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
            let var_id = self
                .names
                .var_id(params[0].name)
                .ice_expect("No variable id.");

            let (type_id, storage_class) = self.types.var_type_id(var_id);
            let spirv_type_id = self.spirv_type_id(type_id, Some(storage_class));

            self.spirv_vars[var_id] = self.code.variable(spirv_type_id, None, storage_class, None);

            self.code.decorate(
                self.spirv_vars[var_id],
                spirv::Decoration::BuiltIn,
                [dr::Operand::BuiltIn(spirv::BuiltIn::GlobalInvocationId)],
            );

            self.uniforms.push(self.spirv_vars[var_id]);

            self.code.entry_point(
                spirv::ExecutionModel::GLCompute,
                fun_spirv_id,
                self.rodeo.resolve(&name.spur),
                &self.uniforms,
            );

            match compute[0].1[..] {
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

    fn uniform(&mut self, attrs: &Attrs, field: &Field) -> Self::Out {
        let var_id = self.names.var_id(field.name).ice_expect("No variable id.");
        let (type_id, storage_class) = self.types.var_type_id(var_id);
        let type_id = self.spirv_type_id(type_id, Some(storage_class));
        self.spirv_vars[var_id] = self.code.variable(type_id, None, storage_class, None);

        let binding = attrs.get_attr(self.rodeo.get("binding"));
        if binding.len() != 1 {
            todo!("Binding error.")
        }
        match binding[0].1[..] {
            [_, (_, Token::Int(i)), _] => {
                self.code.decorate(
                    self.spirv_vars[var_id],
                    spirv::Decoration::Binding,
                    &[dr::Operand::LiteralInt32(i as u32)],
                );
                // Hack
                self.code.decorate(
                    self.spirv_vars[var_id],
                    spirv::Decoration::DescriptorSet,
                    &[dr::Operand::LiteralInt32(0u32)],
                );
            }
            _ => todo!("Binding error."),
        }

        Ok(self.spirv_vars[var_id])
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {
        internal_error("Spirv generation shouldn't be called with errors.")
    }
}

impl ExprVisitorSimple for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let type_id = self.types.type_id(id);
        let spirv_type = self.spirv_type_id(type_id, None);

        let left_type = &self.types[left.get_id()];
        let right_type = &self.types[right.get_id()];

        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        match (left_type, kind, right_type) {
            (Type::Int, BinOpKind::Add, Type::Int) => {
                self.code.i_add(spirv_type, None, left, right)
            }
            (Type::Int, BinOpKind::Sub, Type::Int) => {
                self.code.i_sub(spirv_type, None, left, right)
            }
            (Type::Int, BinOpKind::Mul, Type::Int) => {
                self.code.i_mul(spirv_type, None, left, right)
            }
            (Type::Int, BinOpKind::Div, Type::Int) => {
                self.code.s_div(spirv_type, None, left, right)
            }
            (Type::Int, BinOpKind::Rem, Type::Int) => {
                self.code.s_rem(spirv_type, None, left, right)
            }
            (Type::Vector(_, _), BinOpKind::Add, Type::Vector(_, inner))
                if *inner == builtin_types::INT_ID =>
            {
                self.code.i_add(spirv_type, None, left, right)
            }
            (Type::Vector(_, _), BinOpKind::Sub, Type::Vector(_, inner))
                if *inner == builtin_types::INT_ID =>
            {
                self.code.i_sub(spirv_type, None, left, right)
            }
            (Type::Float, BinOpKind::Add, Type::Float) => {
                self.code.f_add(spirv_type, None, left, right)
            }
            (Type::Float, BinOpKind::Sub, Type::Float) => {
                self.code.f_sub(spirv_type, None, left, right)
            }
            (Type::Float, BinOpKind::Mul, Type::Float) => {
                self.code.f_mul(spirv_type, None, left, right)
            }
            (Type::Float, BinOpKind::Div, Type::Float) => {
                self.code.f_div(spirv_type, None, left, right)
            }
            (Type::Float, BinOpKind::Rem, Type::Float) => {
                self.code.f_rem(spirv_type, None, left, right)
            }
            (Type::Vector(_, _), BinOpKind::Add, Type::Vector(_, inner))
                if *inner == builtin_types::FLOAT_ID =>
            {
                self.code.f_add(spirv_type, None, left, right)
            }
            (Type::Vector(_, _), BinOpKind::Sub, Type::Vector(_, inner))
                if *inner == builtin_types::FLOAT_ID =>
            {
                self.code.f_sub(spirv_type, None, left, right)
            }
            _ => internal_error("Incorrect types and/or operation in binary."),
        }
    }

    fn r#let(&mut self, _id: Id, r#_mut: bool, name: Name, expr: &Expr) -> Self::Out {
        let var_id = self.names.var_id(name).ice_expect("No variable id.");
        let (type_id, storage_class) = self.types.var_type_id(var_id);
        let type_id = self.spirv_type_id(type_id, Some(storage_class));
        self.spirv_vars[var_id] = self.code.variable(type_id, None, storage_class, None);

        let id = self.visit_expr(expr)?;
        self.code.store(self.spirv_vars[var_id], id, None, &[])?;

        Ok(0) // TODO: should return void id
    }

    fn access(&mut self, _id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out {
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
                                    .ice_expect("No member in vector.")
                                    as u32
                            })
                            .collect();

                        type_id = self.types.type_id(*id);
                        let spirv_type = self.spirv_type_id(type_id, None);

                        spirv_id = if indices.len() == 1 {
                            self.code.composite_extract(
                                spirv_type,
                                None,
                                spirv_id,
                                &[indices[0]],
                            )?
                        } else {
                            self.code
                                .vector_shuffle(spirv_type, None, spirv_id, spirv_id, &indices)?
                        };
                    }
                    _ => internal_error("Type without fields."),
                },
                AccessType::Index(_, _) => todo!(),
            }
        }
        Ok(spirv_id)
    }

    fn assign(&mut self, _id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out {
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
        let var_id = self.names.var_id(name).ice_expect("No variable id.");
        let (type_id, _) = self.types.var_type_id(var_id);
        let type_id = self.spirv_type_id(type_id, None);
        let var = self.spirv_vars[var_id];
        self.code.load(type_id, None, var, None, [])
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        let type_id = self.spirv_type_id(self.types.type_id(id), None);
        Ok(self.code.constant_f32(type_id, f))
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        let type_id = self.spirv_type_id(self.types.type_id(id), None);
        Ok(self.code.constant_u32(type_id, i as u32))
    }

    fn block(&mut self, _id: Id, exprs: &[Expr]) -> Self::Out {
        match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(id) => id,
            None => Ok(0), // TODO: should return void id
        }
    }

    fn convert(&mut self, id: Id, expr: &Expr, r#_type: Name) -> Self::Out {
        let type_before = &self.types[expr.get_id()];
        let type_after = &self.types[id];
        let spirv_type_id = self.spirv_type_id(self.types.type_id(id), None);

        let expr_id = self.visit_expr(expr)?;

        match (type_before, type_after) {
            (Type::Int, Type::Float) => self.code.convert_s_to_f(spirv_type_id, None, expr_id),
            (Type::UInt, Type::Float) => self.code.convert_u_to_f(spirv_type_id, None, expr_id),
            (Type::Vector(_, builtin_types::INT_ID), Type::Vector(_, builtin_types::FLOAT_ID)) => {
                self.code.convert_s_to_f(spirv_type_id, None, expr_id)
            }
            (Type::Vector(_, builtin_types::UINT_ID), Type::Vector(_, builtin_types::FLOAT_ID)) => {
                self.code.convert_u_to_f(spirv_type_id, None, expr_id)
            }
            (Type::Vector(_, builtin_types::UINT_ID), Type::Vector(_, builtin_types::INT_ID)) => {
                self.code.bitcast(spirv_type_id, None, expr_id)
            }
            _ => todo!("Conversion error."),
        }
    }

    fn call(&mut self, _id: Id, name: Name, args: &[Expr]) -> Self::Out {
        if let Some(_var_id) = self.names.var_id(name) {
            todo!("Function calls not supported")
        } else if let Some(type_id) = self.names.type_id(name) {
            let spirv_type_id = self.spirv_type_id(type_id, None);
            let args = args.iter().try_fold(vec![], |mut v, e| {
                v.push(self.visit_expr(e)?);
                Ok(v)
            })?;

            match &self.types[type_id] {
                Type::Vector(_, _) => self.code.composite_construct(spirv_type_id, None, &args),
                _ => todo!(),
            }
        } else {
            todo!("err");
        }
    }

    fn r#if(
        &mut self,
        _id: Id,
        _condition: &Expr,
        _block: &Expr,
        r#_else: Option<&Expr>,
    ) -> Self::Out {
        todo!()
    }

    fn expr_error(&mut self, _id: Id) -> Self::Out {
        internal_error("Spirv generation shouldn't be called with errors.")
    }
}
