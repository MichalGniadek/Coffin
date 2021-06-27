mod variable_spirv_ids;

use self::variable_spirv_ids::VariableSpirvIds;
use crate::{
    ast::{
        AccessType, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, Item, ItemVisitor, Name,
    },
    error::CoffinError,
    lexer::Token,
    name_resolution::VariableTable,
    parser::spans_table::SpanTable,
    type_resolution::types::{Type, TypeId, TypeTable},
};
use lasso::RodeoReader;
use rspirv::{
    dr::{self, Builder, Module},
    spirv,
};
use std::collections::HashMap;

pub fn visit(ast: &mut Ast, variables: &VariableTable, types: &TypeTable) -> Result<Module, ()> {
    // You can't generate code if there are any errors
    if !ast.errors.is_empty() {
        return Err(());
    }

    let mut spirv = SpirvGen {
        variables: VariableSpirvIds::new(variables),
        types,
        spirv_types: HashMap::new(),
        rodeo: &ast.rodeo,

        // Will be filled durgina a prepass
        uniforms: vec![],
        code: Builder::new(),

        _spans: &ast.spans,
        errors: &mut ast.errors,
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
                    Err(err) => {
                        spirv.internal_error(&format!("Builder error: {}", err));
                        None
                    }
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
            spirv.internal_error(&format!("Builder error: {}", err));
        }
    }

    if !spirv.errors.is_empty() {
        return Err(());
    }

    Ok(spirv.code.module())
}

struct SpirvGen<'ast, 'vars, 'types> {
    variables: VariableSpirvIds<'vars>,
    types: &'types TypeTable,
    spirv_types: HashMap<TypeId, u32>,
    rodeo: &'ast RodeoReader,

    uniforms: Vec<u32>,
    code: Builder,

    _spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl SpirvGen<'_, '_, '_> {
    fn internal_error(&mut self, str: &str) -> u32 {
        self.errors
            .push(CoffinError::InternalError(str.to_owned(), None));
        0
    }

    fn type_id_to_spirv_id(&mut self, type_id: TypeId) -> u32 {
        if let Some(id) = self.spirv_types.get(&type_id) {
            *id
        } else {
            // Spirv type hasn't been created it, so create it.
            let ttpe = &self.types[type_id];

            let spirv_id = match ttpe {
                Type::Void => self.code.type_void(),
                Type::Error => self.internal_error("Trying to get spirv id for Type::Error"),
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

            // Cache it
            self.spirv_types.insert(type_id, spirv_id);
            spirv_id
        }
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
            _ => self.internal_error("Function must have a Type::Fun type"),
        };

        let fun_spirv_id = self.code.begin_function(
            return_type,
            None,
            spirv::FunctionControl::NONE,
            fun_type_spirv_id,
        )?;

        let compute = attrs.get_attr(self.rodeo.get("compute"));
        if compute.len() > 1 {
            todo!("More than one compute attribute")
        } else if compute.len() == 1 {
            let compute = compute[0];

            if params.len() == 0 {
                todo!("Compute shader function must have one parameter of type Id");
            } else if params.len() > 1 {
                todo!("Compute shader function must have one parameter of type Id");
            } else {
                // Next lines are very hacky. Less hacky solution would be to store the type in colon id in a field.
                // Probably still to hacky. Best solution is to make it possible to get types of variables in spirv gen.
                let spirv_type_id = self.type_id_to_spirv_id(TypeTable::ID_ID);
                let pointer_type_id =
                    self.code
                        .type_pointer(None, spirv::StorageClass::Input, spirv_type_id);
                let id_param =
                    self.code
                        .variable(pointer_type_id, None, spirv::StorageClass::Input, None);

                self.variables
                    .set_variable_spirv_id(params[0].name, id_param);

                self.code.decorate(
                    id_param,
                    spirv::Decoration::BuiltIn,
                    [dr::Operand::BuiltIn(spirv::BuiltIn::GlobalInvocationId)],
                );

                self.uniforms.push(id_param);
            }

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

    fn uniform(&mut self, unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out {
        let pointer_id = self.type_id_to_spirv_id(self.types.type_id(unif_id));
        let var = self
            .code
            .variable(pointer_id, None, spirv::StorageClass::UniformConstant, None);
        self.variables.set_variable_spirv_id(field.name, var);

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
        Ok(self.internal_error("Spirv generation shouldn't be called with errors."))
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
            (BinOpKind::Pow, Type::Int) => Ok(0), // pow() requires extensions.
            _ => Ok(self.internal_error("Incorrect types and/or operation in binary.")),
        }
    }

    fn let_declaration(
        &mut self,
        let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let pointer_id = self.type_id_to_spirv_id(self.types.type_id(let_id));
        let var = self
            .code
            .variable(pointer_id, None, spirv::StorageClass::UniformConstant, None);
        self.variables.set_variable_spirv_id(name, var);

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
                                members.iter().position(|cc| *cc == c).unwrap_or_else(|| {
                                    self.internal_error("No member in vector.") as usize
                                }) as u32
                            })
                            .collect();

                        type_id = self.types.type_id(*id);
                        let spirv_type = self.type_id_to_spirv_id(type_id);

                        spirv_id = self
                            .code
                            .vector_shuffle(spirv_type, None, spirv_id, spirv_id, &indices)?;
                    }
                    _ => return Ok(self.internal_error("Type without fields.")),
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
                    _ => return Ok(self.internal_error("Type non indexable.")),
                },
            }
        }

        // self.code.store(pointer, right, None, &[])?;

        Ok(0) // TODO: should return void id
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let type_id = self.types.type_id(name.id);
        let type_id = self.type_id_to_spirv_id(type_id);
        let var = self.variables[name];
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
        Ok(self.internal_error("Spirv generation shouldn't be called with errors."))
    }
}
