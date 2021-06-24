mod variable_spirv_ids;

use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    error::CoffinError,
    lexer::Token,
    name_resolution::VariableTable,
    parser::spans_table::SpanTable,
    type_resolution::types::{Type, TypeId, TypeTable},
};
use lasso::RodeoResolver;
use rspirv::{
    dr::{self, Builder, Module},
    spirv::{self, StorageClass},
};
use std::collections::HashMap;

use self::variable_spirv_ids::VariableSpirvIds;

pub fn visit(ast: &mut Ast, variables: &VariableTable, types: &TypeTable) -> Result<Module, ()> {
    // You can't generate code if there are any errors
    if !ast.errors.is_empty() {
        return Err(());
    }

    let mut spirv = SpirvGen {
        variables: VariableSpirvIds::new(variables),
        types,
        spirv_types: HashMap::new(),
        resolver: &ast.resolver,

        code: Builder::new(),

        _spans: &ast.spans,
        _errors: &mut ast.errors,
    };

    spirv.code.set_version(1, 3);
    spirv
        .code
        .memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::Simple);

    for item in &ast.items {
        let res = spirv.visit_item(item);
        if let Err(err) = res {
            // Remove later and use CoffinError instead of panics
            panic!("Internal compiler error: {}", err);
        }
    }

    Ok(spirv.code.module())
}

struct SpirvGen<'ast, 'vars, 'types> {
    variables: VariableSpirvIds<'vars>,
    types: &'types TypeTable,
    spirv_types: HashMap<TypeId, u32>,
    resolver: &'ast RodeoResolver,

    code: Builder,
    _spans: &'ast SpanTable,
    _errors: &'ast mut Vec<CoffinError>,
}

impl SpirvGen<'_, '_, '_> {
    fn type_id_to_spirv_id(&mut self, type_id: TypeId) -> u32 {
        if let Some(id) = self.spirv_types.get(&type_id) {
            *id
        } else {
            let ttpe = &self.types[type_id];
            let spirv_id = match ttpe {
                Type::Void => self.code.type_void(),
                Type::Error => unreachable!(),
                Type::Int => self.code.type_int(32, 1),
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
            };
            self.spirv_types.insert(type_id, spirv_id);
            spirv_id
        }
    }
}

impl Visitor for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        _params: &Vec<Field>,
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let type_id = self.types.type_id(fun_id);
        let fun_type_spirv_id = self.type_id_to_spirv_id(type_id);
        let return_type = match &self.types[type_id] {
            Type::Fun(fun) => self.type_id_to_spirv_id(fun.get_return_type()),
            _ => unreachable!("Internal compiler error: Function must have FunType type."),
        };

        let fun_spirv_id = self.code.begin_function(
            return_type,
            None,
            spirv::FunctionControl::NONE,
            fun_type_spirv_id,
        )?;

        self.code.begin_block(None)?;
        self.visit_expr(body)?;
        self.code.ret()?;
        self.code.end_function()?;

        let compute = attrs.get_attr("compute", self.resolver);
        if compute.len() > 1 {
            todo!("More than one compute attribute")
        } else if compute.len() == 1 {
            self.code.entry_point(
                spirv::ExecutionModel::GLCompute,
                fun_spirv_id,
                "main_compute",
                &[],
            );

            let tokens = &compute[0].1;
            match tokens[1..tokens.len() - 2] {
                [(_, Token::Int(x)), (_, Token::Comma), (_, Token::Int(y)), (_, Token::Comma), (_, Token::Int(z))] => {
                    if x <= 0 || y <= 0 || z <= 0 {
                        panic!("Compute arguments must be positive.")
                    } else {
                        self.code.execution_mode(
                            fun_spirv_id,
                            spirv::ExecutionMode::LocalSize,
                            &[x as u32, y as u32, z as u32],
                        );
                    }
                }
                _ => panic!("Compute arguments must have 3 ints seperated by commas."),
            };
        }

        Ok(fun_spirv_id)
    }

    fn uniform(&mut self, unif_id: Id, _attrs: &Attrs, field: &Field) -> Self::Out {
        let pointer_id = self.type_id_to_spirv_id(self.types.type_id(unif_id));
        let var = self
            .code
            .variable(pointer_id, None, StorageClass::UniformConstant, None);
        self.variables.set_variable_spirv_id(field.name, var);

        Ok(var)
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {
        // Shouldn't be a panic
        panic!("Internal compiler error: Spirv generation shouldn't be called with errors.")
    }

    fn binary(&mut self, _id: Id, _kind: BinOpKind, _left: &Expr, _right: &Expr) -> Self::Out {
        todo!()
    }

    fn let_declaration(
        &mut self,
        _let_id: Id,
        _mut_id: Option<Id>,
        _name: Name,
        _eq_id: Id,
        _expr: &Expr,
    ) -> Self::Out {
        todo!()
    }

    fn assign(&mut self, _id: Id, _name: Name, _right: &Expr) -> Self::Out {
        todo!()
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        let type_id = self.types.type_id(name.id);
        let type_id = self.type_id_to_spirv_id(type_id);
        let var = self.variables[name];
        self.code.load(type_id, None, var, None, [])
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        let type_id = *self.spirv_types.get(&self.types.type_id(id)).unwrap();
        Ok(self.code.constant_f32(type_id, f))
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        let type_id = *self.spirv_types.get(&self.types.type_id(id)).unwrap();
        Ok(self.code.constant_u32(type_id, i as u32))
    }

    fn block(&mut self, _id: Id, exprs: &Vec<Expr>) -> Self::Out {
        match exprs.iter().map(|e| self.visit_expr(e)).last() {
            Some(id) => id,
            None => unreachable!("Internal compiler error: cannot generate spirv for empty block."),
        }
    }

    fn expr_error(&mut self, _id: Id) -> Self::Out {
        // Shouldn't be a panic
        panic!("Internal compiler error: Spirv generation shouldn't be called with errors.")
    }
}
