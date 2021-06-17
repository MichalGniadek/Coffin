use crate::{
    ast::{Ast, Attrs, BinOpKind, Expr, Field, Id, Name, Visitor},
    error::CoffinError,
    name_resolution::{VariableId, VariableTable},
    parser::spans_table::SpanTable,
    type_resolution::types::{Type, TypeId, TypeTable},
};
use rspirv::{
    dr::{self, Builder, Module},
    spirv,
};
use std::collections::HashMap;

pub fn visit(
    ast: &Ast,
    variables: &VariableTable,
    types: &TypeTable,
    spans: &SpanTable,
) -> (Module, Vec<CoffinError>) {
    let mut spirv = SpirvGen {
        variables,
        spirv_vars: HashMap::new(),
        types,
        spirv_types: HashMap::new(),

        code: Builder::new(),

        spans,
        errors: vec![],
    };

    spirv.code.set_version(1, 3);
    spirv
        .code
        .memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::Simple);

    for item in ast {
        let res = spirv.visit_item(item);
        if let Err(err) = res {
            // Remove later and use CoffinError instead of panics
            panic!("Internal compiler error: {}", err);
        }
    }

    (spirv.code.module(), spirv.errors)
}

#[allow(dead_code)]
struct SpirvGen<'a, 'b, 'c> {
    variables: &'a VariableTable,
    spirv_vars: HashMap<VariableId, u32>,
    types: &'b TypeTable,
    spirv_types: HashMap<TypeId, u32>,

    code: Builder,
    // VariableId -> u32
    // TypeId -> u32
    spans: &'c SpanTable,
    errors: Vec<CoffinError>,
}

impl SpirvGen<'_, '_, '_> {
    fn type_id_to_spirv_id(&mut self, type_id: TypeId) -> u32 {
        if let Some(id) = self.spirv_types.get(&type_id) {
            *id
        } else {
            let ttpe = self.types.get_type(type_id);
            let spirv_id = match ttpe {
                Type::Void => self.code.type_void(),
                Type::Error => unreachable!(),
                Type::Int => self.code.type_int(32, 1),
                Type::Float => self.code.type_float(32),
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
        _attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        _params: &Vec<Field>,
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let type_id = self.types.type_id(fun_id);

        let fun_type = self.type_id_to_spirv_id(type_id);
        let return_type = match self.types.get_type(type_id) {
            Type::Fun(fun) => self.type_id_to_spirv_id(fun.get_return_type()),
            _ => unreachable!("Internal compiler error: Function must have FunType type."),
        };

        let fun_spirv_id =
            self.code
                .begin_function(return_type, None, spirv::FunctionControl::NONE, fun_type)?;

        self.code.begin_block(None)?;
        self.visit_expr(body)?;
        self.code.ret()?;
        self.code.end_function()?;

        Ok(fun_spirv_id)
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {
        // Shouldn't be a panic
        panic!("Internal compiler error: Spirv generation shouldn't be called with errors.")
    }

    fn binary(&mut self, _id: Id, _kind: BinOpKind, _left: &Expr, _right: &Expr) -> Self::Out {
        todo!()
    }

    fn r#let(
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

    fn identifier(&mut self, _name: Name) -> Self::Out {
        todo!()
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
