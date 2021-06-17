use crate::{
    ast::{Ast, Visitor},
    error::CoffinError,
    name_resolution::{VariableId, VariableTable},
    parser::spans_table::SpanTable,
    type_resolution::types::{TypeId, TypeTable},
};
use rspirv::{
    dr::{self, Builder, Module},
    spirv,
};
use std::collections::HashMap;

pub fn visit(
    ast: &Ast,
    vars: &VariableTable,
    types: &TypeTable,
    spans: &SpanTable,
) -> (Module, Vec<CoffinError>) {
    let mut spirv = SpirvGen {
        vars,
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

struct SpirvGen<'a, 'b, 'c> {
    vars: &'a VariableTable,
    spirv_vars: HashMap<VariableId, u32>,
    types: &'b TypeTable,
    spirv_types: HashMap<TypeId, u32>,

    code: Builder,
    // VariableId -> u32
    // TypeId -> u32
    spans: &'c SpanTable,
    errors: Vec<CoffinError>,
}

impl Visitor for SpirvGen<'_, '_, '_> {
    type Out = Result<u32, dr::Error>;

    fn fun(
        &mut self,
        fun_id: crate::ast::Id,
        attrs: &crate::ast::Attrs,
        name: crate::ast::Name,
        paren_id: crate::ast::Id,
        params: &Vec<crate::ast::Field>,
        ret: &Option<(crate::ast::Id, crate::ast::Name)>,
        body: &crate::ast::Expr,
    ) -> Self::Out {
        // let id = self
        //     .code
        //     .begin_function(
        //         return_type,
        //         None,
        //         spirv::FunctionControl::NONE,
        //         function_type,
        //     )
        //     .unwr();
        todo!()
    }

    fn item_error(&mut self, id: crate::ast::Id) -> Self::Out {
        todo!()
    }

    fn binary(
        &mut self,
        id: crate::ast::Id,
        kind: crate::ast::BinOpKind,
        left: &crate::ast::Expr,
        right: &crate::ast::Expr,
    ) -> Self::Out {
        todo!()
    }

    fn r#let(
        &mut self,
        let_id: crate::ast::Id,
        mut_id: Option<crate::ast::Id>,
        name: crate::ast::Name,
        eq_id: crate::ast::Id,
        expr: &crate::ast::Expr,
    ) -> Self::Out {
        todo!()
    }

    fn assign(
        &mut self,
        id: crate::ast::Id,
        name: crate::ast::Name,
        right: &crate::ast::Expr,
    ) -> Self::Out {
        todo!()
    }

    fn identifier(&mut self, name: crate::ast::Name) -> Self::Out {
        todo!()
    }

    fn float(&mut self, id: crate::ast::Id, f: f32) -> Self::Out {
        todo!()
    }

    fn int(&mut self, id: crate::ast::Id, i: i32) -> Self::Out {
        todo!()
    }

    fn block(&mut self, id: crate::ast::Id, exprs: &Vec<crate::ast::Expr>) -> Self::Out {
        todo!()
    }

    fn expr_error(&mut self, id: crate::ast::Id) -> Self::Out {
        todo!()
    }
}
