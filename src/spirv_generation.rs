use crate::ast::Visitor;
use rspirv::dr::Builder;
use rspirv::spirv;

pub fn visit() -> () {
    let mut spirv = SpirvGen {
        code: Builder::new(),
    };

    spirv.code.set_version(1, 3);
    spirv
        .code
        .memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::Simple);
}

struct SpirvGen {
    code: Builder,
}

trait CoffinUnwrap<T> {
    fn unwr(self) -> T;
}

impl<T> CoffinUnwrap<T> for Result<T, rspirv::dr::Error> {
    fn unwr(self) -> T {
        match self {
            Ok(ok) => ok,
            Err(err) => panic!("Internal compiler error while building spirv: {}", err),
        }
    }
}

impl Visitor for SpirvGen {
    type Out = ();

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
