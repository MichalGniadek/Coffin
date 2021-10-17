pub mod name_table;
pub use name_table::{NameTable, VariableId};

use crate::{
    ast::{AccessType, Ast, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, ItemVisitor, Name},
    error::{CoffinError, InternalError},
    parser::spans_table::SpanTable,
    type_id::TypeId,
};
use lasso::{RodeoReader, Spur};
use std::collections::HashMap;

pub fn visit(ast: &mut Ast) -> NameTable {
    let mut nr = NameResolution {
        names: NameTable::default(),

        var_scope: Scopes::new(),
        type_scope: Scopes::new_with_builtin_types(&ast.rodeo),

        spans: &ast.spans,
        errors: &mut ast.errors,
    };

    for item in &ast.items {
        nr.visit_item(item);
    }

    nr.names
}

struct Scopes<T>(Vec<HashMap<Spur, T>>);

impl<T> Scopes<T> {
    fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    fn push(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.0.pop().ice_expect("Called pop without scopes");
    }

    fn insert(&mut self, name: Spur, id: T) {
        self.0.last_mut().ice_expect("No scope").insert(name, id);
    }

    fn find(&self, name: Spur) -> Option<&T> {
        self.0.iter().rev().find_map(|scope| scope.get(&name))
    }
}

impl Scopes<TypeId> {
    fn new_with_builtin_types(rodeo: &RodeoReader) -> Self {
        let mut slf = Self::new();
        use crate::type_id::builtin_types::*;
        for (name, id) in [
            ("void", VOID_ID),
            ("int", INT_ID),
            ("float", FLOAT_ID),
            ("int1", IVEC_ID[1]),
            ("int2", IVEC_ID[2]),
            ("int3", IVEC_ID[3]),
            ("int4", IVEC_ID[4]),
            ("uint1", UVEC_ID[1]),
            ("uint2", UVEC_ID[2]),
            ("uint3", UVEC_ID[3]),
            ("uint4", UVEC_ID[4]),
            ("float1", FVEC_ID[1]),
            ("float2", FVEC_ID[2]),
            ("float3", FVEC_ID[3]),
            ("float4", FVEC_ID[4]),
            ("image2d", IMAGE_ID),
            ("Id", ID_ID),
        ] {
            if let Some(name) = rodeo.get(name) {
                slf.insert(name, id);
            }
        }
        slf
    }
}

struct NameResolution<'ast> {
    names: NameTable,

    var_scope: Scopes<VariableId>,
    type_scope: Scopes<TypeId>,

    spans: &'ast SpanTable,
    errors: &'ast mut Vec<CoffinError>,
}

impl NameResolution<'_> {
    fn new_variable(&mut self, name: Name) {
        let var_id = self.names.new_variable();
        self.names.set_var(name, var_id);
        self.var_scope.insert(name.spur, var_id);
    }

    fn _new_type(&mut self, name: Name) {
        let type_id = self.names._new_type();
        self.names.set_type(name, type_id);
        self.type_scope.insert(name.spur, type_id);
    }
}

impl ItemVisitor for NameResolution<'_> {
    type Out = ();

    fn fun(
        &mut self,
        _fun_id: Id,
        _attrs: &Attrs,
        name: Name,
        _paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        self.new_variable(name);

        self.var_scope.push();
        for param in params {
            self.new_variable(param.name);

            match self.type_scope.find(param.r#type.spur) {
                Some(type_id) => self.names.set_type(param.r#type, *type_id),
                None => self.errors.push(CoffinError::UndeclaredType(
                    self.spans[param.r#type.id].clone(),
                )),
            }
        }

        if let Some((_, name)) = ret {
            match self.type_scope.find(name.spur) {
                Some(type_id) => self.names.set_type(*name, *type_id),
                None => self
                    .errors
                    .push(CoffinError::UndeclaredType(self.spans[name.id].clone())),
            }
        }

        self.visit_expr(body);
        self.var_scope.pop();
    }

    fn uniform(&mut self, _unif_id: Id, _attrs: &Attrs, field: &Field) -> Self::Out {
        self.new_variable(field.name);

        match self.type_scope.find(field.r#type.spur) {
            Some(type_id) => self.names.set_type(field.r#type, *type_id),
            None => self.errors.push(CoffinError::UndeclaredType(
                self.spans[field.r#type.id].clone(),
            )),
        }
    }

    fn item_error(&mut self, _id: Id) -> Self::Out {}
}

impl ExprVisitor for NameResolution<'_> {
    type Out = ();

    fn binary(&mut self, _id: Id, _kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        self.visit_expr(left);
        self.visit_expr(right);
    }

    fn r#let(
        &mut self,
        _let_id: Id,
        _mut_id: Option<Id>,
        name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        self.visit_expr(expr);
        self.new_variable(name);
    }

    fn access(&mut self, _id: Id, expr: &Expr, access: &Vec<AccessType>) -> Self::Out {
        self.visit_expr(expr);
        for a in access {
            if let AccessType::Index(_, expr) = a {
                self.var_scope.push();
                self.visit_expr(expr);
                self.var_scope.pop();
            }
        }
    }

    fn assign(
        &mut self,
        _id: Id,
        left: &Expr,
        access: &Vec<AccessType>,
        right: &Expr,
    ) -> Self::Out {
        self.visit_expr(left);
        for a in access {
            if let AccessType::Index(_, expr) = a {
                self.var_scope.push();
                self.visit_expr(expr);
                self.var_scope.pop();
            }
        }
        self.visit_expr(right);
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        match self.var_scope.find(name.spur) {
            Some(&var_id) => self.names.set_var(name, var_id),
            None => {
                self.errors
                    .push(CoffinError::UndeclaredVariable(self.spans[name.id].clone()));
                self.new_variable(name)
            }
        }
    }

    fn float(&mut self, _id: Id, _f: f32) -> Self::Out {}
    fn int(&mut self, _id: Id, _i: i32) -> Self::Out {}

    fn block(&mut self, _id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.var_scope.push();
        for e in exprs {
            self.visit_expr(e);
        }
        self.var_scope.pop();
    }

    fn convert(&mut self, _id: Id, expr: &Expr, r#type: Name) -> Self::Out {
        self.visit_expr(expr);

        match self.type_scope.find(r#type.spur) {
            Some(type_id) => self.names.set_type(r#type, *type_id),
            None => self
                .errors
                .push(CoffinError::UndeclaredType(self.spans[r#type.id].clone())),
        }
    }

    fn call(&mut self, _id: Id, name: Name, args: &Vec<Expr>) -> Self::Out {
        if let Some(var_id) = self.var_scope.find(name.spur) {
            self.names.set_var(name, *var_id);
        } else if let Some(type_id) = self.type_scope.find(name.spur) {
            self.names.set_type(name, *type_id);
        } else {
            todo!("Error")
        }

        for e in args {
            self.visit_expr(e)
        }
    }

    fn r#if(
        &mut self,
        _id: Id,
        condition: &Expr,
        block: &Expr,
        r#_else: Option<(Id, &Expr)>,
    ) -> Self::Out {
        self.var_scope.push();
        self.visit_expr(condition);
        self.var_scope.pop();
        self.var_scope.push();
        self.visit_expr(block);
        self.var_scope.pop();
        if let Some((_, r#else)) = r#_else {
            self.var_scope.push();
            self.visit_expr(r#else);
            self.var_scope.pop();
        }
    }

    fn expr_error(&mut self, _id: Id) -> Self::Out {}
}
