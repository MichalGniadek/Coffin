use crate::{
    ast::{Attrs, BinOpKind, Expr, Field, Id, Item, Name, Visitor},
    error::ParserError,
    parser::spans_table::SpansTable,
};
use logos::Span;

pub fn get_item_span(item: &Item, spans: &SpansTable) -> Span {
    SpanGetter(spans).visit_item(item)
}

pub fn get_expr_span(expr: &Expr, spans: &SpansTable) -> Span {
    SpanGetter(spans).visit_expr(expr)
}

struct SpanGetter<'a>(&'a SpansTable);

impl Visitor for SpanGetter<'_> {
    type Out = Span;

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
        let start = match attrs {
            Attrs::Ok(id, _) => &self.0[*id],
            Attrs::None => &self.0[fun_id],
            Attrs::Error(id, _) => &self.0[*id],
        };
        start.start..self.visit_expr(body).end
    }

    fn item_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.0[id].clone()
    }

    fn binary(&mut self, _id: Id, _kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        self.visit_expr(left).start..self.visit_expr(right).end
    }

    fn r#let(
        &mut self,
        let_id: Id,
        _mut_id: Option<Id>,
        _name: Name,
        _eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        self.0[let_id].start..self.visit_expr(expr).end
    }

    fn assign(&mut self, _id: Id, name: Name, right: &Expr) -> Self::Out {
        self.0[name.0].start..self.visit_expr(right).end
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        self.0[name.0].clone()
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.0[id].clone()
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.0[id].clone()
    }

    fn block(&mut self, id: Id, _exprs: &Vec<Expr>) -> Self::Out {
        self.0[id].clone()
    }

    fn expr_error(&mut self, id: Id, _kind: &ParserError) -> Self::Out {
        self.0[id].clone()
    }
}
