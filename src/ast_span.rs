use crate::{
    ast::{AccessType, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, Item, ItemVisitor, Name},
    parser::spans_table::SpanTable,
};
use logos::Span;

pub fn get_item_span(item: &Item, spans: &SpanTable) -> Span {
    SpanGetter(spans).visit_item(item)
}

pub fn get_expr_span(expr: &Expr, spans: &SpanTable) -> Span {
    SpanGetter(spans).visit_expr(expr)
}

struct SpanGetter<'a>(&'a SpanTable);

impl ItemVisitor for SpanGetter<'_> {
    type Out = Span;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        _name: Name,
        _paren_id: Id,
        _params: &[Field],
        _ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let start = match attrs {
            Attrs::Ok(id, _) => &self.0[*id],
            Attrs::None => &self.0[fun_id],
            Attrs::Error(id) => &self.0[*id],
        };
        start.start..self.visit_expr(body).end
    }

    fn uniform(&mut self, unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out {
        let start = match attrs {
            Attrs::Ok(id, _) => &self.0[*id],
            Attrs::None => &self.0[unif_id],
            Attrs::Error(id) => &self.0[*id],
        };
        start.start..self.0[field.r#type.id].end
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        self.0[id].clone()
    }
}

impl ExprVisitor for SpanGetter<'_> {
    type Out = Span;

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

    fn access(&mut self, _id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out {
        let mut span = self.visit_expr(expr);
        if let Some(a) = access.last() {
            span.end = match a {
                AccessType::Dot(_, n) => self.0[n.id].end,
                AccessType::Index(id, _) => self.0[*id].end,
            }
        }
        span
    }

    fn assign(&mut self, _id: Id, left: &Expr, _access: &[AccessType], right: &Expr) -> Self::Out {
        self.visit_expr(left).start..self.visit_expr(right).end
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        self.0[name.id].clone()
    }

    fn float(&mut self, id: Id, _f: f32) -> Self::Out {
        self.0[id].clone()
    }

    fn int(&mut self, id: Id, _i: i32) -> Self::Out {
        self.0[id].clone()
    }

    fn bool(&mut self, id: Id, _b: bool) -> Self::Out {
        self.0[id].clone()
    }

    fn block(&mut self, id: Id, _exprs: &[Expr]) -> Self::Out {
        self.0[id].clone()
    }

    fn convert(&mut self, _id: Id, expr: &Expr, r#type: Name) -> Self::Out {
        self.visit_expr(expr).start..self.0[r#type.id].end
    }

    fn call(&mut self, id: Id, name: Name, _args: &[Expr]) -> Self::Out {
        self.0[name.id].start..self.0[id].end
    }

    fn r#if(
        &mut self,
        id: Id,
        _condition: &Expr,
        block: &Expr,
        r#else: Option<(Id, &Expr)>,
    ) -> Self::Out {
        let end = if let Some((_, r#else)) = r#else {
            self.visit_expr(r#else)
        } else {
            self.visit_expr(block)
        };

        self.0[id].start..end.end
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        self.0[id].clone()
    }
}
