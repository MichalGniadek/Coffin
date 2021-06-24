use crate::{
    ast::{self, Ast, Attr, Attrs, ExprVisitor, Field, ItemVisitor},
    name_resolution::VariableTable,
    parser::spans_table::SpanTable,
    type_resolution::types::TypeTable,
};
use ast::{BinOpKind, Expr, Id, Name};
use lasso::{RodeoReader, Spur};
use std::iter;

pub fn visit(
    ast: &Ast,
    rodeo: bool,
    spans: bool,
    variables: Option<&VariableTable>,
    types: Option<&TypeTable>,
) -> String {
    let mut slf = DebugPrint {
        rodeo: if rodeo { Some(&ast.rodeo) } else { None },
        variables,
        spans: if spans { Some(&ast.spans) } else { None },
        types,
        indent: String::from('\n'),
    };

    ast.items
        .iter()
        .map(|i| slf.visit_item(i))
        .chain(iter::once(String::from("\n===")))
        .chain(ast.errors.iter().map(|err| format!("{}", err)))
        .intersperse(String::from('\n'))
        .collect()
}

pub struct DebugPrint<'ast, 'vars, 'types> {
    rodeo: Option<&'ast RodeoReader>,
    spans: Option<&'ast SpanTable>,
    variables: Option<&'vars VariableTable>,
    types: Option<&'types TypeTable>,
    indent: String,
}

impl DebugPrint<'_, '_, '_> {
    fn span(&self, id: Id) -> String {
        self.spans
            .map_or(String::new(), |s| format!("[{:?}] ", s[id]))
    }

    fn ident(&self, spur: Spur) -> String {
        match self.rodeo {
            Some(r) => r.resolve(&spur).to_owned(),
            None => format!("{:?}", spur),
        }
    }

    fn name(&self, name: Name) -> String {
        let var_string = match self.variables {
            Some(map) => match map.get(name.id) {
                Some(var_id) => format!("@{}", usize::from(var_id)),
                None => String::from("@X"),
            },
            None => String::new(),
        };
        format!(
            "{}{}{}",
            self.span(name.id),
            self.ident(name.spur),
            var_string
        )
    }

    fn ttpe(&self, id: Id) -> String {
        let ttpe = match self.types {
            Some(t) => &t[t.type_id(id)],
            None => return String::new(),
        };

        format!(": {}", ttpe)
    }

    fn field(&self, field: &Field) -> String {
        format!(
            "{} {}: {}",
            self.name(field.name),
            self.span(field.colon_id),
            self.name(field.ttpe)
        )
    }

    fn attrs(&self, attrs: &Attrs) -> String {
        match attrs {
            Attrs::Ok(id, attrs) => {
                format!(
                    "{}#[{}]",
                    self.span(*id),
                    attrs
                        .iter()
                        .map(|Attr(name, tokens)| format!(
                            "{}({})",
                            self.name(*name),
                            tokens
                                .iter()
                                .map(|(id, t)| format!("{}{}", self.span(*id), t))
                                .intersperse(String::from(' '))
                                .collect::<String>()
                        ))
                        .intersperse(String::from(", "))
                        .collect::<String>()
                )
            }
            Attrs::None => String::new(),
            Attrs::Error(id) => format!("#[{}Err]", self.span(*id)),
        }
    }
}

impl ItemVisitor for DebugPrint<'_, '_, '_> {
    type Out = String;

    fn fun(
        &mut self,
        fun_id: Id,
        attrs: &Attrs,
        name: Name,
        paren_id: Id,
        params: &Vec<Field>,
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let params_text = params
            .iter()
            .map(|f| self.field(f))
            .intersperse(String::from(", "))
            .collect::<String>();

        let ret_text = match ret {
            Some((id, name)) => format!("{}-> {}", self.span(*id), self.name(*name)),
            None => String::new(),
        };

        format!(
            "{}{}fun {} {}({}) {} {}",
            self.attrs(attrs),
            self.span(fun_id),
            self.name(name),
            self.span(paren_id),
            params_text,
            ret_text,
            self.visit_expr(body)
        )
    }

    fn uniform(&mut self, unif_id: Id, attrs: &Attrs, field: &Field) -> Self::Out {
        format!(
            "{}{}unif {}",
            self.attrs(attrs),
            self.span(unif_id),
            self.field(field)
        )
    }

    fn item_error(&mut self, id: Id) -> Self::Out {
        format!("{}Err", self.span(id))
    }
}

impl ExprVisitor for DebugPrint<'_, '_, '_> {
    type Out = String;

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        format!(
            "({} {}{} {}){}",
            self.visit_expr(left),
            self.span(id),
            kind,
            self.visit_expr(right),
            self.ttpe(id),
        )
    }

    fn let_declaration(
        &mut self,
        let_id: Id,
        mut_id: Option<Id>,
        name: Name,
        eq_id: Id,
        expr: &Expr,
    ) -> Self::Out {
        let r#mut = match mut_id {
            Some(id) => format!(" {}mut", self.span(id)),
            None => String::new(),
        };

        format!(
            "({}let{} {} {}= {}){}",
            self.span(let_id),
            r#mut,
            self.name(name),
            self.span(eq_id),
            self.visit_expr(expr),
            self.ttpe(let_id),
        )
    }

    fn assign(&mut self, id: Id, name: Name, right: &Expr) -> Self::Out {
        format!(
            "({} {}= {}){}",
            self.name(name),
            self.span(id),
            self.visit_expr(right),
            self.ttpe(id),
        )
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        format!("${}{}", self.name(name), self.ttpe(name.id))
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("{}#{}{}", self.span(id), f, self.ttpe(id))
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("{}#{}{}", self.span(id), i, self.ttpe(id))
    }

    fn block(&mut self, id: Id, exprs: &Vec<Expr>) -> Self::Out {
        self.indent.push('\t');
        let indent = self.indent.clone();

        let body = exprs
            .iter()
            .map(|e| self.visit_expr(e))
            .intersperse(indent)
            .collect::<String>();

        self.indent.pop();

        format!(
            "{}{{{}\t{}{}}}{}",
            self.span(id),
            self.indent,
            body,
            self.indent,
            self.ttpe(id),
        )
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        format!("{}Err", self.span(id))
    }
}
