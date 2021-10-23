use crate::{
    ast::{
        AccessType, Ast, Attr, Attrs, BinOpKind, Expr, ExprVisitor, Field, Id, ItemVisitor, Name,
    },
    name_resolution::NameTable,
    parser::spans_table::SpanTable,
    type_resolution::types::TypeTable,
};
use lasso::{RodeoReader, Spur};
use std::iter;

pub fn visit(
    ast: &Ast,
    spans: bool,
    variables: Option<&NameTable>,
    types: Option<&TypeTable>,
) -> String {
    let mut slf = DebugPrint {
        rodeo: &ast.rodeo,
        names: variables,
        spans: if spans { Some(&ast.spans) } else { None },
        types,
        indent: String::from('\n'),
    };

    ast.items
        .iter()
        .map(|i| slf.visit_item(i))
        .chain(iter::once(String::from("\n===")))
        .chain(ast.errors.iter().map(|err| format!("{}", err)))
        .intersperse('\n'.into())
        .collect()
}

pub struct DebugPrint<'ast, 'vars, 'types> {
    rodeo: &'ast RodeoReader,
    spans: Option<&'ast SpanTable>,
    names: Option<&'vars NameTable>,
    types: Option<&'types TypeTable>,
    indent: String,
}

impl DebugPrint<'_, '_, '_> {
    fn span(&self, id: Id) -> String {
        self.spans
            .map_or(String::new(), |s| format!("[{:?}] ", s[id]))
    }

    fn ident(&self, spur: Spur) -> String {
        self.rodeo.resolve(&spur).to_owned()
    }

    fn name(&self, name: Name) -> String {
        let var_string = match self.names {
            Some(map) => match (map.var_id(name), map.type_id(name)) {
                (Some(id), _) => format!("@{}", usize::from(id)),
                (_, Some(id)) => format!("@T{}", usize::from(id)),
                _ => "@X".into(),
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

    fn r#type(&self, id: Id) -> String {
        let r#type = match self.types {
            Some(t) => &t[t.type_id(id)],
            None => return String::new(),
        };

        format!(": {}", r#type)
    }

    fn field(&self, field: &Field) -> String {
        format!(
            "{} {}: {}",
            self.name(field.name),
            self.span(field.colon_id),
            self.name(field.r#type),
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
                                .intersperse(' '.into())
                                .collect::<String>()
                        ))
                        .intersperse(", ".into())
                        .collect::<String>()
                )
            }
            Attrs::None => String::new(),
            Attrs::Error(id) => format!("#[{}Err]", self.span(*id)),
        }
    }

    fn print_access(&mut self, access: &[AccessType]) -> String {
        access
            .iter()
            .map(|a| match a {
                AccessType::Dot(id, name) => format!(
                    ".{}{}{}",
                    self.span(*id),
                    self.span(name.id),
                    self.ident(name.spur)
                ),
                AccessType::Index(brackets, expr) => {
                    format!("{}[{}]", self.span(*brackets), self.visit_expr(expr))
                }
            })
            .collect()
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
        params: &[Field],
        ret: &Option<(Id, Name)>,
        body: &Expr,
    ) -> Self::Out {
        let params_text = params
            .iter()
            .map(|f| self.field(f))
            .intersperse(", ".into())
            .collect::<String>();

        let ret_text = match ret {
            Some((id, name)) => format!("{}-> {}", self.span(*id), self.name(*name),),
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
            self.r#type(id),
        )
    }

    fn r#let(
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
            self.r#type(let_id),
        )
    }

    fn access(&mut self, _id: Id, expr: &Expr, access: &[AccessType]) -> Self::Out {
        format!("({}){}", self.visit_expr(expr), self.print_access(access))
    }

    fn assign(&mut self, id: Id, left: &Expr, access: &[AccessType], right: &Expr) -> Self::Out {
        format!(
            "(({}){} {}= {}){}",
            self.visit_expr(left),
            self.print_access(access),
            self.span(id),
            self.visit_expr(right),
            self.r#type(id),
        )
    }

    fn identifier(&mut self, name: Name) -> Self::Out {
        format!("${}{}", self.name(name), self.r#type(name.id))
    }

    fn float(&mut self, id: Id, f: f32) -> Self::Out {
        format!("{}#{}{}", self.span(id), f, self.r#type(id))
    }

    fn int(&mut self, id: Id, i: i32) -> Self::Out {
        format!("{}#{}{}", self.span(id), i, self.r#type(id))
    }

    fn bool(&mut self, id: Id, b: bool) -> Self::Out {
        format!("{}#{}{}", self.span(id), b, self.r#type(id))
    }

    fn block(&mut self, id: Id, exprs: &[Expr]) -> Self::Out {
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
            self.r#type(id),
        )
    }

    fn convert(&mut self, id: Id, expr: &Expr, r#type: Name) -> Self::Out {
        format!(
            "({} {}as {}){}",
            self.visit_expr(expr),
            self.span(id),
            self.name(r#type),
            self.r#type(id),
        )
    }

    fn call(&mut self, id: Id, name: Name, args: &[Expr]) -> Self::Out {
        format!(
            "({}{}({})){}",
            self.name(name),
            self.span(id),
            args.iter()
                .map(|e| self.visit_expr(e))
                .intersperse(", ".into())
                .collect::<String>(),
            self.r#type(id),
        )
    }

    fn r#if(
        &mut self,
        id: Id,
        condition: &Expr,
        block: &Expr,
        r#else: Option<(Id, &Expr)>,
    ) -> Self::Out {
        format!(
            "({}if {} {}{}){}",
            self.span(id),
            self.visit_expr(condition),
            self.visit_expr(block),
            if let Some((id, r#else)) = r#else {
                format!(" {}else {}", self.span(id), self.visit_expr(r#else))
            } else {
                String::new()
            },
            self.r#type(id),
        )
    }

    fn expr_error(&mut self, id: Id) -> Self::Out {
        format!("{}Err", self.span(id))
    }
}
