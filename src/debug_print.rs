use crate::{
    ast::{self, Ast, Attr, Attrs, Field, Visitor},
    error::ParserError,
    name_resolution::VariablesTable,
    parser::spans_table::SpansTable,
    type_resolution::{Type, TypesTable},
};
use ast::{BinOpKind, Expr, Id, Name};
use lasso::{RodeoResolver, Spur};

pub struct DebugPrint<'a, 'b, 'c, 'd> {
    rodeo: Option<&'a RodeoResolver>,
    spans: Option<&'b SpansTable>,
    variables: Option<&'c VariablesTable>,
    types: Option<&'d TypesTable>,
    indent: String,
}

impl<'a, 'b, 'c, 'd> DebugPrint<'a, 'b, 'c, 'd> {
    pub fn visit(
        ast: &Ast,
        rodeo: Option<&'a RodeoResolver>,
        spans: Option<&'b SpansTable>,
        variables: Option<&'c VariablesTable>,
        types: Option<&'d TypesTable>,
    ) -> String {
        let mut slf = Self {
            rodeo,
            variables,
            spans,
            types,
            indent: String::from('\n'),
        };

        ast.into_iter()
            .map(|i| slf.visit_item(i))
            .intersperse(String::from('\n'))
            .collect()
    }
}

impl DebugPrint<'_, '_, '_, '_> {
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

    fn err(&self, err: &ParserError) -> String {
        match self.spans {
            Some(_) => format!("[{:?}] {}", err.1, err.to_string()),
            None => err.to_string(),
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
            Some(t) => &t[id],
            None => return String::new(),
        };

        match ttpe {
            Type::Void => format!(": void"),
            Type::Error => format!(": error"),
            Type::Int => format!(": int"),
            Type::Float => format!(": float"),
            Type::Fun(_) => format!(": fun"),
        }
    }

    fn field(&self, field: &Field) -> String {
        format!(
            "{} {}: {}",
            self.name(field.name),
            self.span(field.colon_id),
            self.name(field.ttpe)
        )
    }
}

impl Visitor for DebugPrint<'_, '_, '_, '_> {
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
        let attr_text = match attrs {
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
            Attrs::Error(id, err) => format!("#[{}Err: {}]", self.span(*id), self.err(err)),
        };

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
            attr_text,
            self.span(fun_id),
            self.name(name),
            self.span(paren_id),
            params_text,
            ret_text,
            self.visit_expr(body)
        )
    }

    fn item_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!("{}Err: {}", self.span(id), self.err(err))
    }

    fn binary(&mut self, id: Id, kind: BinOpKind, left: &Expr, right: &Expr) -> Self::Out {
        let symbol = match kind {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Rem => "%",
            BinOpKind::Pow => "**",
            BinOpKind::Eq => "==",
        };
        format!(
            "({} {}{} {}){}",
            self.visit_expr(left),
            self.span(id),
            symbol,
            self.visit_expr(right),
            self.ttpe(id),
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

    fn expr_error(&mut self, id: Id, err: &ParserError) -> Self::Out {
        format!("{}Err: {}", self.span(id), self.err(err))
    }
}
