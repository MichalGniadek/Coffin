use crate::{
    ast::{BinOpKind, Expr, Id, Visitor},
    error::ParserErrorKind,
    lexer::Token,
    pretty_print::PrettyPrint,
};
use logos::{Lexer, Span, SpannedIter};
use std::iter::Peekable;

pub fn parse<'a>(lexer: Lexer<'a, Token>) {
    let mut parser = Parser {
        lexer: lexer.spanned().peekable(),
        id: Id(0),
        spans: vec![],
    };

    let expr = parser.parse_expr(0);

    println!(
        "{}",
        PrettyPrint::new(parser.spans.clone()).visit_expr(&expr.expr()),
    );
}

struct Parser<'a> {
    lexer: Peekable<SpannedIter<'a, Token>>,
    id: Id,
    spans: Vec<Span>,
}

type ParserResult = Result<Expr, Expr>;
trait ParserResultTrait {
    fn expr(self) -> Expr;
}
impl ParserResultTrait for ParserResult {
    fn expr(self) -> Expr {
        match self {
            Ok(e) => e,
            Err(e) => e,
        }
    }
}

impl<'a> Parser<'a> {
    fn span(&mut self) -> Span {
        self.lexer.peek().map_or(0..0, |(_, span)| span.clone())
    }

    fn consume(&mut self) -> Id {
        let span = self.span();
        self.spans.push(span);
        self.lexer.next();

        let id = self.id;
        self.id = Id(self.id.0 + 1);
        id
    }

    fn err_consume(&mut self) -> Id {
        const SYNC_TOKENS: [Token; 1] = [Token::RightBrace];

        let mut fin_span = self.span();

        while let Some((token, span)) = self.lexer.peek() {
            if !SYNC_TOKENS.contains(token) {
                fin_span = fin_span.start..span.end;
                self.lexer.next();
            } else {
                break;
            }
        }

        self.spans.push(fin_span.clone());

        let id = self.id;
        self.id = Id(self.id.0 + 1);
        id
    }

    fn eat(&mut self) -> Span {
        let span = self.span();
        self.lexer.next();
        span
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(t, _)| *t)
    }

    fn parse_expr(&mut self, min_binding_power: u8) -> ParserResult {
        let tree = self.parse_prefix().expr();
        let tree = self.parse_infix(tree, min_binding_power);
        tree
    }

    fn parse_prefix(&mut self) -> ParserResult {
        match self.peek() {
            Some(Token::Int(i)) => Ok(Expr::Int(self.consume(), i)),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(_) | None => Err(Expr::Error(
                self.err_consume(),
                ParserErrorKind::TokenIsntAPrefixToken,
            )),
        }
    }

    fn parse_block(&mut self) -> ParserResult {
        let id = self.consume();

        // Set the span so that first number represents { and the second represents }
        fn eat_right_brace(slf: &mut Parser, id: Id) {
            slf.spans[id.0 as usize] = (slf.spans[id.0 as usize].start)..(slf.eat().end - 1);
        }

        let mut exprs = Vec::new();

        while let Some(token) = self.peek() {
            // Block ended
            if token == Token::RightBrace {
                eat_right_brace(self, id);
                break;
            }

            let expr = self.parse_expr(0);

            match expr {
                Ok(e) => exprs.push(e),
                Err(e) => {
                    exprs.push(e);
                    if let Some(Token::RightBrace) = self.peek() {
                        eat_right_brace(self, id);
                    }
                    return Err(Expr::Block(id, exprs));
                }
            }
        }

        Ok(Expr::Block(id, exprs))
    }

    fn parse_infix(&mut self, tree: Expr, min_binding_power: u8) -> ParserResult {
        // Associativity
        const LEFT: u8 = 0;
        const RIGHT: u8 = 1;

        let mut tree = tree;

        while let Some(token) = self.peek() {
            tree = match token {
                // Token::T if binding_power + associativity > min_binding_power
                Token::Plus if 10 + LEFT > min_binding_power => Expr::Binary(
                    self.consume(),
                    BinOpKind::Add,
                    Box::new(tree),
                    Box::new(self.parse_expr(10).expr()),
                ),
                _ => break,
            }
        }

        Ok(tree)
    }
}
