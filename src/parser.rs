use crate::{
    ast::{BinOpKind, Expr, Id, Visitor},
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
        PrettyPrint::new(parser.spans.clone()).visit_expr(&expr),
    );
}

struct Parser<'a> {
    lexer: Peekable<SpannedIter<'a, Token>>,
    id: Id,
    spans: Vec<Span>,
}

impl<'a> Parser<'a> {
    #![allow(dead_code)]

    fn span(&mut self) -> Span{
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

    fn parse_expr(&mut self, min_binding_power: u8) -> Expr {
        let tree = self.parse_prefix();
        let tree = self.parse_infix(tree, min_binding_power);
        tree
    }

    fn parse_prefix(&mut self) -> Expr {
        match self.peek() {
            Some(Token::Int(i)) => Expr::Int(self.consume(), i),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(Token::RightParen) | Some(Token::Comma) | None => Expr::Error(self.err_consume()),
            Some(_) => Expr::Error(self.err_consume()),
        }
    }

    fn parse_block(&mut self) -> Expr {
        let id = self.consume();
        let mut exprs = Vec::new();

        while let Some(token) = self.peek() {
            // Block ended
            if token == Token::RightBrace {
                break;
            }

            let expr = self.parse_expr(0);

            if let Expr::Error(_) = expr {
                if let Some(Token::RightBrace) = self.peek() {
                    // Expr is an error but it synced to the right brace
                    // so add it to the list and return a Block
                    exprs.push(expr);
                    break;
                } else {
                    // Expr is an error and we can't sync here so return an error
                    return Expr::Error(id);
                }
            } else {
                // Expr is not an error so just add it to the list
                exprs.push(expr);
            }
        }

        // Set the span so that first number represents { and the second represents }
        self.spans[id.0 as usize] = (self.spans[id.0 as usize].start)..(self.eat().end - 1);
        Expr::Block(id, exprs)
    }

    fn parse_infix(&mut self, tree: Expr, min_binding_power: u8) -> Expr {
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
                    Box::new(self.parse_expr(10)),
                ),
                _ => break,
            }
        }

        tree
    }
}
