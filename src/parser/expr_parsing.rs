use super::{Parser, ParserResult};
use crate::{
    ast::{BinOpKind, Expr},
    error::ParserErrorKind,
    lexer::Token,
};

use ParserResult::{Ok, Panic};

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self, min_binding_power: u8) -> ParserResult {
        match self.parse_prefix() {
            Ok(e) => self.parse_infix(e, min_binding_power),
            Panic(e) => Panic(e),
        }
    }

    pub(super) fn parse_prefix(&mut self) -> ParserResult {
        match self.peek() {
            Token::Int(i) => Ok(Expr::Int(self.consume(), i)),
            Token::LeftBrace => self.parse_block(),
            _ => Panic(self.err_consume(ParserErrorKind::ExpectedPrefixToken, &Self::EXPR_SYNC)),
        }
    }

    pub(super) fn parse_block(&mut self) -> ParserResult {
        let id = self.consume_expect(Token::LeftBrace);

        let mut exprs = Vec::new();

        loop {
            if self.peek() == Token::RightBrace {
                self.spans[id].end = self.skip().end - 1;
                return Ok(Expr::Block(id, exprs));
            }

            let (expr, is_panic) = self.parse_expr(0).destruct();
            exprs.push(expr);

            if is_panic && self.peek() != Token::RightBrace {
                self.spans[id].end = self.spans[id].start;
                return Panic(Expr::Block(id, exprs));
            }
        }
    }

    pub(super) fn parse_infix(&mut self, mut tree: Expr, min_binding_power: u8) -> ParserResult {
        // Associativity
        const LEFT: u8 = 0;
        const _RIGHT: u8 = 1;

        loop {
            tree = match self.peek() {
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
