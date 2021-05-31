use super::{ParsedExpr, Parser};
use crate::{
    ast::{BinOpKind, Expr},
    error::ParserErrorKind,
    lexer::Token,
};

use ParsedExpr::{Good, Panic};

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self, min_binding_power: u8) -> ParsedExpr {
        match self.parse_prefix() {
            Good(e) => self.parse_infix(e, min_binding_power),
            Panic(e) => Panic(e),
        }
    }

    pub(super) fn parse_prefix(&mut self) -> ParsedExpr {
        match self.curr_token {
            Token::Int(i) => Good(Expr::Int(self.consume(), i)),
            Token::LeftBrace => self.parse_block(),
            _ => Panic(self.err_consume(ParserErrorKind::ExpectedPrefixToken, &Self::EXPR_SYNC)),
        }
    }

    pub(super) fn parse_block(&mut self) -> ParsedExpr {
        let id = self.consume_expect(Token::LeftBrace);

        let mut exprs = Vec::new();

        loop {
            if self.curr_token == Token::RightBrace {
                self.spans[id].end = self.skip().end - 1;
                return Good(Expr::Block(id, exprs));
            }

            let (expr, is_panic) = self.parse_expr(0).destruct();
            exprs.push(expr);

            if is_panic && self.curr_token != Token::RightBrace {
                self.spans[id].end = self.spans[id].start;
                return Panic(Expr::Block(id, exprs));
            }
        }
    }

    pub(super) fn parse_infix(&mut self, mut tree: Expr, min_binding_power: u8) -> ParsedExpr {
        // Associativity
        const LEFT: u8 = 0;
        const _RIGHT: u8 = 1;

        loop {
            tree = match self.curr_token {
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

        Good(tree)
    }
}
