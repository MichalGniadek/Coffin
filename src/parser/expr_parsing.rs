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
        const RIGHT: u8 = 1;

        loop {
            let (binding_power, assoc, kind) = match self.curr_token {
                Token::DoubleEqual => (5, RIGHT, BinOpKind::Eq),
                Token::Plus => (10, LEFT, BinOpKind::Add),
                Token::Minus => (10, LEFT, BinOpKind::Sub),
                Token::Star => (20, LEFT, BinOpKind::Mul),
                Token::Slash => (20, LEFT, BinOpKind::Div),
                Token::Percent => (20, LEFT, BinOpKind::Rem),
                Token::DoubleStar => (30, RIGHT, BinOpKind::Pow),
                Token::LeftParen | Token::LeftBracket | Token::Dot => {
                    todo!("Function call, indexing, dot operators not implemented.")
                }
                _ => break,
            };

            if binding_power + assoc > min_binding_power {
                let id = self.consume();
                let (expr, is_panic) = self.parse_expr(binding_power).destruct();
                tree = Expr::Binary(id, kind, Box::new(tree), Box::new(expr));
                if is_panic {
                    return Panic(tree);
                }
            } else {
                break;
            }
        }

        Good(tree)
    }
}
