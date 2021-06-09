use super::Parser;
use crate::{
    ast::{BinOpKind, Expr, Name},
    ast_span,
    error::ParserErrorKind,
    lexer::Token,
};

pub enum ExprResult {
    Correct(Expr),
    PanicMode(Expr),
}
use ExprResult::{Correct, PanicMode};

impl ExprResult {
    pub fn expr(self) -> Expr {
        match self {
            Correct(e) => e,
            PanicMode(e) => e,
        }
    }
    pub fn destruct(self) -> (Expr, bool) {
        match self {
            Correct(e) => (e, false),
            PanicMode(e) => (e, true),
        }
    }
    pub fn map<F: FnOnce(Expr) -> Expr>(self, f: F) -> ExprResult {
        match self {
            Correct(e) => Correct(f(e)),
            PanicMode(e) => PanicMode(f(e)),
        }
    }
}

impl Parser<'_> {
    pub fn parse_expr(&mut self, min_binding_power: Option<u8>) -> ExprResult {
        match self.parse_prefix() {
            Correct(e) => self.parse_infix(e, min_binding_power.unwrap_or(0)),
            PanicMode(e) => PanicMode(e),
        }
    }

    fn parse_prefix(&mut self) -> ExprResult {
        match self.curr_token {
            Token::Int(i) => Correct(Expr::Int(self.consume(), i)),
            Token::Identifier(s) => Correct(Expr::Identifier(Name(self.consume(), s))),
            Token::Let => self.parse_let(),
            Token::LeftBrace => self.parse_block(),
            _ => PanicMode(self.err_consume(
                None,
                ParserErrorKind::ExpectedPrefixToken,
                None,
                &Self::EXPR_SYNC,
            )),
        }
    }

    fn parse_let(&mut self) -> ExprResult {
        let let_id = self.consume_expect(Token::Let);
        
        let mut_id = match self.curr_token {
            Token::Mut => Some(self.consume_expect(Token::Mut)),
            _ => None,
        };

        let name = match self.curr_token {
            Token::Identifier(s) => Name(self.consume(), s),
            _ => {
                return self.err_consume(
                    let_id,
                    ParserErrorKind::expected_identifier(),
                    None,
                    &Self::EXPR_SYNC,
                )
            }
        };

        let eq_id = match self.curr_token {
            Token::Equal => self.consume_expect(Token::Equal),
            _ => {
                return self.err_consume(
                    let_id,
                    ParserErrorKind::ExpectedToken(Token::Equal),
                    None,
                    &Self::EXPR_SYNC,
                )
            }
        };

        self.parse_expr(None).map(|expr| Expr::Let {
            let_id,
            mut_id,
            name,
            eq_id,
            expr: Box::new(expr),
        })
    }

    fn parse_block(&mut self) -> ExprResult {
        let id = self.consume_expect(Token::LeftBrace);
        let mut exprs = Vec::new();

        loop {
            if self.curr_token == Token::RightBrace {
                self.spans[id].end = self.skip().start;
                return Correct(Expr::Block(id, exprs));
            }

            let (expr, is_panic) = self.parse_expr(None).destruct();
            exprs.push(expr);

            if is_panic && self.curr_token != Token::RightBrace {
                self.spans[id].end = self.spans[id].start;
                return PanicMode(Expr::Block(id, exprs));
            }
        }
    }

    fn parse_infix(&mut self, mut tree: Expr, min_binding_power: u8) -> ExprResult {
        // Associativity
        const LEFT: u8 = 0;
        const RIGHT: u8 = 1;

        enum InfixType {
            Standard(BinOpKind),
            Assign,
        }
        use InfixType::{Assign, Standard};

        loop {
            let (binding_power, assoc, kind) = match self.curr_token {
                Token::Equal => (5, RIGHT, Assign),
                Token::Plus => (10, LEFT, Standard(BinOpKind::Add)),
                Token::Minus => (10, LEFT, Standard(BinOpKind::Sub)),
                Token::Star => (20, LEFT, Standard(BinOpKind::Mul)),
                Token::Slash => (20, LEFT, Standard(BinOpKind::Div)),
                Token::Percent => (20, LEFT, Standard(BinOpKind::Rem)),
                Token::DoubleStar => (30, RIGHT, Standard(BinOpKind::Pow)),
                Token::LeftParen | Token::LeftBracket | Token::Dot => {
                    todo!("Function call, indexing, dot operators not implemented.")
                }
                _ => break,
            };

            if binding_power + assoc > min_binding_power {
                let id = self.consume();
                let (expr, mut is_panic) = self.parse_expr(Some(binding_power)).destruct();

                tree = if let Standard(bin_op_kind) = kind {
                    Expr::Binary(id, bin_op_kind, Box::new(tree), Box::new(expr))
                } else if let Expr::Identifier(n) = tree {
                    // It's an assignment
                    Expr::Assign(id, n, Box::new(expr))
                } else {
                    // Assignment if tree isn't an identifier
                    is_panic = true;
                    let err_span = ast_span::get_expr_span(&tree, &self.spans);
                    let expr_span = ast_span::get_expr_span(&expr, &self.spans);
                    self.spans[id].start = err_span.start;
                    self.spans[id].end = expr_span.end;

                    self.err_consume(
                        id,
                        ParserErrorKind::ExpressionNotAssignable,
                        err_span,
                        &Self::EXPR_SYNC,
                    )
                };

                if is_panic {
                    return PanicMode(tree);
                }
            } else {
                break;
            }
        }

        Correct(tree)
    }
}
