use super::Parser;
use crate::{
    ast::{AccessType, BinOpKind, Expr, Name},
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
            Token::Identifier(spur) => Correct(Expr::Identifier(Name {
                id: self.consume(),
                spur,
            })),
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
            Token::Identifier(spur) => Name {
                id: self.consume(),
                spur,
            },
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
                self.spans[id].end = self.skip().end;
                return Correct(Expr::Block(id, exprs));
            }

            let (expr, is_panic) = self.parse_expr(None).destruct();
            exprs.push(expr);

            if is_panic && self.curr_token != Token::RightBrace {
                return PanicMode(Expr::Block(id, exprs));
            }
        }
    }

    fn parse_infix(&mut self, mut left: Expr, min_binding_power: u8) -> ExprResult {
        // Associativity
        const LEFT: u8 = 0;
        const RIGHT: u8 = 1;

        enum InfixType {
            Binary(BinOpKind),
            Assign,
            DotAccess,
            IndexAccess,
        }
        use InfixType::{Assign, Binary, DotAccess, IndexAccess};

        loop {
            let (binding_power, assoc, kind) = match self.curr_token {
                Token::Equal => (5, RIGHT, Assign),
                Token::Plus => (10, LEFT, Binary(BinOpKind::Add)),
                Token::Minus => (10, LEFT, Binary(BinOpKind::Sub)),
                Token::Star => (20, LEFT, Binary(BinOpKind::Mul)),
                Token::Slash => (20, LEFT, Binary(BinOpKind::Div)),
                Token::Percent => (20, LEFT, Binary(BinOpKind::Rem)),
                Token::DoubleStar => (30, RIGHT, Binary(BinOpKind::Pow)),
                Token::Dot => (200, LEFT, DotAccess),
                Token::LeftBracket => (200, LEFT, IndexAccess),
                Token::LeftParen => {
                    todo!("Function call operator not implemented.")
                }
                _ => break,
            };

            if binding_power + assoc > min_binding_power {
                let (id, (right, is_panic)) = if let IndexAccess = kind {
                    let id = self.consume();
                    let right = self.parse_expr(None).destruct();
                    self.spans[id].end = self.skip().end;
                    (id, right)
                } else {
                    (
                        self.consume(),
                        self.parse_expr(Some(binding_power)).destruct(),
                    )
                };

                left = match (left, kind, right) {
                    // Binary
                    (left, Binary(kind), right) => {
                        Expr::Binary(id, kind, Box::new(left), Box::new(right))
                    }

                    // Assignment
                    (Expr::Identifier(n), Assign, right) => {
                        Expr::Assign(id, Box::new(Expr::Identifier(n)), vec![], Box::new(right))
                    }
                    (Expr::Access(_, expr, access), Assign, right) => {
                        Expr::Assign(id, expr, access, Box::new(right))
                    }
                    (left, Assign, right) => {
                        let left_span = ast_span::get_expr_span(&left, &self.spans);
                        let right_span = ast_span::get_expr_span(&right, &self.spans);
                        self.spans[id].start = left_span.start;
                        self.spans[id].end = right_span.end;

                        return self.err_consume(
                            id,
                            ParserErrorKind::ExpressionNotAssignable,
                            left_span,
                            &Self::EXPR_SYNC,
                        );
                    }

                    // Dot access
                    (Expr::Access(a_id, expr, mut access), DotAccess, Expr::Identifier(member)) => {
                        access.push(AccessType::Dot(id, member));
                        Expr::Access(a_id, expr, access)
                    }
                    (left, DotAccess, Expr::Identifier(member)) => {
                        Expr::Access(id, Box::new(left), vec![AccessType::Dot(id, member)])
                    }
                    (left, DotAccess, right) => {
                        let left_span = ast_span::get_expr_span(&left, &self.spans);
                        let right_span = ast_span::get_expr_span(&right, &self.spans);

                        self.spans[id].start = left_span.start;
                        self.spans[id].end = right_span.end;

                        return self.err_consume(
                            id,
                            ParserErrorKind::ExpectedIdentifierAfterDot,
                            right_span,
                            &Self::EXPR_SYNC,
                        );
                    }

                    // Index access
                    (Expr::Access(a_id, expr, mut access), IndexAccess, right) => {
                        access.push(AccessType::Index(id, Box::new(right)));
                        Expr::Access(a_id, expr, access)
                    }
                    (left, IndexAccess, right) => Expr::Access(
                        id,
                        Box::new(left),
                        vec![AccessType::Index(id, Box::new(right))],
                    ),
                };

                if is_panic {
                    return PanicMode(left);
                }
            } else {
                break;
            }
        }

        Correct(left)
    }
}
