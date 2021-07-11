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
        let expr = match self.parse_prefix() {
            Correct(e) => e,
            PanicMode(e) => return PanicMode(e),
        };

        let min_binding_power = min_binding_power.unwrap_or(0);
        let expr = match self.parse_infix(expr, min_binding_power) {
            Correct(e) => e,
            PanicMode(e) => return PanicMode(e),
        };

        Correct(expr)
    }

    fn parse_prefix(&mut self) -> ExprResult {
        match self.curr_token {
            Token::Int(i) => Correct(Expr::Int(self.consume(), i)),
            Token::Float(f) => Correct(Expr::Float(self.consume(), f)),
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
            Conversion,
            Call,
        }

        loop {
            let (binding_power, assoc, kind) = match self.curr_token {
                Token::Equal => (5, RIGHT, InfixType::Assign),
                Token::Plus => (10, LEFT, InfixType::Binary(BinOpKind::Add)),
                Token::Minus => (10, LEFT, InfixType::Binary(BinOpKind::Sub)),
                Token::Star => (20, LEFT, InfixType::Binary(BinOpKind::Mul)),
                Token::Slash => (20, LEFT, InfixType::Binary(BinOpKind::Div)),
                Token::Percent => (20, LEFT, InfixType::Binary(BinOpKind::Rem)),
                Token::DoubleStar => (30, RIGHT, InfixType::Binary(BinOpKind::Pow)),
                Token::Dot => (200, LEFT, InfixType::DotAccess),
                Token::LeftBracket => (200, LEFT, InfixType::IndexAccess),
                Token::As => (1, LEFT, InfixType::Conversion),
                Token::LeftParen => (1, LEFT, InfixType::Call),
                _ => break,
            };

            if binding_power + assoc > min_binding_power {
                match kind {
                    InfixType::Binary(kind) => {
                        let id = self.consume();
                        let (right, is_panic) = self.parse_expr(Some(binding_power)).destruct();
                        left = Expr::Binary(id, kind, Box::new(left), Box::new(right));
                        if is_panic {
                            return PanicMode(left);
                        }
                    }
                    InfixType::Assign => {
                        let id = self.consume();
                        let (right, is_panic) = self.parse_expr(None).destruct();

                        left = match left {
                            Expr::Identifier(_) => {
                                Expr::Assign(id, Box::new(left), vec![], Box::new(right))
                            }
                            Expr::Access(_, expr, access) => {
                                Expr::Assign(id, expr, access, Box::new(right))
                            }
                            left => {
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
                        };
                        if is_panic {
                            return PanicMode(left);
                        }
                    }
                    InfixType::DotAccess => {
                        let id = self.consume();
                        let member = match self.curr_token {
                            Token::Identifier(spur) => Name {
                                id: self.consume(),
                                spur,
                            },
                            _ => {
                                return self.err_consume(
                                    id,
                                    ParserErrorKind::expected_identifier(),
                                    None,
                                    &Self::EXPR_SYNC,
                                )
                            }
                        };

                        left = match left {
                            Expr::Access(_, expr, mut access) => {
                                access.push(AccessType::Dot(id, member));
                                Expr::Access(id, expr, access)
                            }
                            left => {
                                Expr::Access(id, Box::new(left), vec![AccessType::Dot(id, member)])
                            }
                        }
                    }
                    InfixType::IndexAccess => {
                        let id = self.consume();
                        let (right, is_panic) = self.parse_expr(None).destruct();
                        if !is_panic {
                            self.spans[id].end = self.skip().end;
                        }

                        left = match left {
                            Expr::Access(_, expr, mut access) => {
                                access.push(AccessType::Index(id, Box::new(right)));
                                Expr::Access(id, expr, access)
                            }
                            left => Expr::Access(
                                id,
                                Box::new(left),
                                vec![AccessType::Index(id, Box::new(right))],
                            ),
                        };

                        if is_panic {
                            return PanicMode(left);
                        }
                    }
                    InfixType::Conversion => {
                        let id = self.consume();
                        let name = match self.curr_token {
                            Token::Identifier(spur) => Name {
                                id: self.consume(),
                                spur,
                            },
                            _ => {
                                return self.err_consume(
                                    id,
                                    ParserErrorKind::expected_identifier(),
                                    None,
                                    &Self::EXPR_SYNC,
                                )
                            }
                        };
                        left = Expr::Convert(id, Box::new(left), name);
                    }
                    InfixType::Call => {
                        let id = self.consume();

                        let name = if let Expr::Identifier(name) = left {
                            name
                        } else {
                            let err_span = ast_span::get_expr_span(&left, &self.spans);
                            self.spans[id].start = err_span.start;
                            return self.err_consume(
                                id,
                                ParserErrorKind::expected_identifier(),
                                err_span,
                                &Self::EXPR_SYNC,
                            );
                        };

                        let mut args = vec![];

                        while self.curr_token != Token::RightParen {
                            let (expr, is_panic) = self.parse_expr(None).destruct();

                            args.push(expr);

                            if is_panic {
                                return PanicMode(Expr::Call(id, name, args));
                            }

                            if self.curr_token == Token::Comma {
                                self.skip();
                            } else if self.curr_token != Token::RightParen {
                                return self.err_consume(
                                    id,
                                    ParserErrorKind::ExpectedToken(Token::RightParen),
                                    None,
                                    &Self::EXPR_SYNC,
                                );
                            }
                        }

                        self.spans[id].end = self.skip().end;

                        left = Expr::Call(id, name, args);
                    }
                }
            } else {
                break;
            }
        }

        Correct(left)
    }
}
