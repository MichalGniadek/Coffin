use super::Parser;
use crate::{
    ast::{Attr, Attrs, Item, Name},
    error::ParserErrorKind,
    lexer::Token,
};

impl Parser<'_> {
    pub fn parse_item(&mut self) -> Item {
        let attrs = match self.curr_token {
            Token::HashBracket => self.parse_attributes(),
            _ => Attrs::None,
        };

        match self.curr_token {
            Token::Fun => self.parse_fun(attrs),
            _ => todo!(),
        }
    }

    fn parse_attributes(&mut self) -> Attrs {
        let brackets_id = self.consume();
        let mut attrs = vec![];

        while self.curr_token != Token::RightBracket {
            let name = match self.curr_token {
                Token::Identifier(spur) => Name {
                    id: self.consume(),
                    spur,
                },
                _ => {
                    return self.err_consume(
                        brackets_id,
                        ParserErrorKind::expected_identifier(),
                        None,
                        &Self::ITEM_SYNC,
                    )
                }
            };

            attrs.push(Attr(name, self.parse_delimited_tokens()));

            if self.curr_token == Token::Comma {
                self.skip();
            } else if self.curr_token != Token::RightBracket {
                return self.err_consume(
                    brackets_id,
                    ParserErrorKind::ExpectedToken(Token::RightBracket),
                    None,
                    &Self::ITEM_SYNC,
                );
            }
        }

        self.spans[brackets_id].end = self.skip().end - 1;
        Attrs::Ok(brackets_id, attrs)
    }

    fn parse_fun(&mut self, attrs: Attrs) -> Item {
        let fun_id = self.consume_expect(Token::Fun);

        let name = match self.curr_token {
            Token::Identifier(spur) => Name {
                id: self.consume(),
                spur,
            },
            _ => {
                return self.err_consume(
                    fun_id,
                    ParserErrorKind::expected_identifier(),
                    None,
                    &Self::ITEM_SYNC,
                )
            }
        };

        let paren_id = match self.curr_token {
            Token::LeftParen => self.consume(),
            _ => {
                return self.err_consume(
                    fun_id,
                    ParserErrorKind::ExpectedToken(Token::LeftParen),
                    None,
                    &Self::ITEM_SYNC,
                )
            }
        };

        let mut params = vec![];

        while self.curr_token != Token::RightParen {
            let field = match self.parse_field(fun_id) {
                Ok(f) => f,
                Err(err) => return err,
            };

            params.push(field);

            if self.curr_token == Token::Comma {
                self.skip();
            } else if self.curr_token != Token::RightParen {
                return self.err_consume(
                    fun_id,
                    ParserErrorKind::ExpectedToken(Token::RightParen),
                    None,
                    &Self::ITEM_SYNC,
                );
            }
        }

        self.spans[paren_id].end = match self.curr_token {
            Token::RightParen => self.skip().end - 1,
            _ => {
                return self.err_consume(
                    fun_id,
                    ParserErrorKind::ExpectedToken(Token::RightParen),
                    None,
                    &Self::ITEM_SYNC,
                )
            }
        };

        let ret = if self.curr_token == Token::Arrow {
            let arrow_id = self.consume();

            let ttpe = match self.curr_token {
                Token::Identifier(spur) => Name {
                    id: self.consume(),
                    spur,
                },
                _ => {
                    return self.err_consume(
                        fun_id,
                        ParserErrorKind::expected_identifier(),
                        None,
                        &Self::ITEM_SYNC,
                    )
                }
            };

            Some((arrow_id, ttpe))
        } else {
            None
        };

        let body = match self.curr_token {
            Token::LeftBrace => self.parse_expr(None).expr(),
            _ => self.err_consume(
                fun_id,
                ParserErrorKind::ExpectedToken(Token::LeftBrace),
                None,
                &Self::ITEM_SYNC,
            ),
        };

        Item::Fun {
            fun_id,
            attrs,
            name,
            paren_id,
            params,
            ret,
            body,
        }
    }
}
