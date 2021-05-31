use super::Parser;
use crate::{
    ast::{Attr, Attrs, Item, Name},
    error::ParserErrorKind,
    lexer::Token,
};

impl Parser<'_> {
    pub(super) fn parse_item(&mut self) -> Item {
        let attrs = match self.peek() {
            Token::HashBracket => self.parse_attributes(),
            _ => Attrs::None,
        };

        match self.peek() {
            Token::Fun => self.parse_fun(attrs),
            _ => todo!(),
        }
    }

    pub(super) fn parse_attributes(&mut self) -> Attrs {
        let brackets_id = self.consume();
        let mut attrs = vec![];

        while self.peek() != Token::RightBracket {
            let name = match self.peek() {
                Token::Identifier(s) => Name(self.consume(), s),
                _ => {
                    return self.err_consume_append(
                        brackets_id,
                        ParserErrorKind::expected_identifier(),
                        &Self::ITEM_SYNC,
                    )
                }
            };

            attrs.push(Attr(name, self.parse_delimited_tokens()));

            if self.peek() == Token::Comma {
                self.skip();
            } else if self.peek() != Token::RightBracket {
                return self.err_consume_append(
                    brackets_id,
                    ParserErrorKind::ExpectedToken(Token::RightBracket),
                    &Self::ITEM_SYNC,
                );
            }
        }

        self.spans[brackets_id].end = self.skip().end - 1;
        Attrs::Ok(brackets_id, attrs)
    }

    pub(super) fn parse_fun(&mut self, attrs: Attrs) -> Item {
        let fun_id = self.consume_expect(Token::Fun);

        let name = match self.peek() {
            Token::Identifier(s) => Name(self.consume(), s),
            _ => {
                return self.err_consume_append(
                    fun_id,
                    ParserErrorKind::expected_identifier(),
                    &Self::ITEM_SYNC,
                )
            }
        };

        match self.peek() {
            Token::LeftParen => self.skip(),
            _ => {
                return self.err_consume_append(
                    fun_id,
                    ParserErrorKind::ExpectedToken(Token::LeftParen),
                    &Self::ITEM_SYNC,
                )
            }
        };

        match self.peek() {
            Token::RightParen => self.skip(),
            _ => {
                return self.err_consume_append(
                    fun_id,
                    ParserErrorKind::ExpectedToken(Token::RightParen),
                    &Self::ITEM_SYNC,
                )
            }
        };

        let expr = match self.peek() {
            Token::LeftBrace => self.parse_block().expr(),
            _ => self.err_consume_append(
                fun_id,
                ParserErrorKind::ExpectedToken(Token::LeftBrace),
                &Self::ITEM_SYNC,
            ),
        };

        Item::Fun(fun_id, attrs, name, expr)
    }
}
