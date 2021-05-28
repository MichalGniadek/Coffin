use super::Parser;
use crate::{
    ast::{Attrs, Item, Name},
    error::ParserErrorKind,
    lexer::Token,
};

impl Parser<'_> {
    pub(super) fn parse_item(&mut self) -> Item {
        match self.peek() {
            Token::Fun => self.parse_fun(),
            _ => todo!(),
        }
    }

    pub(super) fn parse_fun(&mut self) -> Item {
        let id = self.consume_expect(Token::Fun);

        let name = match self.peek() {
            Token::Identifier(s) => Name(self.consume(), s),
            _ => {
                return self.err_consume_append(
                    id,
                    ParserErrorKind::ExpectedIdentifier,
                    &Self::ITEM_SYNC,
                )
            }
        };

        match self.peek() {
            Token::LeftParen => self.skip(),
            _ => return self.err_consume_append(id, ParserErrorKind::TODOError, &Self::ITEM_SYNC),
        };

        match self.peek() {
            Token::RightParen => self.skip(),
            _ => return self.err_consume_append(id, ParserErrorKind::TODOError, &Self::ITEM_SYNC),
        };

        let expr = match self.peek() {
            Token::LeftBrace => self.parse_block().expr(),
            _ => self.err_consume_append(id, ParserErrorKind::TODOError, &Self::ITEM_SYNC),
        };

        Item::Fun(id, Attrs::None, name, expr)
    }
}
