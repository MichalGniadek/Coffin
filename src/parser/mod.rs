mod error_node;
mod expr_parsing;
mod item_parsing;
pub mod spans_table;

use self::{error_node::ErrorNode, spans_table::SpansTable};
use crate::{
    ast::{Ast, Field, Id, Name},
    error::{ParserError, ParserErrorKind},
    lexer::Token,
};
use lasso::RodeoResolver;
use logos::{Lexer, Span};

pub fn parse(lexer: Lexer<'_, Token>) -> (Ast, SpansTable, RodeoResolver) {
    let mut parser = Parser {
        lexer,
        spans: SpansTable::new(),
        curr_token: Token::EOF,
        curr_span: Span::default(),
    };
    parser.advance();
    let mut items = vec![];

    while parser.curr_token != Token::EOF {
        items.push(parser.parse_item());
    }

    (
        Ast::new(items, parser.spans.max_id()),
        parser.spans,
        parser.lexer.extras.into_resolver(),
    )
}

struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    spans: SpansTable,

    curr_token: Token,
    curr_span: Span,
}

impl Parser<'_> {
    const ITEM_SYNC: [Token; 3] = [Token::Fun, Token::HashBracket, Token::EOF];
    const EXPR_SYNC: [Token; 4] = const_concat(Self::ITEM_SYNC, [Token::RightBrace]);

    /// Shouldn't be called directly.
    fn advance(&mut self) {
        self.curr_token = self.lexer.next().unwrap_or(Token::EOF);
        self.curr_span = self.lexer.span();
    }

    /// Advances to the next token and returns the previous span.
    fn skip(&mut self) -> Span {
        let span = self.curr_span.clone();
        self.advance();
        span
    }

    /// Advances to the next token, add span to the Spans and return the
    /// corresponding id.
    fn consume(&mut self) -> Id {
        let span = self.curr_span.clone();
        self.advance();
        self.spans.push(span)
    }

    /// Advances to the next token, add span to the Spans and return the
    /// corresponding id. Panics if the next token isn't the correct.
    fn consume_expect(&mut self, token: Token) -> Id {
        assert!(
            self.curr_token == token,
            "Compiler error: expected '{:?}' token.",
            token
        );
        self.consume()
    }

    fn err_consume<I, S, T>(
        &mut self,
        ast_node_id: I,
        kind: ParserErrorKind,
        err_span: S,
        sync_tokens: &[Token],
    ) -> T
    where
        I: Into<Option<Id>>,
        S: Into<Option<Span>>,
        T: From<ErrorNode>,
    {
        let ast_node_id = match ast_node_id.into() {
            Some(id) => id,
            None => self.spans.push(self.curr_span.clone()),
        };

        let err_span = match err_span.into() {
            Some(span) => span,
            None => self.curr_span.clone(),
        };

        while !sync_tokens.contains(&self.curr_token) {
            self.spans[ast_node_id].end = self.curr_span.end;
            self.advance();
        }

        ErrorNode(ast_node_id, ParserError(kind, err_span)).into()
    }

    /// If a token isn't one of '(', '{', '[' it returns an empty vec.
    fn parse_delimited_tokens(&mut self) -> Vec<(Id, Token)> {
        let delimiter = match self.curr_token {
            Token::LeftBrace => Token::RightBrace,
            Token::LeftBracket => Token::RightBracket,
            Token::LeftParen => Token::RightParen,
            _ => return vec![],
        };

        let mut vec = vec![];
        while self.curr_token != delimiter && self.curr_token != Token::EOF {
            let token = self.curr_token;
            let id = self.consume();
            vec.push((id, token));
        }

        let token = self.curr_token;
        let id = self.consume();
        vec.push((id, token));

        vec
    }

    fn parse_field<T: From<ErrorNode>>(&mut self, ast_node_id: Id) -> Result<Field, T> {
        let name = match self.curr_token {
            Token::Identifier(spur) => Name {
                id: self.consume(),
                spur,
            },
            _ => {
                return Err(self.err_consume(
                    ast_node_id,
                    ParserErrorKind::expected_identifier(),
                    None,
                    &Self::ITEM_SYNC,
                ))
            }
        };

        let colon_id = match self.curr_token {
            Token::Colon => self.consume(),
            _ => {
                return Err(self.err_consume(
                    ast_node_id,
                    ParserErrorKind::ExpectedToken(Token::Colon),
                    None,
                    &Self::ITEM_SYNC,
                ))
            }
        };

        let ttpe = match self.curr_token {
            Token::Identifier(spur) => Name {
                id: self.consume(),
                spur,
            },
            _ => {
                return Err(self.err_consume(
                    ast_node_id,
                    ParserErrorKind::expected_identifier(),
                    None,
                    &Self::ITEM_SYNC,
                ))
            }
        };

        Ok(Field {
            name,
            colon_id,
            ttpe,
        })
    }
}

const fn const_concat<const A: usize, const B: usize, const C: usize>(
    a: [Token; A],
    b: [Token; B],
) -> [Token; C] {
    // Check lengths
    let _ = [0; 1][A + B - C];

    let mut out = [Token::EOF; C];

    let mut i = 0;

    while i < A {
        out[i] = a[i];
        i += 1;
    }

    while i < A + B {
        out[i] = b[i - A];
        i += 1;
    }

    out
}
