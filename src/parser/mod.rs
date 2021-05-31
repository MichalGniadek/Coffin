mod error_node;
mod expr_parsing;
mod item_parsing;

use crate::{
    ast::{Expr, Field, Id, Name, Spans, UntypedAst},
    error::{ParserError, ParserErrorKind},
    lexer::Token,
};
use logos::{Lexer, Span};

pub fn parse(lexer: Lexer<'_, Token>) -> UntypedAst {
    let mut parser = Parser {
        lexer,
        spans: Spans::new(),
        curr_token: Token::EOF,
        curr_span: Span::default(),
    };
    parser.advance();
    let mut items = vec![];

    while parser.curr_token != Token::EOF {
        items.push(parser.parse_item());
    }

    UntypedAst {
        items,
        spans: parser.spans,
        rodeo: parser.lexer.extras.into_resolver(),
    }
}

struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    spans: Spans,

    curr_token: Token,
    curr_span: Span,
}

enum ParsedExpr {
    Good(Expr),
    Panic(Expr),
}
use ParsedExpr::{Good, Panic};

use self::error_node::ErrorNode;

impl ParsedExpr {
    fn expr(self) -> Expr {
        match self {
            Good(e) => e,
            Panic(e) => e,
        }
    }
    fn destruct(self) -> (Expr, bool) {
        match self {
            Good(e) => (e, false),
            Panic(e) => (e, true),
        }
    }
}

const fn concat<const A: usize, const B: usize, const C: usize>(
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

impl Parser<'_> {
    const ITEM_SYNC: [Token; 3] = [Token::Fun, Token::HashBracket, Token::EOF];
    const EXPR_SYNC: [Token; 4] = concat(Self::ITEM_SYNC, [Token::RightBrace]);

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

    /// Consumes tokens until it get to sync token and return error variant of an AST type.
    fn err_consume<T: From<ErrorNode>>(&mut self, kind: ParserErrorKind, tokens: &[Token]) -> T {
        let span = self.curr_span.clone();
        let id = self.spans.push(span);
        self.err_consume_append(id, kind, tokens)
    }

    /// Consumes tokens until it get to sync token and return error variant of an AST type.
    /// The id of the node will be the same as the passed one.
    fn err_consume_append<T: From<ErrorNode>>(
        &mut self,
        id: Id,
        kind: ParserErrorKind,
        tokens: &[Token],
    ) -> T {
        let err_span = self.curr_span.clone();

        while !tokens.contains(&self.curr_token) {
            self.spans[id].end = self.curr_span.end;
            self.advance();
        }

        ErrorNode(id, ParserError(kind, err_span)).into()
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

    /// In case of parsing error, err_consume_append will be called with error_id.
    fn parse_field<T: From<ErrorNode>>(&mut self, error_id: Id) -> Result<Field, T> {
        let name = match self.curr_token {
            Token::Identifier(s) => Name(self.consume(), s),
            _ => {
                return Err(self.err_consume_append(
                    error_id,
                    ParserErrorKind::expected_identifier(),
                    &Self::ITEM_SYNC,
                ))
            }
        };

        let colon_id = match self.curr_token {
            Token::Colon => self.consume(),
            _ => {
                return Err(self.err_consume_append(
                    error_id,
                    ParserErrorKind::ExpectedToken(Token::Colon),
                    &Self::ITEM_SYNC,
                ))
            }
        };

        let ttpe = match self.curr_token {
            Token::Identifier(s) => Name(self.consume(), s),
            _ => {
                return Err(self.err_consume_append(
                    error_id,
                    ParserErrorKind::expected_identifier(),
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
