use crate::{
    ast::{BinOpKind, Expr, Id, Item, ItemName, Spans, UntypedAst},
    error::ParserErrorKind,
    lexer::Token,
};
use logos::{Lexer, Span, SpannedIter};
use std::iter::Peekable;

pub fn parse(lexer: Lexer<'_, Token>) -> UntypedAst {
    let mut parser = Parser {
        lexer: lexer.spanned().peekable(),
        spans: Spans::new(),
    };

    let fun = parser.parse_item();

    UntypedAst {
        item: fun,
        spans: parser.spans,
    }
}

struct Parser<'a> {
    lexer: Peekable<SpannedIter<'a, Token>>,
    spans: Spans,
}

enum ParserResult {
    Ok(Expr),
    Panic(Expr),
}
use ParserResult::{Ok, Panic};
impl ParserResult {
    fn expr(self) -> Expr {
        match self {
            Ok(e) => e,
            Panic(e) => e,
        }
    }
}

impl Parser<'_> {
    const ITEM_SYNC: [Token; 1] = [Token::Fun];
    const EXPR_SYNC: [Token; 2] = [Token::RightBrace, Token::Fun];

    /// Peeks the next token.
    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(t, _)| *t)
    }

    /// Peeks the next span.
    fn peek_span(&mut self) -> Span {
        self.lexer.peek().map_or(0..0, |(_, s)| s.clone())
    }

    /// Advances to the next token and returns the previous span.
    fn skip(&mut self) -> Span {
        let span = self.peek_span();
        self.lexer.next();
        span
    }

    /// Advances to the next token, add span to the Spans and return the
    /// corresponding id.
    fn consume(&mut self) -> Id {
        let span = self.peek_span();
        self.lexer.next();
        self.spans.push(span)
    }

    /// Advances until it peeks one of the token in tokens and adds all
    /// the previous spans to Spans and return the corresponding id.
    /// You should pass one of the sync tokens arrays.
    fn err_consume(&mut self, tokens: &[Token]) -> Id {
        let span = self.peek_span();
        let id = self.spans.push(span);
        self.err_consume_add_to(id, tokens)
    }

    /// Advances until it peeks one of the token in tokens and adds all
    /// the previous spans to the passed id and return the passed id.
    /// You should pass one of the sync tokens arrays.
    fn err_consume_add_to(&mut self, id: Id, tokens: &[Token]) -> Id {
        while let Some((token, span)) = self.lexer.peek() {
            if !tokens.contains(token) {
                self.spans[id].end = span.end;
                self.lexer.next();
            } else {
                break;
            }
        }
        id
    }

    fn parse_item(&mut self) -> Item {
        match self.peek() {
            Some(Token::Fun) => self.parse_fun(),
            _ => todo!(),
        }
    }

    fn parse_fun(&mut self) -> Item {
        assert!(
            self.peek() == Some(Token::Fun),
            "Compiler error: parse_fun should be called with fun keyword remaining."
        );
        let id = self.consume();

        let name = match self.peek() {
            Some(Token::Identifier(s)) => ItemName(self.consume(), s),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        match self.peek() {
            Some(Token::LeftParen) => self.skip(),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        match self.peek() {
            Some(Token::RightParen) => self.skip(),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        let expr = match self.peek() {
            Some(Token::LeftBrace) => self.parse_block().expr(),
            _ => Expr::Error(
                self.err_consume(&Self::EXPR_SYNC),
                ParserErrorKind::TODOError,
            ),
        };

        Item::Fun(id, name, expr)
    }

    fn parse_expr(&mut self, min_binding_power: u8) -> ParserResult {
        match self.parse_prefix() {
            Ok(e) => self.parse_infix(e, min_binding_power),
            Panic(e) => Panic(e),
        }
    }

    fn parse_prefix(&mut self) -> ParserResult {
        match self.peek() {
            Some(Token::Int(i)) => Ok(Expr::Int(self.consume(), i)),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(_) | None => Panic(Expr::Error(
                self.err_consume(&Self::EXPR_SYNC),
                ParserErrorKind::TokenIsntAPrefixToken,
            )),
        }
    }

    fn parse_block(&mut self) -> ParserResult {
        assert!(
            self.peek() == Some(Token::LeftBrace),
            "Compiler error: parse_fun should be called with left brace keyword remaining."
        );
        let id = self.consume();

        let mut exprs = Vec::new();

        while let Some(token) = self.peek() {
            // Block ended
            if token == Token::RightBrace {
                self.spans[id].end = self.skip().end - 1;
                break;
            }

            let expr = self.parse_expr(0);

            match expr {
                Ok(e) => exprs.push(e),
                Panic(e) => {
                    exprs.push(e);
                    if let Some(Token::RightBrace) = self.peek() {
                        self.spans[id].end = self.skip().end - 1;
                    }
                    return Panic(Expr::Block(id, exprs));
                }
            }
        }

        Ok(Expr::Block(id, exprs))
    }

    fn parse_infix(&mut self, mut tree: Expr, min_binding_power: u8) -> ParserResult {
        // Associativity
        const LEFT: u8 = 0;
        const _RIGHT: u8 = 1;

        while let Some(token) = self.peek() {
            tree = match token {
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

        Ok(tree)
    }
}
