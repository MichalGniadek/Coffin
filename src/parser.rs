use std::iter::Peekable;

use crate::{
    ast::{Attr, BinOpKind, Expr, Id, UntypedAst},
    error::{self, ParserErrorKind},
    lexer::Token,
};
use error::ParserError;
use logos::{Lexer, Span, SpannedIter};

pub fn parse<'a>(lexer: Lexer<'a, Token>) -> (UntypedAst, Vec<ParserError>) {
    let mut parser = Parser {
        lexer: lexer.spanned().peekable(),
        span: Span::default(),
        id: Id(0),
        spans: vec![],
        errors: vec![],
    };

    let mut attrs = vec![];

    while let Some(token) = parser.next() {
        match token {
            Token::HashBracket => attrs.extend(parser.parse_attributes()),
            // Token::Identifier(_identifier) => {}
            // Token::Fun => {}
            // Token::Struct => {}
            // Token::Poisoned => {}
            Token::Comment => continue,
            _ => {
                parser.expr_err(ParserErrorKind::UnexpectedTokenWhileParsingItem);
            }
        }
    }

    (
        UntypedAst {
            spans: parser.spans.clone(),
        },
        parser.errors,
    )
}

struct Parser<'a> {
    lexer: Peekable<SpannedIter<'a, Token>>,
    span: Span,
    id: Id,
    spans: Vec<Span>,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    #![allow(dead_code)]

    /// Gets the next token
    fn next(&mut self) -> Option<Token> {
        self.lexer.next().map(|(t, span)| {
            self.span = span;
            t
        })
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(t, _)| t).copied()
    }

    fn expr_err(&mut self, kind: ParserErrorKind) -> Expr {
        self.errors.push(ParserError(kind, self.span.clone()));
        Expr::Error(self.span_and_id())
    }

    fn err_none<T>(&mut self, kind: ParserErrorKind) -> Option<T> {
        self.errors.push(ParserError(kind, self.span.clone()));
        None
    }

    fn span_and_id(&mut self) -> Id {
        self.spans.push(self.span.clone());

        let id = self.id;
        self.id = Id(self.id.0 + 1);
        id
    }

    fn parse_grouping<F, T>(&mut self, seperator: Token, stop: Token, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut v = Vec::new();

        while let Some(token) = self.peek() {
            if token == seperator {
                if v.len() == 0 {
                    panic!("TODO");
                    // break;
                } else {
                    self.next();
                }
            } else if token == stop {
                self.next();
                break;
            }

            v.push(f(self));
        }

        v
    }

    fn parse_attributes(&mut self) -> Vec<Attr> {
        self.parse_grouping(Token::Comma, Token::RightBracket, |slf| Attr {
            ident: match slf.next() {
                Some(Token::Identifier(s)) => Some(s),
                Some(_) => slf.err_none(ParserErrorKind::AttributeMustBeAnIdentifier),
                None => None,
            },

            args: match slf.peek() {
                Some(Token::LeftParen) => {
                    slf.next();
                    slf.parse_grouping(Token::Comma, Token::LeftParen, |slf| slf.parse_expr(0))
                }
                _ => vec![],
            },

            id: slf.span_and_id(),
        })
    }

    fn parse_global(&mut self) {}
    fn parse_function(&mut self) {}
    fn parse_struct(&mut self) {}

    fn parse_expr(&mut self, min_binding_power: u8) -> Expr {
        let tree = self.parse_prefix();
        let tree = self.parse_infix(tree, min_binding_power);
        tree
    }

    fn parse_prefix(&mut self) -> Expr {
        match self.next() {
            Some(Token::Int(i)) => Expr::Int(self.span_and_id(), i),
            Some(_) => self.expr_err(ParserErrorKind::TokenIsntAPrefixToken),
            None => self.expr_err(ParserErrorKind::UnfinishedExpression),
        }
    }

    #[allow(unreachable_code)]
    fn parse_infix(&mut self, tree: Expr, min_binding_power: u8) -> Expr {
        let mut tree = tree;

        while let Some((token, Some(binding_power))) = self
            .peek()
            .map(|t| (t, t.check_infix_binding_power(min_binding_power)))
        {
            self.next();

            tree = match token {
                Token::Plus => Expr::Binary(
                    self.span_and_id(),
                    BinOpKind::Add,
                    Box::new(tree),
                    Box::new(self.parse_expr(binding_power)),
                ),
                Token::Comment => continue,
                _ => panic!(
                    "Unexpected infix token {:?}, but this shouldn't happen.",
                    token
                ),
            }
        }

        tree
    }
}
