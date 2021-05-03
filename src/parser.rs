use crate::{
    ast::{Attr, BinOpKind, Expr, Id, UntypedAst},
    error::{self, ParserErrorKind},
    lexer::Token,
};
use error::ParserError;
use logos::{Lexer, Span, SpannedIter};
use std::{iter::Peekable, result::Result};

pub fn parse<'a>(lexer: Lexer<'a, Token>) -> (UntypedAst, Vec<ParserError>) {
    let mut parser = Parser {
        lexer: lexer.spanned().peekable(),
        curr_span: Span::default(),
        id: Id(0),
        spans: vec![],
        errors: vec![],
    };

    let mut attrs = vec![];

    while let Some(token) = parser.peek() {
        match token {
            Token::HashBracket => {
                parser.consume();
                attrs.extend(parser.parse_attributes())
            }
            // Token::Identifier(_identifier) => {}
            // Token::Fun => {}
            // Token::Struct => {}
            // Token::Poisoned => {}
            Token::Comment => {
                parser.consume();
                continue;
            }
            _ => {
                parser.err_expr(ParserErrorKind::UnexpectedTokenWhileParsingItem);
                parser.consume();
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
    curr_span: Span,
    id: Id,
    spans: Vec<Span>,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    #![allow(dead_code)]

    fn consume(&mut self) {
        self.lexer.next();
        if let Some((_, span)) = self.lexer.peek() {
            self.curr_span = span.clone();
        };
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(t, _)| *t)
    }

    fn skip_to(&mut self, tokens: &[Token]) {
        while !matches!(self.peek(), Some(t) if tokens.contains(&t)) {
            self.consume();
        }
    }

    fn err_expr(&mut self, kind: ParserErrorKind) -> Expr {
        self.errors.push(ParserError(kind, self.curr_span.clone()));
        Expr::Error(self.save_id())
    }

    fn err_none<T>(&mut self, kind: ParserErrorKind) -> Option<T> {
        self.errors.push(ParserError(kind, self.curr_span.clone()));
        None
    }

    fn save_id(&mut self) -> Id {
        self.spans.push(self.curr_span.clone());

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
            if token == stop || (token == seperator && v.len() > 0) {
                self.consume();
            }
            if token == stop {
                break;
            }

            v.push(f(self));
        }

        v
    }

    fn parse_attributes(&mut self) -> Vec<Option<Attr>> {
        self.parse_grouping(Token::Comma, Token::RightBracket, |slf| {
            Some(Attr {
                ident: match slf.peek() {
                    Some(Token::Identifier(s)) => {
                        slf.consume();
                        s
                    }
                    Some(Token::Comma) | Some(Token::RightBracket) | None => {
                        return slf.err_none(ParserErrorKind::MissingAttribute)
                    }
                    Some(_) => return slf.err_none(ParserErrorKind::AttributeMustBeAnIdentifier),
                },

                args: match slf.peek() {
                    Some(Token::LeftParen) => {
                        slf.consume();
                        slf.parse_grouping(Token::Comma, Token::RightParen, |slf| {
                            match slf.parse_expr(0) {
                                Result::Ok(expr) => expr,
                                Result::Err(expr) => {
                                    slf.skip_to(&[Token::Comma, Token::RightParen]);
                                    expr
                                }
                            }
                        })
                    }
                    _ => vec![],
                },

                id: slf.save_id(),
            })
        })
    }

    fn parse_global(&mut self) {}
    fn parse_function(&mut self) {}
    fn parse_struct(&mut self) {}

    fn parse_expr(&mut self, min_binding_power: u8) -> Result<Expr, Expr> {
        let tree = self.parse_prefix()?;
        let tree = self.parse_infix(tree, min_binding_power)?;
        Ok(tree)
    }

    fn parse_prefix(&mut self) -> Result<Expr, Expr> {
        match self.peek() {
            Some(Token::Int(i)) => {
                self.consume();
                Ok(Expr::Int(self.save_id(), i))
            }
            Some(Token::RightParen) | Some(Token::Comma) | None => {
                Err(self.err_expr(ParserErrorKind::UnfinishedExpression))
            }
            Some(_) => Err(self.err_expr(ParserErrorKind::TokenIsntAPrefixToken)),
        }
    }

    fn parse_infix(&mut self, tree: Expr, min_binding_power: u8) -> Result<Expr, Expr> {
        // Associativity
        const LEFT: u8 = 0;
        const RIGHT: u8 = 1;

        let mut tree = tree;

        while let Some(token) = self.peek() {
            tree = match token {
                // Token::T if binding_power + associativity > min_binding_power
                Token::Plus if 10 + LEFT > min_binding_power => {
                    self.consume();
                    Expr::Binary(
                        self.save_id(),
                        BinOpKind::Add,
                        Box::new(tree),
                        Box::new(self.parse_expr(10)?),
                    )
                }
                _ => break,
            }
        }

        Ok(tree)
    }
}
