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

    let mut items = vec![];

    while parser.peek() != Token::EOF {
        items.push(parser.parse_item());
    }
    
    UntypedAst {
        items,
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
    fn destruct(self) -> (Expr, bool) {
        match self {
            Ok(e) => (e, false),
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
    const ITEM_SYNC: [Token; 2] = [Token::Fun, Token::EOF];
    const EXPR_SYNC: [Token; 3] = concat(Self::ITEM_SYNC, [Token::RightBrace]);

    /// Peeks the next token.
    fn peek(&mut self) -> Token {
        self.lexer.peek().map_or(Token::EOF, |(t, _)| t.clone())
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

    /// Advances to the next token, add span to the Spans and return the
    /// corresponding id. Panics if the next token isn't the correct.
    fn consume_expect(&mut self, token: Token) -> Id {
        assert!(
            self.peek() == token,
            "Compiler error: expected '{:?}' token.",
            token
        );
        self.consume()
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
            Token::Fun => self.parse_fun(),
            _ => todo!(),
        }
    }

    fn parse_fun(&mut self) -> Item {
        let id = self.consume_expect(Token::Fun);

        let name = match self.peek() {
            Token::Identifier(s) => ItemName(self.consume(), s),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        match self.peek() {
            Token::LeftParen => self.skip(),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        match self.peek() {
            Token::RightParen => self.skip(),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        let expr = match self.peek() {
            Token::LeftBrace => self.parse_block().expr(),
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
            Token::Int(i) => Ok(Expr::Int(self.consume(), i)),
            Token::LeftBrace => self.parse_block(),
            _ => Panic(Expr::Error(
                self.err_consume(&Self::EXPR_SYNC),
                ParserErrorKind::TokenIsntAPrefixToken,
            )),
        }
    }

    fn parse_block(&mut self) -> ParserResult {
        let id = self.consume_expect(Token::LeftBrace);

        let mut exprs = Vec::new();

        loop {
            if self.peek() == Token::RightBrace {
                self.spans[id].end = self.skip().end - 1;
                return Ok(Expr::Block(id, exprs));
            }

            let (expr, is_panic) = self.parse_expr(0).destruct();
            exprs.push(expr);

            if is_panic && self.peek() != Token::RightBrace {
                self.spans[id].end = self.spans[id].start;
                return Panic(Expr::Block(id, exprs));
            }
        }
    }

    fn parse_infix(&mut self, mut tree: Expr, min_binding_power: u8) -> ParserResult {
        // Associativity
        const LEFT: u8 = 0;
        const _RIGHT: u8 = 1;

        loop {
            tree = match self.peek() {
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
