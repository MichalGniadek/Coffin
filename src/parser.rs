use crate::{
    ast::{BinOpKind, Expr, Id, Item, ItemName, Visitor},
    error::ParserErrorKind,
    lexer::Token,
    pretty_print::PrettyPrint,
};
use logos::{Lexer, Span, SpannedIter};
use std::iter::Peekable;

pub fn parse<'a>(lexer: Lexer<'a, Token>) {
    let mut parser = Parser {
        lexer: lexer.spanned().peekable(),
        id: Id(0),
        spans: vec![],
    };

    let fun = parser.parse_fun();
    println!(
        "{}",
        PrettyPrint::new(parser.spans.clone()).visit_item(&fun)
    );
    let fun = parser.parse_fun();
    println!(
        "{}",
        PrettyPrint::new(parser.spans.clone()).visit_item(&fun)
    );
}

struct Parser<'a> {
    lexer: Peekable<SpannedIter<'a, Token>>,
    id: Id,
    spans: Vec<Span>,
}

type ParserResult = Result<Expr, Expr>;

trait ParserResultTrait {
    fn expr(self) -> Expr;
}

impl ParserResultTrait for ParserResult {
    fn expr(self) -> Expr {
        match self {
            Ok(e) => e,
            Err(e) => e,
        }
    }
}

impl<'a> Parser<'a> {
    const ITEM_SYNC: [Token; 1] = [Token::Fun];
    const EXPR_SYNC: [Token; 2] = [Token::RightBrace, Token::Fun];

    fn span(&mut self) -> Span {
        self.lexer.peek().map_or(0..0, |(_, span)| span.clone())
    }

    fn consume(&mut self) -> Id {
        let span = self.span();
        self.spans.push(span);
        self.lexer.next();

        let id = self.id;
        self.id = Id(self.id.0 + 1);
        id
    }

    fn err_consume(&mut self, tokens: &[Token]) -> Id {
        let mut fin_span = self.span();

        while let Some((token, span)) = self.lexer.peek() {
            if !tokens.contains(token) {
                fin_span = fin_span.start..span.end;
                self.lexer.next();
            } else {
                break;
            }
        }

        self.spans.push(fin_span.clone());

        let id = self.id;
        self.id = Id(self.id.0 + 1);
        id
    }

    fn err_consume_add_to(&mut self, id: Id, tokens: &[Token]) -> Id {
        let mut fin_span = self.spans[id.0 as usize].clone();

        while let Some((token, span)) = self.lexer.peek() {
            if !tokens.contains(token) {
                fin_span = fin_span.start..span.end;
                self.lexer.next();
            } else {
                break;
            }
        }

        self.spans[id.0 as usize] = fin_span;

        id
    }

    fn eat(&mut self) -> Span {
        let span = self.span();
        self.lexer.next();
        span
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(t, _)| *t)
    }

    fn parse_fun(&mut self) -> Item {
        // Fun keyword
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
            Some(Token::LeftParen) => self.eat(),
            _ => {
                return Item::Error(
                    self.err_consume_add_to(id, &Self::ITEM_SYNC),
                    ParserErrorKind::TODOError,
                )
            }
        };

        match self.peek() {
            Some(Token::RightParen) => self.eat(),
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
        let tree = self.parse_prefix().expr();
        let tree = self.parse_infix(tree, min_binding_power);
        tree
    }

    fn parse_prefix(&mut self) -> ParserResult {
        match self.peek() {
            Some(Token::Int(i)) => Ok(Expr::Int(self.consume(), i)),
            Some(Token::LeftBrace) => self.parse_block(),
            Some(_) | None => Err(Expr::Error(
                self.err_consume(&Self::EXPR_SYNC),
                ParserErrorKind::TokenIsntAPrefixToken,
            )),
        }
    }

    fn parse_block(&mut self) -> ParserResult {
        let id = self.consume();

        // Set the span so that first number represents { and the second represents }
        fn eat_right_brace(slf: &mut Parser, id: Id) {
            slf.spans[id.0 as usize] = (slf.spans[id.0 as usize].start)..(slf.eat().end - 1);
        }

        let mut exprs = Vec::new();

        while let Some(token) = self.peek() {
            // Block ended
            if token == Token::RightBrace {
                eat_right_brace(self, id);
                break;
            }

            let expr = self.parse_expr(0);

            match expr {
                Ok(e) => exprs.push(e),
                Err(e) => {
                    exprs.push(e);
                    if let Some(Token::RightBrace) = self.peek() {
                        eat_right_brace(self, id);
                    }
                    return Err(Expr::Block(id, exprs));
                }
            }
        }

        Ok(Expr::Block(id, exprs))
    }

    fn parse_infix(&mut self, tree: Expr, min_binding_power: u8) -> ParserResult {
        // Associativity
        const LEFT: u8 = 0;
        const RIGHT: u8 = 1;

        let mut tree = tree;

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
