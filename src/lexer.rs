use lasso::{Rodeo, Spur};
use logos::{Lexer, Logos};
use std::fmt::Display;

pub fn lex(src: &str) -> Lexer<Token> {
    let mut lexer = Token::lexer(src);

    for s in ["void", "int"] {
        lexer.extras.get_or_intern_static(s);
    }

    lexer
}

#[derive(Debug, PartialEq, Clone, Copy, Logos)]
#[logos(extras = Rodeo)]
pub enum Token {
    /// (
    #[token("(")]
    LeftParen,
    /// )
    #[token(")")]
    RightParen,
    /// {
    #[token("{")]
    LeftBrace,
    /// }
    #[token("}")]
    RightBrace,
    /// [
    #[token("[")]
    LeftBracket,
    /// #[
    #[token("#[")]
    HashBracket,
    /// ]
    #[token("]")]
    RightBracket,
    /// ,
    #[token(",")]
    Comma,
    /// .
    #[token(".")]
    Dot,

    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    /// :
    #[token(":")]
    Colon,
    #[token("*")]
    Star,
    #[token("**")]
    DoubleStar,

    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    DoubleEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    /// ->
    #[token("->")]
    Arrow,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.extras.get_or_intern(lex.slice()))]
    Identifier(Spur),
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Int(i32),
    // Float(f32),
    #[token("else")]
    Else,
    #[token("false")]
    False,
    #[token("fun")]
    Fun,
    #[token("if")]
    If,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("struct")]
    Struct,
    #[token("loop")]
    Loop,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("spirv")]
    Spirv,
    #[regex(r"//.*")]
    Comment,
    EOF,
    #[error]
    #[regex(r"[ \r\n\t\f]+", logos::skip)]
    Poisoned,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "'('"),
            Token::RightParen => write!(f, "')'"),
            Token::LeftBrace => write!(f, "'{{'"),
            Token::RightBrace => write!(f, "'}}'"),
            Token::LeftBracket => write!(f, "'['"),
            Token::HashBracket => write!(f, "'#['"),
            Token::RightBracket => write!(f, "']'"),
            Token::Comma => write!(f, "','"),
            Token::Dot => write!(f, "'.'"),
            Token::Minus => write!(f, "'-'"),
            Token::Plus => write!(f, "'+'"),
            Token::Slash => write!(f, "'/'"),
            Token::Percent => write!(f, "'%'"),
            Token::Colon => write!(f, "':'"),
            Token::Star => write!(f, "'*'"),
            Token::DoubleStar => write!(f, "'**'"),
            Token::Bang => write!(f, "'!'"),
            Token::BangEqual => write!(f, "'!='"),
            Token::Equal => write!(f, "'='"),
            Token::DoubleEqual => write!(f, "'=='"),
            Token::Greater => write!(f, "'>'"),
            Token::GreaterEqual => write!(f, "'>='"),
            Token::Less => write!(f, "'<'"),
            Token::LessEqual => write!(f, "'<='"),
            Token::Arrow => write!(f, "'->'"),
            Token::Identifier(_) => write!(f, "Identifier"),
            Token::Int(_) => write!(f, "Int"),
            Token::Else => write!(f, "'else'"),
            Token::False => write!(f, "'false'"),
            Token::Fun => write!(f, "'fun'"),
            Token::If => write!(f, "'if'"),
            Token::Return => write!(f, "'return'"),
            Token::True => write!(f, "'true'"),
            Token::Struct => write!(f, "'struct'"),
            Token::Loop => write!(f, "'loop'"),
            Token::Let => write!(f, "'let'"),
            Token::Mut => write!(f, "'mut'"),
            Token::Spirv => write!(f, "'spirv'"),
            Token::Comment => write!(f, "'//'"),
            Token::EOF => write!(f, "EOF"),
            Token::Poisoned => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn identifiers() {
        assert_debug_snapshot!(Token::lexer("asd . _asd . _ . asd90 . 90asd").collect::<Vec<_>>(), @r###"
        [
            Identifier(
                Spur {
                    key: 1,
                },
            ),
            Dot,
            Identifier(
                Spur {
                    key: 2,
                },
            ),
            Dot,
            Identifier(
                Spur {
                    key: 3,
                },
            ),
            Dot,
            Identifier(
                Spur {
                    key: 4,
                },
            ),
            Dot,
            Int(
                90,
            ),
            Identifier(
                Spur {
                    key: 1,
                },
            ),
        ]
        "###);
    }
}
