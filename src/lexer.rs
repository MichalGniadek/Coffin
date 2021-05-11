use lasso::{Rodeo, Spur};
use logos::Logos;

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

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.extras.get_or_intern(lex.slice()))]
    Identifier(Spur),
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Int(i32),
    // Float(f32),
    #[token("and")]
    And,
    #[token("else")]
    Else,
    #[token("false")]
    False,
    #[token("fun")]
    Fun,
    #[token("if")]
    If,
    #[token("or")]
    Or,
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
    #[token("spirv")]
    Spirv,
    #[regex(r"//.*")]
    Comment,
    #[error]
    #[regex(r"[ \r\n\t\f]+", logos::skip)]
    Poisoned,
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
