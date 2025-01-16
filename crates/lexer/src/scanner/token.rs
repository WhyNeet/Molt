use super::keywords::Keyword;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn as_ident(&self) -> Option<&str> {
        match self.kind {
            TokenKind::Ident(ref ident) => Some(ident),
            _ => None,
        }
    }

    pub fn as_keyword(&self) -> Option<Keyword> {
        match self.kind {
            TokenKind::Keyword(kw) => Some(kw),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal> {
        match self.kind {
            TokenKind::Literal(ref literal) => Some(literal),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// Comment surrounded by /* */
    BlockComment { terminated: bool },

    /// An identifier or keyword, e.g. `ident` or `continue`.
    Ident(String),

    /// A keyword, e.g. `fun`, `let`, ...
    Keyword(Keyword),

    /// Literals, e.g. `12u8`, `1.0e-40`, `b"123"`.
    Literal(Literal),

    /// `::`
    PathSep,
    /// `->`
    RArrow,
    /// `<-`
    LArrow,

    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `@`
    At,
    /// `#`
    Pound,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,

    /// `<=`
    Le,
    /// `>=`
    Ge,
    /// `==`
    EqEq,
    /// `!=`
    Ne,
    /// `&&`
    AndAnd,
    /// '||'
    OrOr,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal {
    pub symbol: String,
    pub suffix: Option<String>,
    pub kind: LiteralKind,
}

/// Enum representing the literal types supported by the lexer.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// `12_u8`, `0o100`, `0b120i99`, `1f32`.
    Int { base: Base, empty_int: bool },
    /// `12.34f32`, `1e3`, but not `1f32`.
    Float { base: Base, empty_exponent: bool },
    /// `'a'`, `'\\'`, `'''`, `';`
    Char { terminated: bool },
    /// `true` or `false`
    Bool,
    /// `b'a'`, `b'\\'`, `b'''`, `b';`
    // Byte { terminated: bool },
    /// `"abc"`, `"abc`
    Str { terminated: bool },
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0b".
    Binary = 2,
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0x".
    Hexadecimal = 16,
}
