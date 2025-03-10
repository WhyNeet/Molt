#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Keyword {
    Fun,
    Let,
    Loop,
    If,
    Else,
    Return,
    Break,
    Continue,
    As,
    Mut,
    Struct,
    Self_,
}

impl TryFrom<&str> for Keyword {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "fun" => Ok(Keyword::Fun),
            "let" => Ok(Keyword::Let),
            "loop" => Ok(Keyword::Loop),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "return" => Ok(Keyword::Return),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "as" => Ok(Keyword::As),
            "mut" => Ok(Keyword::Mut),
            "struct" => Ok(Keyword::Struct),
            "self" => Ok(Keyword::Self_),
            other => Err(format!("`{other}` is not a keyword.")),
        }
    }
}
