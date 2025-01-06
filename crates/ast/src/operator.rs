use lexer::scanner::token::{Token, TokenKind};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Not,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

impl TryFrom<&Token> for Operator {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Plus => Ok(Self::Add),
            TokenKind::Minus => Ok(Self::Sub),
            TokenKind::Star => Ok(Self::Mul),
            TokenKind::Slash => Ok(Self::Div),
            TokenKind::AndAnd => Ok(Self::And),
            TokenKind::OrOr => Ok(Self::Or),
            TokenKind::Caret => Ok(Self::Xor),
            ref other => Err(format!("unknown operator: `{other:?}`")),
        }
    }
}
