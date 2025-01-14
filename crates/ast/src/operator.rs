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
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
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
            TokenKind::Caret => Ok(Self::BitXor),
            TokenKind::Gt => Ok(Self::Gt),
            TokenKind::Ge => Ok(Self::Ge),
            TokenKind::Lt => Ok(Self::Lt),
            TokenKind::Le => Ok(Self::Le),
            TokenKind::EqEq => Ok(Self::Eq),
            TokenKind::Ne => Ok(Self::Ne),
            ref other => Err(format!("unknown operator: `{other:?}`")),
        }
    }
}
