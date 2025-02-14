use crate::{
    token::{Token, TokenKind},
    Type,
};

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
    Ref,
    Deref,
}

impl Operator {
    pub fn produces(&self, operand_type: Type) -> Type {
        match self {
            Self::Eq | Self::Ge | Self::Gt | Self::Le | Self::Lt | Self::Ne | Self::Not => {
                Type::Bool
            }
            Self::Ref => Type::Ptr(Box::new(operand_type)),
            Self::Deref => match operand_type {
                Type::Ptr(subtype) => subtype.as_ref().clone(),
                _ => unreachable!(),
            },
            _ => operand_type,
        }
    }

    pub fn accepts(&self, operand_type: &Type) -> bool {
        match self {
            Self::Eq | Self::Ge | Self::Gt | Self::Le | Self::Lt | Self::Ne => true,
            Self::Neg => operand_type.is_numeric(),
            Operator::Add => operand_type.is_numeric() || operand_type == &Type::Str,
            Operator::Sub => operand_type.is_numeric(),
            Operator::Mul | Operator::Div => operand_type.is_numeric(),
            Operator::And | Operator::Or | Operator::Not => operand_type == &Type::Bool,
            Operator::BitXor | Operator::Shl | Operator::Shr => operand_type.is_numeric(),
            Operator::Ref | Operator::Deref => true,
        }
    }
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
            TokenKind::GtGt => Ok(Self::Shr),
            TokenKind::LtLt => Ok(Self::Shl),
            TokenKind::And => Ok(Self::Ref),
            ref other => Err(format!("unknown operator: `{other:?}`")),
        }
    }
}
