use std::fmt;

use common::Operator;

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
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

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitXor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
        }
    }
}

impl From<&Operator> for BinaryOperator {
    fn from(value: &Operator) -> Self {
        match value {
            Operator::Add => Self::Add,
            Operator::And => Self::And,
            Operator::BitXor => Self::BitXor,
            Operator::Div => Self::Div,
            Operator::Eq => Self::Eq,
            Operator::Ge => Self::Ge,
            Operator::Gt => Self::Gt,
            Operator::Le => Self::Le,
            Operator::Lt => Self::Lt,
            Operator::Mul => Self::Mul,
            Operator::Ne => Self::Ne,
            Operator::Or => Self::Or,
            Operator::Shl => Self::Shl,
            Operator::Shr => Self::Shr,
            Operator::Sub => Self::Sub,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Neg,
    Not,
    Ref,
    Deref,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Ref => write!(f, "&"),
            Self::Deref => write!(f, "*"),
        }
    }
}

impl From<&Operator> for UnaryOperator {
    fn from(value: &Operator) -> Self {
        match value {
            Operator::Neg => Self::Neg,
            Operator::Not => Self::Not,
            Operator::Ref => Self::Ref,
            Operator::Deref => Self::Deref,
            _ => unreachable!(),
        }
    }
}
