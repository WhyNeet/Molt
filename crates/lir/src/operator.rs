use ast::operator::Operator;

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
}

impl From<&Operator> for UnaryOperator {
    fn from(value: &Operator) -> Self {
        match value {
            Operator::Neg => Self::Neg,
            Operator::Not => Self::Not,
            _ => unreachable!(),
        }
    }
}
