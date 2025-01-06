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
