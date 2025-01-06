use crate::{literal::Literal, operator::Operator, statement::Statement};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Unary {
        operator: Operator,
        expr: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Identifier(String),
    Literal(Literal),
    Assignment {
        identifier: String,
        expr: Box<Expression>,
    },
    Block(Vec<Statement>),
}
