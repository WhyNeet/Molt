use crate::{
    literal::{Literal, Type},
    operator::Operator,
    statement::Statement,
};

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
    Conditional {
        condition: Box<Expression>,
        body: Vec<Statement>,
        alternative: Option<Box<Expression>>,
    },
    MemberAccess {
        expr: Box<Expression>,
        ident: String,
    },
    Call {
        expr: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Cast {
        expr: Box<Expression>,
        ty: Type,
    },
    Loop(Vec<Statement>),
    Continue,
    Break,
}
