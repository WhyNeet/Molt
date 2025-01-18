use std::rc::Rc;

use ast::literal::{Literal, Type};

use crate::operator::{BinaryOperator, UnaryOperator};

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: PrimaryExpression,
        operator: BinaryOperator,
        right: PrimaryExpression,
    },
    Unary {
        operator: UnaryOperator,
        expr: PrimaryExpression,
    },
    MemberAccess {
        expr: Rc<Expression>,
        ident: String,
    },
    Cast {
        expr: Rc<Expression>,
        ty: Type,
    },
    Call {
        expr: Rc<Expression>,
        arguments: Vec<Rc<Expression>>,
    },
}

#[derive(Debug)]
pub enum PrimaryExpression {
    Literal(Rc<Literal>),
    Identifier(String),
}
