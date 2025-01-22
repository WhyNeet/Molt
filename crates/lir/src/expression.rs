use std::rc::Rc;

use common::{Literal, Type};

use crate::operator::{BinaryOperator, UnaryOperator};

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: StaticExpression,
        operator: BinaryOperator,
        right: StaticExpression,
    },
    Unary {
        operator: UnaryOperator,
        expr: StaticExpression,
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
    Static(Rc<StaticExpression>),
}

#[derive(Debug)]
pub enum StaticExpression {
    Literal(Rc<Literal>),
    Identifier(String),
}
