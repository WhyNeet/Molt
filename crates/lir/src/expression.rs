use std::rc::Rc;

use common::{Literal, Type};

use crate::operator::{BinaryOperator, UnaryOperator};

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: StaticExpression,
        operator: BinaryOperator,
        right: StaticExpression,
        ty: Type,
    },
    Unary {
        operator: UnaryOperator,
        expr: StaticExpression,
        ty: Type,
    },
    MemberAccess {
        expr: Rc<Expression>,
        ident: String,
        ty: Type,
    },
    Cast {
        expr: Rc<StaticExpression>,
        ty: Type,
    },
    Call {
        expr: Rc<Expression>,
        arguments: Vec<Rc<Expression>>,
        ty: Type,
    },
    Static(Rc<StaticExpression>, Type),
}

#[derive(Debug)]
pub enum StaticExpression {
    Literal(Rc<Literal>),
    Identifier(u64),
    FnIdentifier(String),
    Ptr(Rc<StaticExpression>),
}
