use std::rc::Rc;

use common::{Literal, Type};

use crate::operator::{BinaryOperator, UnaryOperator};

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: StaticExpression,
        operator: BinaryOperator,
        right: StaticExpression,
        operand_ty: Type,
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
    Trunc {
        expr: Rc<StaticExpression>,
        ty: Type,
    },
    Ext {
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
    Identifier(String),
    FnIdentifier(String),
    Ptr(Rc<StaticExpression>),
}

impl StaticExpression {
    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Self::Literal(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&str> {
        match self {
            Self::Identifier(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_fn_ident(&self) -> Option<&str> {
        match self {
            Self::FnIdentifier(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_ptr(&self) -> Option<&StaticExpression> {
        match self {
            Self::Ptr(value) => Some(value),
            _ => None,
        }
    }
}
