use std::rc::Rc;

use crate::{
    literal::{Literal, Type},
    operator::Operator,
    statement::Statement,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Binary {
        left: Rc<Expression>,
        operator: Operator,
        right: Rc<Expression>,
    },
    Unary {
        operator: Operator,
        expr: Rc<Expression>,
    },
    Grouping(Rc<Expression>),
    Identifier(String),
    Literal(Rc<Literal>),
    Assignment {
        identifier: String,
        expr: Rc<Expression>,
    },
    Block(Vec<Rc<Statement>>),
    Conditional {
        condition: Rc<Expression>,
        body: Vec<Rc<Statement>>,
        alternative: Option<Rc<Expression>>,
    },
    MemberAccess {
        expr: Rc<Expression>,
        ident: String,
    },
    Call {
        expr: Rc<Expression>,
        arguments: Vec<Expression>,
    },
    Cast {
        expr: Rc<Expression>,
        ty: Type,
    },
    Loop(Vec<Rc<Statement>>),
    Continue,
    Break,
}
