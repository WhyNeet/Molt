use std::rc::Rc;

use common::{Literal, Operator, Type};

use crate::statement::Statement;

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
    Ptr(Rc<Expression>),
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
        arguments: Vec<Rc<Expression>>,
    },
    Cast {
        expr: Rc<Expression>,
        ty: Type,
    },
    Loop(Vec<Rc<Statement>>),
    Continue,
    Break,
}
