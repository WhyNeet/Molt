use std::rc::Rc;

use crate::{effect::Effect, expression::Expression, fn_attribute::FunctionAttribute};
use common::Type;

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub stmt: Rc<StatementKind>,
    pub effects: Vec<Effect>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Expression {
        expr: Expression,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Expression,
        ty: Type,
        is_mut: bool,
    },
    FunctionDeclaration {
        name: String,
        block: Option<Rc<Expression>>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        attributes: Vec<FunctionAttribute>,
    },
    Return(Rc<Expression>),
}
