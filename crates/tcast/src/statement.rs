use std::rc::Rc;

use crate::{expression::Expression, fn_attribute::FunctionAttribute};
use ast::annotation::Annotation;
use common::Type;

use crate::effect::Effect;

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
    },
    FunctionDeclaration {
        name: String,
        block: Option<Expression>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        attributes: Vec<FunctionAttribute>,
    },
    Return(Rc<Expression>),
}
