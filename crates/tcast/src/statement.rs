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
    StructDeclaration {
        name: String,
        /// Field name, field type, optional default initializer
        fields: Vec<(String, Type, Option<Expression>)>,
        methods: Vec<(String, MethodDeclaration)>,
        ty: Type,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct MethodDeclaration {
    pub parameters: Vec<(String, Type)>,
    pub expression: Rc<Expression>,
    pub return_type: Type,
    /// `None` - no self param, `Some(false)` - `self`, `Some(true)` - `mut self`
    pub self_param: Option<bool>,
}
