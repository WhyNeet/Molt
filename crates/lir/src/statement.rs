use std::rc::Rc;

use common::Type;

use crate::{
    block::BasicBlock,
    expression::{Expression, StaticExpression},
};

#[derive(Debug)]
pub enum Statement {
    Expression {
        expr: Rc<Expression>,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Rc<Expression>,
        ty: Type,
    },
    StaticVariableDeclaration {
        id: u64,
        expr: Rc<Expression>,
        ty: Type,
    },
    ExternalFunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    FunctionDeclaration {
        name: String,
        blocks: Vec<BasicBlock>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    Goto(u64),
    Branch {
        condition: Rc<StaticExpression>,
        then: u64,
        alternative: u64,
    },
    Return(Rc<StaticExpression>),
}
