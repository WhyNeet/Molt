use std::rc::Rc;

use common::Type;

use crate::{
    block::BasicBlock,
    expression::{Expression, StaticExpression},
};

#[derive(Debug)]
pub enum Statement {
    GlobalVariableDeclaration {
        name: String,
        expr: Rc<StaticExpression>,
        ty: Type,
    },
    VariableDeclaration {
        name: u64,
        expr: Rc<Expression>,
        ty: Type,
        is_mut: bool,
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
        is_var_args: bool,
    },
    FunctionDeclaration {
        name: String,
        blocks: Vec<BasicBlock>,
        return_type: Type,
        parameters: Vec<(u64, Type)>,
    },
    Goto(u64),
    Branch {
        condition: Rc<StaticExpression>,
        then: u64,
        alternative: u64,
    },
    Store {
        id: String,
        value: Rc<StaticExpression>,
    },
    Return(Rc<StaticExpression>),
}
