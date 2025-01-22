use std::rc::Rc;

use common::Type;

use crate::expression::{Expression, StaticExpression};

#[derive(Debug)]
pub enum Statement {
    Expression {
        expr: Rc<Expression>,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Rc<Expression>,
        allocation: VariableAllocationKind,
        ty: Type,
    },
    ExternalFunctionDeclaration {
        name: String,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    FunctionDeclaration {
        name: String,
        block: Vec<Rc<Statement>>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    Return(Rc<StaticExpression>),
}

#[derive(Debug)]
pub enum VariableAllocationKind {
    SSA,
    Stack,
}
