use std::rc::Rc;

use crate::expression::Expression;
use ast::{annotation::Annotation, literal::Type};

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
    },
    Annotated {
        annotations: Vec<Annotation>,
        stmt: Rc<Statement>,
    },
    Return(Rc<Expression>),
}
