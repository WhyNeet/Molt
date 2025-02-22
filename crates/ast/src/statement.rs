use std::rc::Rc;

use common::Type;

use crate::{annotation::Annotation, expression::Expression};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Expression {
        expr: Rc<Expression>,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Rc<Expression>,
        /// If type is not specified, infer it.
        ty: Option<Type>,
        is_mut: bool,
    },
    FunctionDeclaration {
        name: String,
        block: Option<Rc<Expression>>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    Annotated {
        annotations: Vec<Annotation>,
        stmt: Rc<Statement>,
    },
    Return(Rc<Expression>),
}
