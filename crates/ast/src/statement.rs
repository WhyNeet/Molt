use std::rc::Rc;

use crate::{annotation::Annotation, expression::Expression, literal::Type};

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
}
