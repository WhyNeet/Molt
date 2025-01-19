use std::rc::Rc;

use common::Type;

use crate::expression::Expression;

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
    FunctionDeclaration {
        name: String,
        block: Option<Rc<Expression>>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    Return(Rc<Expression>),
}
