use crate::{expression::Expression, literal::Type};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression {
        expr: Expression,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Expression,
        annotations: Vec<String>,
    },
    FunctionDeclaration {
        name: String,
        block: Option<Expression>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
        annotations: Vec<String>,
    },
}
