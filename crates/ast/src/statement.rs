use crate::{annotation::Annotation, expression::Expression, literal::Type};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression {
        expr: Expression,
        end_semi: bool,
    },
    VariableDeclaration {
        name: String,
        expr: Expression,
    },
    FunctionDeclaration {
        name: String,
        block: Option<Expression>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    },
    Annotated {
        annotations: Vec<Annotation>,
        stmt: Box<Statement>,
    },
}
