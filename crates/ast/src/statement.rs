use crate::expression::Expression;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration {
        name: String,
        expr: Expression,
        annotations: Vec<String>,
    },
}
