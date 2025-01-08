use ast::literal::Type;

use crate::effect::Effect;

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub expr: ast::expression::Expression,
    pub effects: Vec<Effect>,
    pub ty: Type,
}
