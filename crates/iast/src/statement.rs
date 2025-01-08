use crate::effect::Effect;

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub stmt: ast::statement::Statement,
    pub effects: Vec<Effect>,
}
