use std::rc::Rc;

use ast::literal::Type;

use crate::effect::Effect;

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub expr: Rc<ast::expression::Expression>,
    pub effects: Vec<Effect>,
    pub ty: Type,
}
