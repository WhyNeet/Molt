use std::rc::Rc;

use lir::expression::Expression;

use super::module::ModuleEmitterScope;

pub struct IrExpressionEmitter<'a> {
    scope: &'a ModuleEmitterScope<'a>,
}

impl<'a> IrExpressionEmitter<'a> {
    pub fn new(scope: &'a ModuleEmitterScope<'a>) -> Self {
        Self { scope }
    }
}

impl<'a> IrExpressionEmitter<'a> {
    pub fn emit(&self, expression: Rc<Expression>) {
        match expression.as_ref() {
            _ => todo!(),
        }
    }
}
