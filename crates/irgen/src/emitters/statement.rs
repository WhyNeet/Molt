use std::rc::Rc;

use lir::statement::Statement;

use super::module::ModuleEmitterScope;

pub struct IrStatementEmitter<'a> {
    scope: Rc<ModuleEmitterScope<'a>>,
}

impl<'a> IrStatementEmitter<'a> {
    pub fn new(scope: Rc<ModuleEmitterScope<'a>>) -> Self {
        Self { scope }
    }
}

impl<'a> IrStatementEmitter<'a> {
    pub fn emit(&self, statement: Rc<Statement>) {
        match statement.as_ref() {
            Statement::Return(expr) => {}
            _ => todo!(),
        }
    }
}
