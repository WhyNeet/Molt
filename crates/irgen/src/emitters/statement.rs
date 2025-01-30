use std::rc::Rc;

use common::Type;
use inkwell::{
    types::{AnyType, BasicType},
    values::BasicValue,
};
use lir::statement::Statement;

use super::{
    expression::IrExpressionEmitter,
    function::{FunctionEmitterScope, VariableData},
    module::ModuleEmitterScope,
};

pub struct IrStatementEmitter<'a> {
    mod_scope: Rc<ModuleEmitterScope<'a>>,
    fn_scope: Rc<FunctionEmitterScope<'a>>,
}

impl<'a> IrStatementEmitter<'a> {
    pub fn new(
        mod_scope: Rc<ModuleEmitterScope<'a>>,
        fn_scope: Rc<FunctionEmitterScope<'a>>,
    ) -> Self {
        Self {
            fn_scope,
            mod_scope,
        }
    }
}

impl<'a> IrStatementEmitter<'a> {
    pub fn emit(&self, statement: Rc<Statement>) {
        let expr_emitter =
            IrExpressionEmitter::new(Rc::clone(&self.mod_scope), Rc::clone(&self.fn_scope));

        match statement.as_ref() {
            Statement::StaticVariableDeclaration { id, expr, ty } => {
                let (ty, value) = expr_emitter.emit(Rc::clone(expr), Some(*id)).unwrap();

                self.fn_scope.define(*id, VariableData::new(ty, value));
            }
            Statement::Return(expr) => {
                let value = expr_emitter
                    .emit_static_expression(expr)
                    .map(|(ty, val)| val);

                self.mod_scope
                    .builder()
                    .build_return(value.as_ref().map(|val| val as &dyn BasicValue))
                    .unwrap();
            }
            Statement::VariableDeclaration { name, expr, ty } => {
                let (ty, value) = expr_emitter.emit(Rc::clone(expr), Some(*name)).unwrap();

                self.fn_scope.define(*name, VariableData::new(ty, value));
            }
            _ => todo!(),
        }
    }
}
