use std::rc::Rc;

use common::Type;
use inkwell::values::BasicValue;
use lir::statement::Statement;

use super::{
    expression::{IrExpressionEmitter, StaticExpressionEmitter},
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
        let static_emitter = StaticExpressionEmitter::new(
            Rc::clone(&self.mod_scope),
            Some(Rc::clone(&self.fn_scope)),
        );

        match statement.as_ref() {
            Statement::StaticVariableDeclaration { id, expr, ty } => {
                if *ty == Type::Unit {
                    // do not unwrap
                    expr_emitter.emit(Rc::clone(expr), Some(*id));
                    return;
                }

                let (ty, value) = expr_emitter.emit(Rc::clone(expr), Some(*id)).unwrap();

                self.fn_scope
                    .define(id.to_string(), VariableData::new(ty, value, false));
            }
            Statement::Return(expr) => {
                let value = static_emitter
                    .emit_static_expression(expr)
                    .map(|(ty, val)| val);

                self.mod_scope
                    .builder()
                    .build_return(value.as_ref().map(|val| val as &dyn BasicValue))
                    .unwrap();
            }
            Statement::VariableDeclaration {
                name,
                expr,
                is_mut,
                ty: _,
            } => {
                let (ty, value) = expr_emitter.emit(Rc::clone(expr), Some(*name)).unwrap();

                let value = if *is_mut {
                    let ptr = self
                        .mod_scope
                        .builder()
                        .build_alloca(ty, &name.to_string())
                        .unwrap();
                    self.mod_scope.builder().build_store(ptr, value).unwrap();
                    ptr.as_basic_value_enum()
                } else {
                    value
                };

                self.fn_scope
                    .define(name.to_string(), VariableData::new(ty, value, *is_mut));
            }
            Statement::Branch {
                condition,
                then,
                alternative,
            } => {
                let (_ty, value) = static_emitter.emit_static_expression(condition).unwrap();
                self.mod_scope
                    .builder()
                    .build_conditional_branch(
                        value.into_int_value(),
                        self.fn_scope.get_block(*then).unwrap(),
                        self.fn_scope.get_block(*alternative).unwrap(),
                    )
                    .unwrap();
            }
            Statement::Goto(id) => {
                self.mod_scope
                    .builder()
                    .build_unconditional_branch(self.fn_scope.get_block(*id).unwrap())
                    .unwrap();
            }
            Statement::Store { id, value } => {
                let assignee_ptr = self
                    .fn_scope
                    .get(&id.to_string())
                    .unwrap()
                    .value
                    .into_pointer_value();

                self.mod_scope
                    .builder()
                    .build_store(
                        assignee_ptr,
                        static_emitter.emit_static_expression(value).unwrap().1,
                    )
                    .unwrap();
            }
            Statement::FunctionDeclaration { .. }
            | Statement::GlobalVariableDeclaration { .. }
            | Statement::ExternalFunctionDeclaration { .. } => unreachable!(),
        }
    }
}
