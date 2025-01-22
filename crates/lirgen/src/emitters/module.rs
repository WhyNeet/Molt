use std::{cell::RefCell, rc::Rc};

use lir::{module::LirModule, statement::Statement};
use tcast::{
    fn_attribute::FunctionAttribute,
    statement::{Statement as CheckedStatement, StatementKind},
};

use super::function::LirFunctionEmitter;

pub struct LirModuleEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
    entrypoint: RefCell<Option<String>>,
}

impl LirModuleEmitter {
    pub fn new() -> Self {
        Self {
            stmts: RefCell::default(),
            entrypoint: RefCell::default(),
        }
    }
}

impl LirModuleEmitter {
    pub fn emit(&self, tcast: Vec<Rc<CheckedStatement>>) -> LirModule {
        for stmt in tcast.iter() {
            let stmt = match stmt.stmt.as_ref() {
                StatementKind::FunctionDeclaration {
                    attributes,
                    block,
                    name,
                    parameters,
                    return_type,
                } => {
                    if attributes.contains(&FunctionAttribute::Main) {
                        let mut entrypoint = self.entrypoint.borrow_mut();
                        if entrypoint.is_some() {
                            panic!("a module cannot have multiple entrypoints.")
                        }

                        *entrypoint = Some(name.clone());
                    }

                    LirFunctionEmitter::new().emit(
                        name.clone(),
                        block.clone(),
                        return_type.clone(),
                        parameters.clone(),
                    )
                }
                _ => todo!(),
            };

            self.stmts.borrow_mut().push(Rc::new(stmt));
        }

        LirModule::new(self.stmts.take(), self.entrypoint.take())
    }
}
