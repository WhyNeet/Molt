use std::{cell::RefCell, collections::HashMap, rc::Rc};

use lir::{module::LirModule, statement::Statement};
use tcast::{
    fn_attribute::FunctionAttribute,
    statement::{Statement as CheckedStatement, StatementKind},
};

use super::function::LirFunctionEmitter;

#[derive(Debug, Default)]
pub struct LirModuleEmitterScope {
    mapping: RefCell<HashMap<String, u64>>,
}

impl LirModuleEmitterScope {
    pub fn define(&self, name: String) -> u64 {
        let mut mapping = self.mapping.borrow_mut();
        let id = mapping.len() as u64;
        mapping.insert(name, id);

        id
    }

    pub fn get(&self, name: &str) -> Option<u64> {
        self.mapping.borrow().get(name).map(|n| *n)
    }
}

#[derive(Default)]
pub struct LirModuleEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
    entrypoint: RefCell<Option<String>>,
    scope: Rc<LirModuleEmitterScope>,
}

impl LirModuleEmitter {
    pub fn new() -> Self {
        Self {
            stmts: RefCell::default(),
            entrypoint: RefCell::default(),
            ..Default::default()
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

                    LirFunctionEmitter::new(Rc::clone(&self.scope)).emit(
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
