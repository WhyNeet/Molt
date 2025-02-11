use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use lir::{module::LirModule, statement::Statement};
use tcast::{
    fn_attribute::FunctionAttribute,
    statement::{Statement as CheckedStatement, StatementKind},
};

use super::function::LirFunctionEmitter;

#[derive(Debug, Default)]
pub struct LirModuleEmitterScope {
    mapping: RefCell<HashMap<String, u64>>,
    exact: RefCell<HashSet<String>>,
}

impl LirModuleEmitterScope {
    pub fn define(&self, name: String) -> u64 {
        let mut mapping = self.mapping.borrow_mut();
        let id = mapping.len() as u64;
        mapping.insert(name, id);

        id
    }

    pub fn define_exact(&self, name: String) {
        self.exact.borrow_mut().insert(name);
    }

    pub fn get(&self, name: &str) -> Option<String> {
        if let Some(name) = self.exact.borrow().get(name) {
            Some(name.to_string())
        } else if let Some(id) = self.mapping.borrow().get(name).map(|n| *n) {
            Some(id.to_string())
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct LirModuleEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
    entrypoint: RefCell<Option<u64>>,
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
                    let is_main = attributes.contains(&FunctionAttribute::Main);

                    let func = LirFunctionEmitter::new(Rc::clone(&self.scope)).emit(
                        if is_main {
                            "main".to_string()
                        } else {
                            name.clone()
                        },
                        block.clone(),
                        return_type.clone(),
                        parameters.clone(),
                        is_main,
                    );

                    if attributes.contains(&FunctionAttribute::Main) {
                        // let mut entrypoint = self.entrypoint.borrow_mut();
                        // if entrypoint.is_some() {
                        //     panic!("a module cannot have multiple entrypoints.")
                        // }

                        // *entrypoint = Some(match &func {
                        //     Statement::FunctionDeclaration { name, .. } => name.to_st,
                        //     Statement::ExternalFunctionDeclaration { name, .. } => {
                        //         panic!("external function `{name}` cannot be an entrypoint.")
                        //     }
                        //     _ => unreachable!(),
                        // });
                    }

                    func
                }
                _ => todo!(),
            };

            self.stmts.borrow_mut().push(Rc::new(stmt));
        }

        LirModule::new(self.stmts.take())
    }
}
