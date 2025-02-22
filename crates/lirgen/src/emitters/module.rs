use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use lir::{expression::StaticExpression, module::LirModule, statement::Statement};
use tcast::{
    expression::ExpressionKind,
    fn_attribute::FunctionAttribute,
    statement::{Statement as CheckedStatement, StatementKind},
};

use crate::environment::Environment;

use super::function::LirFunctionEmitter;

#[derive(Debug, Default)]
pub struct LirModuleEmitterScope {
    mapping: RefCell<HashMap<String, u64>>,
    exact: RefCell<HashSet<String>>,
    environment: RefCell<Environment>,
    globals: RefCell<HashSet<String>>,
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

    pub fn define_global(&self, name: String) {
        self.globals.borrow_mut().insert(name);
    }

    pub fn has_global(&self, name: &str) -> bool {
        self.globals.borrow().contains(name)
    }
}

#[derive(Default)]
pub struct LirModuleEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
    scope: Rc<LirModuleEmitterScope>,
}

impl LirModuleEmitter {
    pub fn new() -> Self {
        Self {
            stmts: RefCell::default(),
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
                        attributes.contains(&FunctionAttribute::VarArg),
                    );

                    func
                }
                StatementKind::VariableDeclaration { name, expr, ty, .. } => {
                    let expr = match expr.expr.as_ref() {
                        ExpressionKind::Literal(lit) => StaticExpression::Literal(Rc::clone(lit)),
                        other => panic!("`{other:?}` global expressions are not supported."),
                    };

                    self.scope.define_global(name.to_string());

                    let stmt = Statement::GlobalVariableDeclaration {
                        name: name.to_string(),
                        expr: Rc::new(expr),
                        ty: ty.clone(),
                    };

                    stmt
                }
                StatementKind::Expression { .. } => continue,
                StatementKind::Return(_) => unreachable!(),
            };

            self.stmts.borrow_mut().push(Rc::new(stmt));
        }

        LirModule::new(self.stmts.take())
    }
}
