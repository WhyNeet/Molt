use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::literal::Type;
use iast::effect::Effect;

#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Type,
    pub effects: Vec<Effect>,
}

#[derive(Default)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    declarations: RefCell<HashMap<String, Declaration>>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            ..Default::default()
        }
    }

    /// Returns `true` if the value was shadowed.
    pub fn declare(&self, name: String, ty: Type, effects: Vec<Effect>) -> bool {
        self.declarations
            .borrow_mut()
            .insert(name, Declaration { effects, ty })
            .is_some()
    }

    pub fn get(&self, name: &str) -> Option<Declaration> {
        if let Some(declaration) = self.declarations.borrow().get(name) {
            Some(declaration.clone())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
