use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::Type;
use tcast::effect::Effect;

#[derive(Debug, Clone)]
pub struct Declaration {
    pub ty: Type,
    pub effects: Vec<Effect>,
    pub is_mut: bool,
    pub num_mutations: u64,
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

    pub fn add_enclosing(&mut self, enclosing: Rc<Environment>) {
        self.enclosing = Some(enclosing);
    }

    pub fn enclosing(&self) -> Option<Rc<Environment>> {
        self.enclosing.as_ref().map(Rc::clone)
    }

    pub fn with_enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            ..Default::default()
        }
    }

    /// Returns `true` if the value was shadowed.
    pub fn declare(&self, name: String, ty: Type, effects: Vec<Effect>, is_mut: bool) -> bool {
        self.declarations
            .borrow_mut()
            .insert(
                name,
                Declaration {
                    effects,
                    ty,
                    is_mut,
                    num_mutations: 0,
                },
            )
            .is_some()
    }

    pub fn assigned(&self, name: String) {
        self.declarations
            .borrow_mut()
            .entry(name)
            .and_modify(|decl| decl.num_mutations += 1);
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

    pub fn get_all(&self) -> &RefCell<HashMap<String, Declaration>> {
        &self.declarations
    }
}
