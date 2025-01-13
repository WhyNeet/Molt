use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::literal::Type;

#[derive(Default)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    declarations: RefCell<HashMap<String, Type>>,
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
    pub fn declare(&self, name: String, ty: Type) -> bool {
        self.declarations.borrow_mut().insert(name, ty).is_some()
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        if let Some(ty) = self.declarations.borrow().get(name) {
            Some(*ty)
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
