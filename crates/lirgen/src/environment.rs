use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Default)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    // original name -> (id, is_mut)
    mapping: HashMap<String, (u64, bool)>,
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

    pub fn define(&mut self, name: String, id: u64, is_mut: bool) {
        self.mapping.insert(name, (id, is_mut));
    }

    pub fn get(&self, name: &str) -> Option<(u64, bool)> {
        if let Some(id) = self.mapping.get(name) {
            Some(*id)
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }
}
