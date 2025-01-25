use std::{cell::RefCell, rc::Rc};

use crate::statement::Statement;

#[derive(Debug, Clone, Default)]
pub struct BasicBlock(pub u64, pub RefCell<Vec<Rc<Statement>>>);

impl BasicBlock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn append(&self, stmts: &mut Vec<Rc<Statement>>) {
        self.1.borrow_mut().append(stmts);
    }

    pub fn push(&self, stmt: Rc<Statement>) {
        self.1.borrow_mut().push(stmt);
    }
}
