use std::rc::Rc;

use crate::statement::Statement;

#[derive(Debug)]
pub struct LirModule {
    stmts: Vec<Rc<Statement>>,
}

impl LirModule {
    pub fn new(stmts: Vec<Rc<Statement>>) -> LirModule {
        LirModule { stmts }
    }

    pub fn stmts(&self) -> Vec<Rc<Statement>> {
        self.stmts.clone()
    }
}
