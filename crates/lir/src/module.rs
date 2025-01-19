use std::rc::Rc;

use crate::statement::Statement;

#[derive(Debug)]
pub struct LirModule {
    stmts: Vec<Rc<Statement>>,
    entrypoint: Option<String>,
}

impl LirModule {
    pub fn new(stmts: Vec<Rc<Statement>>, entrypoint: Option<String>) -> LirModule {
        LirModule { stmts, entrypoint }
    }

    pub fn stmts(&self) -> Vec<Rc<Statement>> {
        self.stmts.clone()
    }

    pub fn entrypoint(&self) -> Option<&str> {
        self.entrypoint.as_ref().map(|s| s.as_str())
    }
}
