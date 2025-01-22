use std::rc::Rc;

use emitters::module::LirModuleEmitter;
use lir::module::LirModule;
use tcast::statement::Statement as CheckedStatement;

pub mod emitters;
pub mod var_name_gen;
pub mod variable;

pub struct LirEmitter {
    tcast: Vec<Rc<CheckedStatement>>,
}

impl LirEmitter {
    pub fn new(tcast: Vec<Rc<CheckedStatement>>) -> Self {
        Self { tcast }
    }
}

impl LirEmitter {
    pub fn emit_lir_module(&self) -> LirModule {
        LirModuleEmitter::new().emit(self.tcast.clone())
    }
}
