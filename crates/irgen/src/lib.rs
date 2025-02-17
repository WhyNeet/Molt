pub mod emitters;
pub mod util;

use std::rc::Rc;

use emitters::module::IrModuleEmitter;
use inkwell::{context::Context, module::Module};
use lir::module::LirModule;

pub struct IrEmitter {
    context: Context,
}

impl IrEmitter {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn run<'ctx>(&'ctx self, module: LirModule) -> Rc<Module<'ctx>> {
        let emitter = IrModuleEmitter::new(&self.context);

        emitter.emit(&module);
        emitter.scope.module()
    }
}
