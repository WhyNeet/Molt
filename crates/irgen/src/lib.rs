pub mod emitters;
pub mod util;

use std::rc::Rc;

use emitters::module::IrModuleEmitter;
use inkwell::{context::Context, module::Module};
use lir::module::LirModule;

pub struct IrEmitter<'a> {
    module: LirModule,
    emitter: IrModuleEmitter<'a>,
}

impl<'a> IrEmitter<'a> {
    pub fn new(module: LirModule) -> Self {
        Self {
            module,
            emitter: IrModuleEmitter::new(Context::create()),
        }
    }

    pub fn emit_llvm_ir(&'a self) -> Rc<Module<'a>> {
        self.emitter.emit(&self.module)
    }
}
