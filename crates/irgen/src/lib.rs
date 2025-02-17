pub mod emitters;
pub mod util;

use std::rc::Rc;

use emitters::module::IrModuleEmitter;
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;
use inkwell::{context::Context, module::Module};
use lir::module::LirModule;

pub use inkwell::targets::FileType;

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

pub struct TargetInitializer {
    triple: TargetTriple,
}

impl TargetInitializer {
    pub fn native() -> Self {
        Target::initialize_native(&InitializationConfig::default()).unwrap();

        Self {
            triple: TargetMachine::get_default_triple(),
        }
    }

    pub fn init_target_machine(&self) -> TargetMachine {
        let target = Target::from_triple(&self.triple).unwrap();
        target
            .create_target_machine(
                &self.triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap()
    }
}
