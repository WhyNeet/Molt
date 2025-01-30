use std::{cell::RefCell, rc::Rc};

use inkwell::{builder::Builder, context::Context, module::Module};
use lir::{module::LirModule, statement::Statement};

use super::function::IrFunctionEmitter;

pub struct ModuleEmitterScope<'a> {
    context: &'a Context,
    builder: RefCell<Option<Rc<Builder<'a>>>>,
    module: RefCell<Option<Rc<Module<'a>>>>,
}

impl<'a> ModuleEmitterScope<'a> {
    pub fn context(&self) -> &'a Context {
        &self.context
    }

    pub fn builder(&self) -> Rc<Builder<'a>> {
        Rc::clone(self.builder.borrow().as_ref().unwrap())
    }

    pub fn module(&self) -> Rc<Module<'a>> {
        Rc::clone(self.module.borrow().as_ref().unwrap())
    }

    fn init(&self) {
        *self.builder.borrow_mut() = Some(Rc::new(self.context.create_builder()));
        *self.module.borrow_mut() = Some(Rc::new(self.context.create_module("main")));
    }
}

pub struct IrModuleEmitter<'a> {
    pub(crate) scope: Rc<ModuleEmitterScope<'a>>,
}

impl<'a> IrModuleEmitter<'a> {
    pub fn new(context: &'a Context) -> Self {
        let scope = ModuleEmitterScope {
            context,
            builder: RefCell::default(),
            module: RefCell::default(),
        };

        Self {
            scope: Rc::new(scope),
        }
    }

    pub fn scope(&self) -> Rc<ModuleEmitterScope<'a>> {
        Rc::clone(&self.scope)
    }
}

impl<'a> IrModuleEmitter<'a> {
    pub fn emit(&self, lir: &LirModule) {
        self.scope.init();

        let fn_emitter = IrFunctionEmitter::new(Rc::clone(&self.scope));

        for stmt in lir.stmts() {
            match stmt.as_ref() {
                Statement::FunctionDeclaration {
                    name,
                    blocks,
                    return_type,
                    parameters,
                } => fn_emitter.emit(name, parameters, blocks, return_type),
                _ => todo!(),
            }
        }
    }
}
