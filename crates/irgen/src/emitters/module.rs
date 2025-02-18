use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder, context::Context, module::Module, values::FunctionValue, AddressSpace,
};
use lir::{module::LirModule, statement::Statement};

use crate::util;

use super::function::IrFunctionEmitter;

pub struct ModuleEmitterScope<'a> {
    context: &'a Context,
    builder: RefCell<Option<Rc<Builder<'a>>>>,
    module: RefCell<Option<Rc<Module<'a>>>>,
    functions: RefCell<HashMap<String, FunctionValue<'a>>>,
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

    pub fn define_function(&self, name: String, function: FunctionValue<'a>) {
        self.functions.borrow_mut().insert(name, function);
    }

    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'a>> {
        self.functions.borrow().get(name).map(|f| *f)
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
            functions: RefCell::default(),
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
                } => fn_emitter.emit(name.to_string(), parameters, blocks, return_type),
                Statement::ExternalFunctionDeclaration {
                    name,
                    return_type,
                    parameters,
                    is_var_args,
                } => fn_emitter.emit_external(
                    name.to_string(),
                    parameters.iter().map(|(_, ty)| ty).collect(),
                    return_type,
                    *is_var_args,
                ),
                Statement::GlobalVariableDeclaration { name, expr, ty } => {
                    self.scope.module().add_global(
                        util::into_primitive_context_type(ty, self.scope().context()).unwrap(),
                        Some(AddressSpace::default()),
                        name,
                    );
                }
                _ => unreachable!(),
            }
        }
    }
}
