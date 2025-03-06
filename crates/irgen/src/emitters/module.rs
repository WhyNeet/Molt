use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue},
    AddressSpace,
};
use lir::{module::LirModule, statement::Statement};

use crate::util;

use super::{expression::StaticExpressionEmitter, function::IrFunctionEmitter};

pub struct ModuleEmitterScope<'a> {
    context: &'a Context,
    builder: RefCell<Option<Rc<Builder<'a>>>>,
    module: RefCell<Option<Rc<Module<'a>>>>,
    functions: RefCell<HashMap<String, FunctionValue<'a>>>,
    globals: RefCell<HashMap<String, BasicValueEnum<'a>>>,
    structs: RefCell<HashMap<String, BasicTypeEnum<'a>>>,
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

    pub fn get_global(&self, name: &str) -> Option<BasicValueEnum<'a>> {
        self.globals.borrow().get(name).map(|val| *val)
    }

    pub fn get_struct(&self, name: &str) -> Option<BasicTypeEnum<'a>> {
        self.structs.borrow().get(name).map(|val| *val)
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
            globals: RefCell::default(),
            structs: RefCell::default(),
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
                    let static_emitter = StaticExpressionEmitter::new(self.scope(), None);
                    let (_ty, value) = static_emitter.emit_static_expression(expr).unwrap();
                    self.scope
                        .module()
                        .add_global(
                            util::into_primitive_context_type(ty, self.scope().context()).unwrap(),
                            Some(AddressSpace::default()),
                            name,
                        )
                        .set_initializer(&value);

                    self.scope()
                        .globals
                        .borrow_mut()
                        .insert(name.to_string(), value);
                }
                Statement::StructDeclaration {
                    name,
                    fields,
                    methods,
                } => {
                    let struct_ty = self.scope().context().struct_type(
                        fields
                            .iter()
                            .map(|(_, ty)| {
                                util::into_primitive_context_type(ty, &self.scope().context())
                                    .unwrap()
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    );

                    self.scope()
                        .structs
                        .borrow_mut()
                        .insert(name.clone(), struct_ty.as_basic_type_enum());
                }
                _ => unreachable!(),
            }
        }
    }
}
