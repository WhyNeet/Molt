use std::{borrow::Borrow, cell::RefCell, ptr, rc::Rc};

use common::Type;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyType, BasicMetadataTypeEnum, BasicType},
    values::FunctionValue,
};
use lir::{block::BasicBlock as LirBasicBlock, module::LirModule, statement::Statement};

use crate::util;

pub struct IrModuleEmitter<'a> {
    context: Context,
    builder: RefCell<Option<Rc<Builder<'a>>>>,
    module: RefCell<Option<Rc<Module<'a>>>>,
    block_id: RefCell<u64>,
}

impl<'a> IrModuleEmitter<'a> {
    pub fn new(context: Context) -> Self {
        Self {
            context,
            builder: RefCell::default(),
            module: RefCell::default(),
            block_id: RefCell::default(),
        }
    }

    fn builder(&self) -> Rc<Builder> {
        Rc::clone(self.builder.borrow().as_ref().unwrap())
    }

    fn module(&self) -> Rc<Module<'a>> {
        Rc::clone(self.module.borrow().as_ref().unwrap())
    }
}

impl<'a> IrModuleEmitter<'a> {
    pub fn emit(&'a self, lir: &LirModule) -> Rc<Module<'a>> {
        *self.builder.borrow_mut() = Some(Rc::new(self.context.create_builder()));
        *self.module.borrow_mut() = Some(Rc::new(self.context.create_module("main")));

        for stmt in lir.stmts() {
            match stmt.as_ref() {
                Statement::FunctionDeclaration {
                    name,
                    blocks,
                    return_type,
                    parameters,
                } => {
                    self.block_id.replace(0);

                    let function = self.module().add_function(
                        name,
                        match return_type {
                            Type::Bool => self.context.bool_type().fn_type(
                                parameters
                                    .iter()
                                    .map(|(_name, ty)| {
                                        util::into_primitive_context_type(ty, &self.context)
                                            .as_basic_type_enum()
                                    })
                                    .map(|ty| BasicMetadataTypeEnum::from(ty))
                                    .collect::<Vec<BasicMetadataTypeEnum>>()
                                    .as_slice(),
                                false,
                            ),
                            Type::Unit => self.context.void_type().fn_type(
                                parameters
                                    .iter()
                                    .map(|(_name, ty)| {
                                        util::into_primitive_context_type(ty, &self.context)
                                            .as_basic_type_enum()
                                    })
                                    .map(|ty| BasicMetadataTypeEnum::from(ty))
                                    .collect::<Vec<BasicMetadataTypeEnum>>()
                                    .as_slice(),
                                false,
                            ),
                            _ => todo!(),
                        },
                        None,
                    );

                    for block in blocks {
                        self.emit_for_block(block, function);
                    }
                }
                _ => todo!(),
            }
        }

        self.module()
    }

    fn emit_for_block(
        &'a self,
        block: &LirBasicBlock,
        function: FunctionValue<'a>,
    ) -> BasicBlock<'a> {
        let block = self
            .context
            .append_basic_block(function, &self.block_id.borrow().to_string());

        block
    }
}
