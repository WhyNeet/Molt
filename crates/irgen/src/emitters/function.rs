use std::rc::Rc;

use common::Type;
use inkwell::{
    basic_block::BasicBlock,
    types::{BasicMetadataTypeEnum, BasicType},
    values::FunctionValue,
};
use lir::block::BasicBlock as LirBasicBlock;

use crate::util;

use super::{module::ModuleEmitterScope, statement::IrStatementEmitter};

pub struct IrFunctionEmitter<'a> {
    scope: Rc<ModuleEmitterScope<'a>>,
}

impl<'a> IrFunctionEmitter<'a> {
    pub fn new(scope: Rc<ModuleEmitterScope<'a>>) -> Self {
        Self { scope }
    }
}

impl<'a> IrFunctionEmitter<'a> {
    pub fn emit(
        &self,
        name: &str,
        parameters: &Vec<(String, Type)>,
        blocks: &Vec<LirBasicBlock>,
        return_type: &Type,
    ) {
        let parameters = parameters
            .iter()
            .map(|(_name, ty)| {
                util::into_primitive_context_type(ty, self.scope.context()).as_basic_type_enum()
            })
            .map(|ty| BasicMetadataTypeEnum::from(ty))
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let function = self.scope.module().add_function(
            name,
            match return_type {
                Type::Bool => self.scope.context().bool_type().fn_type(&parameters, false),
                Type::Unit => self.scope.context().void_type().fn_type(&parameters, false),
                _ => todo!(),
            },
            None,
        );

        for block in blocks {
            self.emit_for_block(block, function);
        }
    }

    fn emit_for_block(
        &self,
        lir_block: &LirBasicBlock,
        function: FunctionValue<'a>,
    ) -> BasicBlock<'a> {
        let block = self
            .scope
            .context()
            .append_basic_block(function, &lir_block.0.to_string());

        let stmt_emitter = IrStatementEmitter::new(Rc::clone(&self.scope));

        for stmt in lir_block.1.borrow().iter() {
            stmt_emitter.emit(Rc::clone(stmt));
        }

        block
    }
}
