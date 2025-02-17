use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::Type;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    module::Linkage,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue},
    AddressSpace,
};
use lir::block::BasicBlock as LirBasicBlock;

use crate::util;

use super::{module::ModuleEmitterScope, statement::IrStatementEmitter};

#[derive(Debug, Clone, PartialEq)]
pub struct VariableData<'a> {
    pub(crate) ty: BasicTypeEnum<'a>,
    pub(crate) value: BasicValueEnum<'a>,
}

impl<'a> VariableData<'a> {
    pub fn new(ty: BasicTypeEnum<'a>, value: BasicValueEnum<'a>) -> Self {
        Self { ty, value }
    }
}

pub struct FunctionEmitterScope<'a> {
    ssa: RefCell<HashMap<u64, VariableData<'a>>>,
}

impl<'a> FunctionEmitterScope<'a> {
    pub fn new() -> Self {
        Self {
            ssa: RefCell::default(),
        }
    }

    pub fn define(&self, id: u64, data: VariableData<'a>) {
        self.ssa.borrow_mut().insert(id, data);
    }

    pub fn get(&self, id: &u64) -> Option<VariableData<'a>> {
        self.ssa.borrow().get(id).map(|val| val.clone())
    }
}

pub struct IrFunctionEmitter<'a> {
    mod_scope: Rc<ModuleEmitterScope<'a>>,
    scope: Rc<FunctionEmitterScope<'a>>,
}

impl<'a> IrFunctionEmitter<'a> {
    pub fn new(scope: Rc<ModuleEmitterScope<'a>>) -> Self {
        Self {
            mod_scope: scope,
            scope: Rc::new(FunctionEmitterScope::new()),
        }
    }
}

impl<'a> IrFunctionEmitter<'a> {
    pub fn emit_external(
        &self,
        name: String,
        parameters: Vec<&Type>,
        return_type: &Type,
        is_var_args: bool,
    ) {
        let cx = self.mod_scope.context();

        let parameters = parameters
            .iter()
            .map(|ty| util::into_primitive_context_type(ty, cx).unwrap())
            .collect::<Vec<_>>();

        let fn_parameters = parameters
            .iter()
            .map(|ty| BasicMetadataTypeEnum::from(*ty))
            .collect::<Vec<_>>();

        let function = self.mod_scope.module().add_function(
            &name,
            match return_type {
                Type::Bool => self
                    .mod_scope
                    .context()
                    .bool_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::Unit => self
                    .mod_scope
                    .context()
                    .void_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::Char => self
                    .mod_scope
                    .context()
                    .i8_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::Str => self
                    .mod_scope
                    .context()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&fn_parameters, is_var_args),
                Type::Float32 => self
                    .mod_scope
                    .context()
                    .f32_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::Float64 => self
                    .mod_scope
                    .context()
                    .f64_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::UInt8 | Type::Int8 => self
                    .mod_scope
                    .context()
                    .i8_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::UInt16 | Type::Int16 => self
                    .mod_scope
                    .context()
                    .i16_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::UInt32 | Type::Int32 => self
                    .mod_scope
                    .context()
                    .i32_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::UInt64 | Type::Int64 => self
                    .mod_scope
                    .context()
                    .i64_type()
                    .fn_type(&fn_parameters, is_var_args),
                Type::Ptr(other) => self
                    .mod_scope
                    .context()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&fn_parameters, is_var_args),
                Type::Callable {
                    parameters,
                    return_type,
                    var_args,
                } => todo!(),
                Type::NoReturn => unreachable!(),
            },
            None,
        );

        function.add_attribute(
            AttributeLoc::Function,
            cx.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
        );

        self.mod_scope.define_function(name, function);
    }

    pub fn emit(
        &self,
        name: String,
        parameters: &Vec<(u64, Type)>,
        blocks: &Vec<LirBasicBlock>,
        return_type: &Type,
    ) {
        let parameters = parameters
            .iter()
            .map(|(_id, ty)| {
                util::into_primitive_context_type(ty, self.mod_scope.context()).unwrap()
            })
            .collect::<Vec<_>>();

        let fn_parameters = parameters
            .iter()
            .map(|ty| BasicMetadataTypeEnum::from(*ty))
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let function = self.mod_scope.module().add_function(
            &name,
            match return_type {
                Type::Bool => self
                    .mod_scope
                    .context()
                    .bool_type()
                    .fn_type(&fn_parameters, false),
                Type::Unit => self
                    .mod_scope
                    .context()
                    .void_type()
                    .fn_type(&fn_parameters, false),
                Type::Char => self
                    .mod_scope
                    .context()
                    .i8_type()
                    .fn_type(&fn_parameters, false),
                Type::Str => self
                    .mod_scope
                    .context()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&fn_parameters, false),
                Type::Float32 => self
                    .mod_scope
                    .context()
                    .f32_type()
                    .fn_type(&fn_parameters, false),
                Type::Float64 => self
                    .mod_scope
                    .context()
                    .f64_type()
                    .fn_type(&fn_parameters, false),
                Type::UInt8 | Type::Int8 => self
                    .mod_scope
                    .context()
                    .i8_type()
                    .fn_type(&fn_parameters, false),
                Type::UInt16 | Type::Int16 => self
                    .mod_scope
                    .context()
                    .i16_type()
                    .fn_type(&fn_parameters, false),
                Type::UInt32 | Type::Int32 => self
                    .mod_scope
                    .context()
                    .i32_type()
                    .fn_type(&fn_parameters, false),
                Type::UInt64 | Type::Int64 => self
                    .mod_scope
                    .context()
                    .i64_type()
                    .fn_type(&fn_parameters, false),
                Type::Ptr(other) => self
                    .mod_scope
                    .context()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&fn_parameters, false),
                Type::Callable {
                    parameters,
                    return_type,
                    var_args,
                } => todo!(),
                Type::NoReturn => unreachable!(),
            },
            None,
        );

        self.mod_scope.define_function(name, function);

        for (id, ty) in parameters.iter().enumerate() {
            self.scope.ssa.borrow_mut().insert(
                id as u64,
                VariableData {
                    ty: *ty,
                    value: function.get_params()[id],
                },
            );
        }

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
            .mod_scope
            .context()
            .append_basic_block(function, &lir_block.0.to_string());

        self.mod_scope.builder().position_at_end(block);

        let stmt_emitter =
            IrStatementEmitter::new(Rc::clone(&self.mod_scope), Rc::clone(&self.scope));

        for stmt in lir_block.1.borrow().iter() {
            stmt_emitter.emit(Rc::clone(stmt));
        }

        block
    }
}
