use std::rc::Rc;

use common::{Literal, Number};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum},
};
use lir::expression::{Expression, StaticExpression};

use super::{function::FunctionEmitterScope, module::ModuleEmitterScope};

pub struct IrExpressionEmitter<'a> {
    mod_scope: Rc<ModuleEmitterScope<'a>>,
    fn_scope: Rc<FunctionEmitterScope<'a>>,
}

impl<'a> IrExpressionEmitter<'a> {
    pub fn new(
        mod_scope: Rc<ModuleEmitterScope<'a>>,
        fn_scope: Rc<FunctionEmitterScope<'a>>,
    ) -> Self {
        Self {
            mod_scope,
            fn_scope,
        }
    }
}

impl<'a> IrExpressionEmitter<'a> {
    pub fn emit(
        &self,
        expression: Rc<Expression>,
    ) -> Option<(BasicTypeEnum<'a>, BasicValueEnum<'a>)> {
        match expression.as_ref() {
            Expression::Static(expr) => self.emit_static_expression(expr),
            _ => todo!(),
        }
    }

    pub fn emit_static_expression(
        &self,
        expression: &StaticExpression,
    ) -> Option<(BasicTypeEnum<'a>, BasicValueEnum<'a>)> {
        match expression {
            StaticExpression::Identifier(id) => {
                let data = self.fn_scope.get(id).unwrap();

                Some((data.ty, data.value))
            }
            StaticExpression::Literal(literal) => {
                let cx = self.mod_scope.context();
                match literal.as_ref() {
                    Literal::Bool(value) => Some((
                        cx.bool_type().as_basic_type_enum(),
                        cx.bool_type()
                            .const_int(*value as u64, false)
                            .as_basic_value_enum(),
                    )),
                    Literal::Char(value) => Some((
                        cx.i8_type().as_basic_type_enum(),
                        cx.i8_type()
                            .const_int(*value as u64, false)
                            .as_basic_value_enum(),
                    )),
                    Literal::Str(value) => Some((
                        cx.i8_type()
                            .array_type(value.len() as u32)
                            .as_basic_type_enum(),
                        cx.const_string(value.as_bytes(), true)
                            .as_basic_value_enum(),
                    )),
                    Literal::Number(value) => Some(match value {
                        Number::Float32(n) => (
                            cx.f32_type().as_basic_type_enum(),
                            cx.f32_type().const_float(*n as f64).as_basic_value_enum(),
                        ),
                        Number::Float64(n) => (
                            cx.f64_type().as_basic_type_enum(),
                            cx.f64_type().const_float(*n).as_basic_value_enum(),
                        ),
                        Number::UInt8(n) => (
                            cx.i8_type().as_basic_type_enum(),
                            cx.i8_type()
                                .const_int(*n as u64, false)
                                .as_basic_value_enum(),
                        ),
                        Number::Int8(n) => (
                            cx.i8_type().as_basic_type_enum(),
                            cx.i8_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                        ),
                        Number::UInt16(n) => (
                            cx.i16_type().as_basic_type_enum(),
                            cx.i16_type()
                                .const_int(*n as u64, false)
                                .as_basic_value_enum(),
                        ),
                        Number::Int16(n) => (
                            cx.i16_type().as_basic_type_enum(),
                            cx.i16_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                        ),
                        Number::UInt32(n) => (
                            cx.i32_type().as_basic_type_enum(),
                            cx.i32_type()
                                .const_int(*n as u64, false)
                                .as_basic_value_enum(),
                        ),
                        Number::Int32(n) => (
                            cx.i32_type().as_basic_type_enum(),
                            cx.i32_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                        ),
                        Number::UInt64(n) => (
                            cx.i64_type().as_basic_type_enum(),
                            cx.i64_type()
                                .const_int(*n as u64, false)
                                .as_basic_value_enum(),
                        ),
                        Number::Int64(n) => (
                            cx.i64_type().as_basic_type_enum(),
                            cx.i64_type()
                                .const_int(*n as u64, true)
                                .as_basic_value_enum(),
                        ),
                    }),
                    Literal::Unit => None,
                }
            }
            StaticExpression::FnIdentifier(ident) => {
                panic!("call `{ident}`: function calling is not yet implemented")
            }
        }
    }
}
