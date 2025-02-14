use std::rc::Rc;

use common::{Literal, Number, Type};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace,
};
use lir::{
    expression::{Expression, StaticExpression},
    operator::{BinaryOperator, UnaryOperator},
};

use crate::util;

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
        store_in: Option<u64>,
    ) -> Option<(BasicTypeEnum<'a>, BasicValueEnum<'a>)> {
        match expression.as_ref() {
            Expression::Static(expr, ty) => self.emit_static_expression(expr),
            Expression::Binary {
                left,
                operator,
                right,
                ty,
            } => {
                let left_expr = self.emit_static_expression(left).unwrap();
                let right_expr = self.emit_static_expression(right).unwrap();

                if !ty.is_numeric() {
                    todo!("non-numeric binary operations are not yet implemented")
                }

                let is_int = match ty {
                    Type::Float32 | Type::Float64 => false,
                    _ => true,
                };

                let res = if is_int {
                    let lhs = left_expr.1.into_int_value();
                    let rhs = right_expr.1.into_int_value();
                    let is_signed = match ty {
                        Type::UInt8 | Type::UInt16 | Type::UInt32 | Type::UInt64 => true,
                        _ => false,
                    };
                    let name = &store_in.unwrap().to_string();
                    let builder = self.mod_scope.builder();

                    match operator {
                        BinaryOperator::Add => builder.build_int_add(lhs, rhs, name),
                        BinaryOperator::Sub => builder.build_int_sub(lhs, rhs, name),
                        BinaryOperator::Mul => builder.build_int_mul(lhs, rhs, name),
                        BinaryOperator::Div => {
                            if is_signed {
                                builder.build_int_signed_div(lhs, rhs, name)
                            } else {
                                builder.build_int_unsigned_div(lhs, rhs, name)
                            }
                        }
                        BinaryOperator::Eq => {
                            builder.build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, name)
                        }
                        BinaryOperator::Ne => {
                            builder.build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, name)
                        }
                        BinaryOperator::Gt => builder.build_int_compare(
                            if is_signed {
                                inkwell::IntPredicate::SGT
                            } else {
                                inkwell::IntPredicate::UGT
                            },
                            lhs,
                            rhs,
                            name,
                        ),
                        BinaryOperator::Ge => builder.build_int_compare(
                            if is_signed {
                                inkwell::IntPredicate::SGE
                            } else {
                                inkwell::IntPredicate::UGE
                            },
                            lhs,
                            rhs,
                            name,
                        ),
                        BinaryOperator::Lt => builder.build_int_compare(
                            if is_signed {
                                inkwell::IntPredicate::SLT
                            } else {
                                inkwell::IntPredicate::ULT
                            },
                            lhs,
                            rhs,
                            name,
                        ),
                        BinaryOperator::Le => builder.build_int_compare(
                            if is_signed {
                                inkwell::IntPredicate::SLE
                            } else {
                                inkwell::IntPredicate::ULE
                            },
                            lhs,
                            rhs,
                            name,
                        ),
                        BinaryOperator::And => builder.build_and(lhs, rhs, name),
                        BinaryOperator::Or => builder.build_or(lhs, rhs, name),
                        BinaryOperator::BitXor => builder.build_xor(lhs, rhs, name),
                        BinaryOperator::Shl => builder.build_left_shift(lhs, rhs, name),
                        BinaryOperator::Shr => builder.build_right_shift(lhs, rhs, is_signed, name),
                    }
                    .unwrap()
                    .as_basic_value_enum()
                } else {
                    let lhs = left_expr.1.into_float_value();
                    let rhs = right_expr.1.into_float_value();
                    let name = &store_in.unwrap().to_string();
                    let builder = self.mod_scope.builder();

                    match operator {
                        BinaryOperator::Add => builder
                            .build_float_add(lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Sub => builder
                            .build_float_sub(lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Mul => builder
                            .build_float_mul(lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Div => builder
                            .build_float_div(lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Eq => builder
                            .build_float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Ne => builder
                            .build_float_compare(inkwell::FloatPredicate::ONE, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Gt => builder
                            .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Ge => builder
                            .build_float_compare(inkwell::FloatPredicate::OGE, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Lt => builder
                            .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        BinaryOperator::Le => builder
                            .build_float_compare(inkwell::FloatPredicate::OLE, lhs, rhs, name)
                            .unwrap()
                            .as_basic_value_enum(),
                        _ => todo!(),
                    }
                };

                Some((
                    util::into_primitive_context_type(ty, &self.mod_scope.context()).unwrap(),
                    res,
                ))
            }
            Expression::Unary { operator, expr, ty } => {
                let expr = self.emit_static_expression(expr).unwrap();

                if ty.is_numeric() {
                    let is_int = match ty {
                        Type::Float32 | Type::Float64 => false,
                        _ => true,
                    };
                    let res = if is_int {
                        let ty = util::into_primitive_context_type(ty, &self.mod_scope.context())
                            .unwrap();
                        let value = expr.1.into_int_value();
                        let name = &store_in.unwrap().to_string();

                        match operator {
                            UnaryOperator::Neg => self
                                .mod_scope
                                .builder()
                                .build_int_neg(value, name)
                                .unwrap()
                                .as_basic_value_enum(),
                            UnaryOperator::Not => self
                                .mod_scope
                                .builder()
                                .build_not(value, name)
                                .unwrap()
                                .as_basic_value_enum(),
                            UnaryOperator::Ref => unsafe {
                                self.mod_scope.builder().build_gep(
                                    value.get_type(),
                                    value.const_to_pointer(ty.ptr_type(AddressSpace::default())),
                                    &[],
                                    name,
                                )
                            }
                            .unwrap()
                            .as_basic_value_enum(),
                            UnaryOperator::Deref => todo!(),
                        }
                    } else {
                        let value = expr.1.into_float_value();
                        let name = &store_in.unwrap().to_string();

                        match operator {
                            UnaryOperator::Neg => self
                                .mod_scope
                                .builder()
                                .build_float_neg(value, name)
                                .unwrap()
                                .as_basic_value_enum(),
                            UnaryOperator::Ref => todo!(),
                            UnaryOperator::Deref => todo!(),
                            UnaryOperator::Not => todo!(),
                        }
                    };

                    Some((
                        util::into_primitive_context_type(ty, &self.mod_scope.context()).unwrap(),
                        res,
                    ))
                } else {
                    todo!()
                }
            }
            Expression::Call {
                expr,
                arguments,
                ty,
            } => {
                let name = match expr.as_ref() {
                    Expression::Static(expr, _) => match expr.as_ref() {
                        StaticExpression::FnIdentifier(ident) => ident,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                println!("IDENT FN: {name}");

                let func = self.mod_scope.get_function(name).unwrap();

                let args = arguments
                    .iter()
                    .map(|arg| self.emit(Rc::clone(arg), None).unwrap())
                    .collect::<Vec<_>>();

                let value = self
                    .mod_scope
                    .builder()
                    .build_call(
                        func,
                        &args
                            .into_iter()
                            .map(|(_, val)| BasicMetadataValueEnum::from(val))
                            .collect::<Vec<BasicMetadataValueEnum>>(),
                        name,
                    )
                    .unwrap();

                Some((
                    util::into_primitive_context_type(ty, &self.mod_scope.context()).unwrap(),
                    value.try_as_basic_value().left().unwrap(),
                ))
            }
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
            StaticExpression::Ptr(expr) => {
                let cx = self.mod_scope.context();
                Some((
                    cx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
                    self.emit_static_expression(expr)
                        .unwrap()
                        .1
                        .as_basic_value_enum(),
                ))
            }
            StaticExpression::FnIdentifier(ident) => {
                panic!("{ident}")
            }
        }
    }
}
