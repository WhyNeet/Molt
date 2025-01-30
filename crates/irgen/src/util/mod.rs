use common::Type;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

pub fn into_primitive_context_type<'a>(ty: &Type, cx: &'a Context) -> BasicTypeEnum<'a> {
    match ty {
        Type::Bool => cx.bool_type().as_basic_type_enum(),
        Type::Float32 => cx.f32_type().as_basic_type_enum(),
        Type::Float64 => cx.f64_type().as_basic_type_enum(),
        Type::Int8 | Type::UInt8 => cx.i8_type().as_basic_type_enum(),
        Type::Int16 | Type::UInt16 => cx.i16_type().as_basic_type_enum(),
        Type::Int32 | Type::UInt32 => cx.i32_type().as_basic_type_enum(),
        Type::Int64 | Type::UInt64 => cx.i64_type().as_basic_type_enum(),
        Type::Char => cx.i8_type().as_basic_type_enum(),
        other => panic!("`{other:?}` is not a primitive type."),
    }
}
