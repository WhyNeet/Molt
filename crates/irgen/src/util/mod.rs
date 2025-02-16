use common::Type;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};

pub fn into_primitive_context_type<'a>(ty: &Type, cx: &'a Context) -> Option<BasicTypeEnum<'a>> {
    match ty {
        Type::Bool => Some(cx.bool_type().as_basic_type_enum()),
        Type::Float32 => Some(cx.f32_type().as_basic_type_enum()),
        Type::Float64 => Some(cx.f64_type().as_basic_type_enum()),
        Type::Int8 | Type::UInt8 => Some(cx.i8_type().as_basic_type_enum()),
        Type::Int16 | Type::UInt16 => Some(cx.i16_type().as_basic_type_enum()),
        Type::Int32 | Type::UInt32 => Some(cx.i32_type().as_basic_type_enum()),
        Type::Int64 | Type::UInt64 => Some(cx.i64_type().as_basic_type_enum()),
        Type::Char => Some(cx.i8_type().as_basic_type_enum()),
        // Type::Str => Some(cx.i8_type().array_type()),
        other => None,
    }
}

pub fn into_ptr_context_type<'a>(ty: &Type, cx: &'a Context) -> Option<BasicTypeEnum<'a>> {
    match ty {
        Type::Ptr(ty) => Some(
            into_primitive_context_type(ty, cx)
                .map(|ty| ty.ptr_type(AddressSpace::default()).as_basic_type_enum())
                .unwrap_or_else(|| cx.ptr_type(AddressSpace::default()).as_basic_type_enum()),
        ),
        other => None,
    }
}
