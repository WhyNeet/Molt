use crate::{Type, Typed};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(Number),
    Str(String),
    Char(char),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
}

impl Typed for Number {
    fn get_type(&self) -> Type {
        match self {
            Self::UInt8(_) => Type::UInt8,
            Self::Int8(_) => Type::Int8,
            Self::UInt16(_) => Type::UInt16,
            Self::Int16(_) => Type::Int16,
            Self::UInt32(_) => Type::UInt32,
            Self::Int32(_) => Type::Int32,
            Self::UInt64(_) => Type::UInt64,
            Self::Int64(_) => Type::Int64,
            Self::Float32(_) => Type::Float32,
            Self::Float64(_) => Type::Float64,
        }
    }
}

impl Typed for Literal {
    fn get_type(&self) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::Bool(_) => Type::Bool,
            Self::Str(_) => Type::Str,
            Self::Char(_) => Type::Char,
            Self::Number(number) => number.get_type(),
        }
    }
}
