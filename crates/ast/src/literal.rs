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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    Str,
    Char,
    Bool,
    Callable {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Unit,
    NoReturn,
}

impl TryFrom<&str> for Type {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "u8" => Ok(Self::UInt8),
            "i8" => Ok(Self::Int8),
            "u16" => Ok(Self::UInt16),
            "i16" => Ok(Self::Int16),
            "u32" => Ok(Self::UInt32),
            "i32" => Ok(Self::Int32),
            "u64" => Ok(Self::UInt64),
            "i64" => Ok(Self::Int64),
            "f32" => Ok(Self::Float32),
            "f64" => Ok(Self::Float64),
            "bool" => Ok(Self::Bool),
            "char" => Ok(Self::Char),
            "str" => Ok(Self::Str),
            "()" => Ok(Self::Unit),
            "noreturn" => Ok(Self::NoReturn),
            other => Err(format!("`{other}` is not a primitive type.")),
        }
    }
}

pub trait Typed {
    fn get_type(&self) -> Type;
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
