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
