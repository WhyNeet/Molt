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
    Ptr(Box<Type>),
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

impl Type {
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Float32
            | Type::Float64
            | Type::Int8
            | Type::UInt8
            | Type::Int16
            | Type::UInt16
            | Type::Int32
            | Type::UInt32
            | Type::Int64
            | Type::UInt64 => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Ptr(_) => true,
            _ => false,
        }
    }

    pub fn can_cast(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }

        match self {
            Type::Str | Type::Callable { .. } | Type::Unit | Type::NoReturn => false,
            Type::Char => match other {
                Type::UInt8 => true,
                _ => false,
            },
            Type::Bool => other.is_numeric(),
            Type::Ptr(ty1) => match other {
                Type::Ptr(ty2) if **ty2 == Type::Int8 => true,
                Type::Ptr(ty2) => ty1 == ty2,
                _ => false,
            },
            _ => other.is_numeric(),
        }
    }
}
