use std::fmt;

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
        var_args: bool,
    },
    Unit,
    NoReturn,
    Ptr(Box<Type>),
    Struct {
        fields: Vec<(String, Type)>,
        methods: Vec<(String, Type)>,
    },
    Named(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Str => write!(f, "str"),
            Self::Int8 => write!(f, "i8"),
            Self::Int16 => write!(f, "i16"),
            Self::Int32 => write!(f, "i32"),
            Self::Int64 => write!(f, "i64"),
            Self::UInt8 => write!(f, "u8"),
            Self::UInt16 => write!(f, "u16"),
            Self::UInt32 => write!(f, "u32"),
            Self::UInt64 => write!(f, "u64"),
            Type::Float32 => write!(f, "f32"),
            Type::Float64 => write!(f, "f64"),
            Type::Callable {
                parameters,
                return_type,
                var_args,
            } => write!(
                f,
                "callable({}{}{}) -> {return_type}",
                parameters[..(parameters.len() - 1)]
                    .iter()
                    .map(|param| param.to_string() + ", ")
                    .collect::<String>(),
                parameters[parameters.len() - 1],
                if *var_args { ", ..." } else { "" }
            ),
            Type::Unit => write!(f, "unit"),
            Type::NoReturn => unreachable!(),
            Type::Ptr(ty) => write!(f, "*{ty}"),
            Type::Struct { fields, methods } => {
                write!(
                    f,
                    "struct {{ {} fields; {} methods; }}",
                    fields.len(),
                    methods.len()
                )?;

                Ok(())
            }
            Type::Named(_) => unreachable!(),
        }
    }
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
            "unit" => Ok(Self::Unit),
            "noreturn" => Ok(Self::NoReturn),
            struct_name => Ok(Self::Named(struct_name.to_string())),
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

    pub fn is_numeric_signed(&self) -> bool {
        match self {
            Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int64
            | Self::Float32
            | Self::Float64 => true,
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
                Type::Ptr(_) if **ty1 == Type::Int8 => true,
                Type::Ptr(ty2) => ty1 == ty2,
                _ => false,
            },
            _ => other.is_numeric(),
        }
    }

    pub fn numeric_bits(&self) -> Option<u8> {
        match self {
            Self::Int8 | Self::UInt8 => Some(8),
            Self::Int16 | Self::UInt16 => Some(16),
            Self::Int32 | Self::UInt32 | Self::Float32 => Some(32),
            Self::Int64 | Self::UInt64 | Self::Float64 => Some(64),
            _ => None,
        }
    }
}
