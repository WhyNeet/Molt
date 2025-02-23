use common::Type;

#[derive(Debug)]
pub enum Context {
    /// Module context.
    Global,
    /// Function context with return type.
    Function(Type),

    Method {
        struct_name: String,
        return_type: Type,
    },
}

impl Context {
    pub fn is_global(&self) -> bool {
        match self {
            Self::Global => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(_) => true,
            _ => false,
        }
    }

    pub fn is_method(&self) -> bool {
        match self {
            Self::Method { .. } => true,
            _ => false,
        }
    }
}
