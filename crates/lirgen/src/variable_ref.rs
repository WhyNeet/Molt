#[derive(Debug, PartialEq)]
pub enum VariableRef {
    Direct(String),
    Pointer(String),
    None,
}

impl VariableRef {
    pub fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }
}
