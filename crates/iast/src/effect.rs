use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Effect {
    IO,
    Network,
    FileSystem,
    Concurrency,
    State,
    NonDet,
    Env,
}

impl TryFrom<&str> for Effect {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "io" => Ok(Self::IO),
            "net" => Ok(Self::Network),
            "fs" => Ok(Self::FileSystem),
            "async" => Ok(Self::Concurrency),
            "mut" => Ok(Self::State),
            "nondet" => Ok(Self::NonDet),
            other => Err(format!("`{other}` is not a valid effect.")),
        }
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::IO => "io",
            Self::Concurrency => "async",
            Self::Env => "env",
            Self::FileSystem => "fs",
            Self::Network => "net",
            Self::State => "mut",
            Self::NonDet => "nondet",
        };

        write!(f, "{s}")
    }
}
