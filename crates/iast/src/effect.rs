#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Effect {
    IO,
    Network,
    FileSystem,
    Concurrency,
    State,
    NonDet,
    Env,
    External,
    Logging,
}
