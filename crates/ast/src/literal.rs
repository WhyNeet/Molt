#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(Number),
    Str(String),
    Char(char),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq)]
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
