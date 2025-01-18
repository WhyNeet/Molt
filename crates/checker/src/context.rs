use ast::literal::Type;

#[derive(Debug)]
pub enum Context {
    /// Module context.
    Global,
    /// Function context with return type.
    Function(Type),
}
