use std::{cell::RefCell, rc::Rc};

use common::Type;
use lir::expression::Expression;

#[derive(Debug)]
pub enum LirVariableKind {
    Named,
    Temporary,
}

#[derive(Debug)]
pub struct LirVariable {
    kind: LirVariableKind,
    ty: Type,
    expr: RefCell<Option<Rc<Expression>>>,
}

impl LirVariable {
    pub fn new(kind: LirVariableKind, ty: Type) -> Self {
        Self {
            expr: RefCell::default(),
            kind,
            ty,
        }
    }

    pub fn put(&self, expr: Rc<Expression>) {
        let mut var_expr = self.expr.borrow_mut();

        if var_expr.is_some() {
            panic!("cannot put expression into a variable more than once.")
        }

        *var_expr = Some(expr);
    }

    pub fn take(self) -> (Option<Rc<Expression>>, Type) {
        (self.expr.borrow().clone(), self.ty)
    }
}
