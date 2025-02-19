use std::{cell::RefCell, rc::Rc};

use common::Type;
use lir::expression::Expression;

#[derive(Debug)]
pub struct LirVariable {
    name: u64,
    ty: Type,
    expr: RefCell<Option<Rc<Expression>>>,
}

impl LirVariable {
    pub fn new(name: u64, ty: Type) -> Self {
        Self {
            name,
            expr: RefCell::default(),
            ty,
        }
    }

    pub fn store(&self, expr: Rc<Expression>) {
        let mut var_expr = self.expr.borrow_mut();

        if var_expr.is_some() {
            panic!("cannot put expression into a variable more than once.")
        }

        *var_expr = Some(expr);
    }

    pub fn take(self) -> (u64, Option<Rc<Expression>>, Type) {
        (self.name, self.expr.borrow().clone(), self.ty)
    }
}
