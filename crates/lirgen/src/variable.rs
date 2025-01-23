use std::{cell::RefCell, rc::Rc};

use common::Type;
use lir::expression::{Expression, StaticExpression};

#[derive(Debug)]
pub struct LirVariable {
    name: String,
    ty: Type,
    expr: RefCell<Option<Rc<Expression>>>,
}

impl LirVariable {
    pub fn new(name: String, ty: Type) -> Self {
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

    pub fn take(self) -> (String, Option<Rc<Expression>>, Type) {
        (self.name, self.expr.borrow().clone(), self.ty)
    }
}
