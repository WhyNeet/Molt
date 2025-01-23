use std::{cell::RefCell, rc::Rc};

use common::Type;
use lir::expression::{Expression, StaticExpression};

#[derive(Debug)]
pub enum LirVariableKind {
    Named,
    Temporary,
}

#[derive(Debug)]
pub struct LirVariable {
    name: String,
    kind: LirVariableKind,
    ty: Type,
    expr: RefCell<Option<Rc<StaticExpression>>>,
}

impl LirVariable {
    pub fn new(name: String, kind: LirVariableKind, ty: Type) -> Self {
        Self {
            name,
            expr: RefCell::default(),
            kind,
            ty,
        }
    }

    pub fn put(&self, expr: Rc<StaticExpression>) {
        let mut var_expr = self.expr.borrow_mut();

        if var_expr.is_some() {
            panic!("cannot put expression into a variable more than once.")
        }

        *var_expr = Some(expr);
    }

    pub fn take(self) -> (String, Option<Rc<StaticExpression>>, Type) {
        (self.name, self.expr.borrow().clone(), self.ty)
    }
}
