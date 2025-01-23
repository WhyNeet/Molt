use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    statement::{Statement, VariableAllocationKind},
};
use tcast::statement::{Statement as CheckedStatement, StatementKind};

use super::function::LirFunctionEmitterScope;

#[derive(Debug, Default)]
pub struct LirStatementEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
    scope: RefCell<Weak<LirFunctionEmitterScope>>,
}

impl LirStatementEmitter {
    pub fn new(scope: Weak<LirFunctionEmitterScope>) -> Self {
        Self {
            scope: RefCell::new(scope),
            ..Default::default()
        }
    }

    pub(crate) fn update_scope(&self, scope: Weak<LirFunctionEmitterScope>) {
        self.scope.replace(scope);
    }
}

impl LirStatementEmitter {
    pub fn emit(&self, stmt: &CheckedStatement) -> Vec<Rc<Statement>> {
        self.lower(stmt);

        self.stmts.take()
    }

    fn lower(&self, stmt: &CheckedStatement) {
        match stmt.stmt.as_ref() {
            StatementKind::Expression { expr, .. } => {
                // block implicit returns will be processed differently
                // this is ONLY for expressions that end with `;`

                if expr.effects.is_empty() {
                    // if there are no effects
                    // do not execute an expression statement (skip it)
                    return;
                }

                let mut stmts = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit(expr);

                self.stmts.borrow_mut().append(&mut stmts);
            }
            StatementKind::VariableDeclaration { name, expr, ty } => {
                let (mut lir_stmts, ssa_name) = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit_into_variable(expr, None);

                self.stmts.borrow_mut().append(&mut lir_stmts);

                let stmt = Statement::VariableDeclaration {
                    name: name.to_string(),
                    expr: Rc::new(StaticExpression::Identifier(ssa_name)),
                    allocation: VariableAllocationKind::Stack,
                    ty: ty.clone(),
                };

                self.stmts.borrow_mut().push(Rc::new(stmt));
            }
            _ => todo!(),
        }
    }
}
