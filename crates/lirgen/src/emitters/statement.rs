use std::rc::{Rc, Weak};

use lir::{expression::StaticExpression, statement::Statement};
use tcast::statement::{Statement as CheckedStatement, StatementKind};

use super::function::LirFunctionEmitterScope;

#[derive(Debug, Default)]
pub struct LirStatementEmitter {
    stmts: Vec<Rc<Statement>>,
    scope: Weak<LirFunctionEmitterScope>,
}

impl LirStatementEmitter {
    pub fn new(scope: Weak<LirFunctionEmitterScope>) -> Self {
        Self {
            scope,
            ..Default::default()
        }
    }

    pub(crate) fn update_scope(&mut self, scope: Weak<LirFunctionEmitterScope>) {
        self.scope = scope;
    }
}

impl LirStatementEmitter {
    pub fn emit(&mut self, stmt: &CheckedStatement) -> Vec<Rc<Statement>> {
        self.lower(stmt);

        self.stmts.clone()
    }

    fn lower(&mut self, stmt: &CheckedStatement) {
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
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .borrow_mut()
                    .emit(expr);

                self.stmts.append(&mut stmts);

                // if *end_semi {
                // } else {
                //     // this is an implicit return from a function or a block
                //     let (mut stmts, tmp_name) = self
                //         .scope
                //         .upgrade()
                //         .unwrap()
                //         .expr_emitter
                //         .borrow_mut()
                //         .emit_into_variable(expr, None);

                //     let ret = Statement::Return(Rc::new(StaticExpression::Identifier(tmp_name)));

                //     self.stmts.push(Rc::new(ret));

                //     self.stmts.append(&mut stmts);
                // }
            }
            _ => todo!(),
        }
    }
}
