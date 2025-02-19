use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    statement::Statement,
};
use tcast::statement::{Statement as CheckedStatement, StatementKind};

use crate::builder::FunctionBuilder;

use super::function::LirFunctionEmitterScope;

#[derive(Debug, Default)]
pub struct LirStatementEmitter {
    builder: Rc<FunctionBuilder>,
    scope: RefCell<Weak<LirFunctionEmitterScope>>,
}

impl LirStatementEmitter {
    pub fn new(scope: Weak<LirFunctionEmitterScope>, builder: Rc<FunctionBuilder>) -> Self {
        Self {
            scope: RefCell::new(scope),
            builder,
            ..Default::default()
        }
    }

    pub(crate) fn update_scope(&self, scope: Weak<LirFunctionEmitterScope>) {
        self.scope.replace(scope);
    }
}

impl LirStatementEmitter {
    pub fn emit(&self, stmt: &CheckedStatement) {
        self.lower(stmt);
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

                self.scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit_into_variable(expr, None);
            }
            StatementKind::VariableDeclaration { name, expr, ty } => {
                let ssa_name = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit_into_variable(expr, None);
                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();
                let stmt = Statement::VariableDeclaration {
                    name: id,
                    expr: Rc::new(Expression::Static(
                        Rc::new(StaticExpression::Identifier(ssa_name.to_string())),
                        expr.ty.clone(),
                    )),
                    ty: ty.clone(),
                };

                self.scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .environment
                    .borrow_mut()
                    .define(name.to_string(), id);

                self.builder.push(Rc::new(stmt));
            }
            StatementKind::FunctionDeclaration { .. } => unreachable!(),
            StatementKind::Return(expr) => {
                let ssa_name = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit_into_variable(expr, None);

                let ret =
                    Statement::Return(Rc::new(StaticExpression::Identifier(ssa_name.to_string())));

                self.builder.push(Rc::new(ret));
            }
        }
    }
}
