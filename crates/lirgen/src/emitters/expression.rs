use std::{
    borrow::BorrowMut,
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    statement::{Statement, VariableAllocationKind},
};
use tcast::{
    expression::{Expression as CheckedExpression, ExpressionKind},
    statement::{Statement as CheckedStatement, StatementKind},
};

use crate::{
    var_name_gen::VariableNameGenerator,
    variable::{LirVariable, LirVariableKind},
};

use super::function::LirFunctionEmitterScope;

#[derive(Debug, Default)]
pub struct LirExpressionEmitter {
    ssa_name_gen: RefCell<VariableNameGenerator>,
    stmts: RefCell<Vec<Rc<Statement>>>,
    scope: RefCell<Weak<LirFunctionEmitterScope>>,
}

impl LirExpressionEmitter {
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

impl LirExpressionEmitter {
    /// If the variable is `None`, returns the identifier for a new temporary variable
    pub fn emit_into_variable(
        &self,
        expr: &CheckedExpression,
        variable: Option<LirVariable>,
    ) -> (Vec<Rc<Statement>>, String) {
        let name = self.ssa_name_gen.borrow_mut().generate();

        let var = if let Some(var) = variable {
            var
        } else {
            LirVariable::new(
                name.to_string(),
                LirVariableKind::Temporary,
                expr.ty.clone(),
            )
        };
        self.lower_expr(expr, Some(&var));

        let (var_name, expr, ty) = var.take();

        let ssa = Statement::VariableDeclaration {
            name: name.to_string(),
            allocation: VariableAllocationKind::SSA,
            expr: expr.unwrap(),
            ty,
        };

        self.stmts.borrow_mut().push(Rc::new(ssa));

        (self.stmts.take(), var_name)
    }

    pub fn emit(&self, expr: &CheckedExpression) -> Vec<Rc<Statement>> {
        self.lower_expr(expr, None);

        self.stmts.take()
    }

    fn lower_expr(&self, expr: &CheckedExpression, store_in: Option<&LirVariable>) {
        match expr.expr.as_ref() {
            ExpressionKind::Literal(literal) => {
                let expr = StaticExpression::Literal(Rc::clone(literal));

                if let Some(variable) = store_in {
                    variable.put(Rc::new(expr));
                }
            }
            ExpressionKind::Block(stmts) => {
                let stmt_emitter = &self.scope.borrow().upgrade().unwrap().stmt_emitter;

                let mut block_stmts = if stmts.len() > 1 {
                    stmts[..(stmts.len() - 1)]
                        .iter()
                        .map(|stmt| stmt_emitter.emit(stmt))
                        .flatten()
                        .collect::<Vec<Rc<Statement>>>()
                } else {
                    vec![]
                };

                self.stmts.borrow_mut().append(&mut block_stmts);

                if let Some(last) = stmts.last() {
                    match last.stmt.as_ref() {
                        StatementKind::Expression { expr, end_semi } if !*end_semi => {
                            self.lower_expr(expr, store_in);
                        }
                        _ => {
                            let mut stmts = stmt_emitter.emit(last);

                            self.stmts.borrow_mut().append(&mut stmts)
                        }
                    }
                };
            }
            _ => todo!(),
        }
    }
}
