use std::{
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
    ssa_name_gen: VariableNameGenerator,
    stmts: Vec<Rc<Statement>>,
    scope: Weak<LirFunctionEmitterScope>,
}

impl LirExpressionEmitter {
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

impl LirExpressionEmitter {
    /// If the variable is `None`, returns the identifier for a new temporary variable
    pub fn emit_into_variable(
        &mut self,
        expr: &CheckedExpression,
        variable: Option<LirVariable>,
    ) -> (Vec<Rc<Statement>>, String) {
        let name = self.ssa_name_gen.generate();

        println!("store expr in: {name}");

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

        println!("ssa with name: {var_name}");

        self.stmts.push(Rc::new(ssa));

        (self.stmts.clone(), var_name)
    }

    pub fn emit(&mut self, expr: &CheckedExpression) -> Vec<Rc<Statement>> {
        self.lower_expr(expr, None);

        self.stmts.clone()
    }

    fn lower_expr(&mut self, expr: &CheckedExpression, store_in: Option<&LirVariable>) {
        match expr.expr.as_ref() {
            ExpressionKind::Literal(literal) => {
                let expr = Rc::new(Expression::Static(Rc::new(StaticExpression::Literal(
                    Rc::clone(literal),
                ))));

                if let Some(variable) = store_in {
                    variable.put(expr);
                }
            }
            ExpressionKind::Block(stmts) => {
                let mut block_stmts = if stmts.len() > 1 {
                    stmts[..(stmts.len() - 1)]
                        .iter()
                        .map(|stmt| {
                            self.scope
                                .upgrade()
                                .unwrap()
                                .stmt_emitter
                                .borrow_mut()
                                .emit(stmt)
                        })
                        .flatten()
                        .collect::<Vec<Rc<Statement>>>()
                } else {
                    vec![]
                };

                println!("block: {block_stmts:?}");

                self.stmts.append(&mut block_stmts);

                if let Some(last) = stmts.last() {
                    match last.stmt.as_ref() {
                        StatementKind::Expression { expr, end_semi } if !*end_semi => {
                            self.lower_expr(expr, store_in);
                        }
                        _ => {
                            let mut stmts = self
                                .scope
                                .upgrade()
                                .unwrap()
                                .stmt_emitter
                                .borrow_mut()
                                .emit(last);

                            self.stmts.append(&mut stmts)
                        }
                    }
                };
            }
            _ => todo!(),
        }
    }
}
