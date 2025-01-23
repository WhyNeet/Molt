use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    operator::BinaryOperator,
    statement::Statement,
};
use tcast::{
    expression::{Expression as CheckedExpression, ExpressionKind},
    statement::StatementKind,
};

use crate::{var_name_gen::VariableNameGenerator, variable::LirVariable};

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
            LirVariable::new(name.to_string(), expr.ty.clone())
        };
        self.lower_expr(expr, Some(&var));

        let (var_name, expr, ty) = var.take();

        let ssa = Statement::StaticVariableDeclaration {
            id: name,
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
                let expr =
                    Expression::Static(Rc::new(StaticExpression::Literal(Rc::clone(literal))));

                if let Some(variable) = store_in {
                    variable.store(Rc::new(expr));
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
            ExpressionKind::Assignment { .. } => todo!(),
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let (mut left_stmts, left_ssa_name) = self.emit_into_variable(left, None);
                let (mut right_stmts, right_ssa_name) = self.emit_into_variable(right, None);

                let mut stmts = self.stmts.borrow_mut();

                stmts.append(&mut left_stmts);
                stmts.append(&mut right_stmts);

                let lir_expr = Expression::Binary {
                    left: StaticExpression::Identifier(left_ssa_name),
                    operator: BinaryOperator::from(operator),
                    right: StaticExpression::Identifier(right_ssa_name),
                };

                let ssa_id = self.ssa_name_gen.borrow_mut().generate();
                let ssa = Statement::StaticVariableDeclaration {
                    id: ssa_id,
                    expr: Rc::new(lir_expr),
                    ty: expr.ty.clone(),
                };

                stmts.push(Rc::new(ssa));

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Static(Rc::new(
                        StaticExpression::Identifier(ssa_id.to_string()),
                    ))));
                }
            }
            ExpressionKind::Identifier(ident) => {
                let expr = StaticExpression::Identifier(ident.to_string());

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Static(Rc::new(expr))));
                }
            }
            other => todo!("`{other:?}` is not implemented"),
        }
    }
}
