use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    operator::{BinaryOperator, UnaryOperator},
    statement::Statement,
};
use tcast::{
    expression::{Expression as CheckedExpression, ExpressionKind},
    statement::{Statement as CheckedStatement, StatementKind},
};

use crate::{builder::FunctionBuilder, variable::LirVariable};

use super::function::LirFunctionEmitterScope;

#[derive(Debug)]
pub enum LirExpressionContext {
    Default,
    Loop { loop_id: u64, exit_id: u64 },
}

impl Default for LirExpressionContext {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Default)]
pub struct LirExpressionEmitter {
    builder: Rc<FunctionBuilder>,
    scope: RefCell<Weak<LirFunctionEmitterScope>>,
    cx: RefCell<LirExpressionContext>,
}

impl LirExpressionEmitter {
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

impl LirExpressionEmitter {
    /// Returns the identifier for a new temporary variable
    pub fn emit_into_variable(
        &self,
        expr: &CheckedExpression,
        variable: Option<LirVariable>,
    ) -> String {
        let name = self
            .scope
            .borrow()
            .upgrade()
            .unwrap()
            .name_gen
            .borrow_mut()
            .generate();

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

        self.builder.push(Rc::new(ssa));

        var_name
    }

    pub fn emit(&self, expr: &CheckedExpression) {
        self.lower_expr(expr, None);
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
                self.lower_block(stmts, store_in);
            }
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_ssa_name = self.emit_into_variable(left, None);
                let right_ssa_name = self.emit_into_variable(right, None);

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Binary {
                        left: StaticExpression::Identifier(left_ssa_name),
                        operator: BinaryOperator::from(operator),
                        right: StaticExpression::Identifier(right_ssa_name),
                    }));
                }
            }
            ExpressionKind::Identifier(ident) => {
                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .environment
                    .borrow()
                    .get(ident)
                    .unwrap();
                let expr = StaticExpression::Identifier(id.to_string());

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Static(Rc::new(expr))));
                }
            }
            ExpressionKind::Unary { operator, expr } => {
                let ssa_name = self.emit_into_variable(expr, None);

                let lir_expr = Expression::Unary {
                    operator: UnaryOperator::from(operator),
                    expr: StaticExpression::Identifier(ssa_name),
                };

                if let Some(variable) = store_in {
                    variable.store(Rc::new(lir_expr));
                }
            }
            ExpressionKind::Grouping(expr) => {
                let ssa_name = self.emit_into_variable(expr, None);

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Static(Rc::new(
                        StaticExpression::Identifier(ssa_name),
                    ))));
                }
            }
            ExpressionKind::Call {
                expr: sub_expr,
                arguments,
            } => {
                let fn_ident = match sub_expr.expr.as_ref() {
                    ExpressionKind::Identifier(ident) => ident,
                    _ => todo!("callable expressions are not yet implemented"),
                };

                let mut fn_args = vec![];

                for argument in arguments {
                    let ssa_name = self.emit_into_variable(argument, None);
                    fn_args.push(Rc::new(Expression::Static(Rc::new(
                        StaticExpression::Identifier(ssa_name),
                    ))));
                }

                if let Some(variable) = store_in {
                    variable.store(Rc::new(Expression::Call {
                        expr: Rc::new(Expression::Static(Rc::new(StaticExpression::Identifier(
                            fn_ident.to_string(),
                        )))),
                        arguments: fn_args,
                    }));
                }
            }
            ExpressionKind::Cast { expr, ty } => {
                let ssa_name = self.emit_into_variable(expr, None);

                let lir_expr = Expression::Cast {
                    ty: ty.clone(),
                    expr: Rc::new(StaticExpression::Identifier(ssa_name)),
                };

                if let Some(variable) = store_in {
                    variable.store(Rc::new(lir_expr));
                }
            }
            ExpressionKind::Conditional {
                condition,
                body,
                alternative,
            } => {
                let condition_ssa = self.emit_into_variable(condition, None);
                let entry_block_id = self.builder.current_block_id();
                let body_id = self.builder.append_block();
                self.lower_block(body, store_in);

                let alternative_id = if let Some(stmts) = alternative {
                    let alternative_id = self.builder.append_block();
                    self.lower_expr(stmts, store_in);
                    Some(alternative_id)
                } else {
                    None
                };

                let after_conditional_block_id = self.builder.append_block();

                self.builder.position_at_end(body_id);
                self.builder
                    .push(Rc::new(Statement::Goto(after_conditional_block_id)));

                if let Some(alt_id) = alternative_id {
                    self.builder.position_at_end(alt_id);
                    self.builder
                        .push(Rc::new(Statement::Goto(after_conditional_block_id)));
                }

                let branch = Statement::Branch {
                    condition: Rc::new(StaticExpression::Identifier(condition_ssa)),
                    then: body_id,
                    alternative: alternative_id.unwrap_or(after_conditional_block_id),
                };
                self.builder.position_at_end(entry_block_id);
                self.builder.push(Rc::new(branch));

                self.builder.position_at_end(after_conditional_block_id);
            }
            ExpressionKind::Loop(stmts) => {
                let entry_block_id = self.builder.current_block_id();
                let loop_block_id = self.builder.append_block();
                self.builder.position_at_end(entry_block_id);
                self.builder.push(Rc::new(Statement::Goto(loop_block_id)));
                let exit_block_id = self.builder.append_block();
                self.builder.position_at_end(loop_block_id);

                let prev_cx = self.cx.replace(LirExpressionContext::Loop {
                    loop_id: loop_block_id,
                    exit_id: exit_block_id,
                });

                self.lower_block(stmts, None);

                self.cx.replace(prev_cx);

                self.builder.position_at_end(exit_block_id);
            }
            ExpressionKind::Break => {
                let exit_block_id = match &*self.cx.borrow() {
                    LirExpressionContext::Loop { exit_id, .. } => *exit_id,
                    _ => unreachable!(),
                };
                let goto = Statement::Goto(exit_block_id);
                self.builder.push(Rc::new(goto));
            }
            ExpressionKind::Continue => {
                let loop_block_id = match &*self.cx.borrow() {
                    LirExpressionContext::Loop { loop_id, .. } => *loop_id,
                    _ => unreachable!(),
                };
                let goto = Statement::Goto(loop_block_id);
                self.builder.push(Rc::new(goto));
            }
            ExpressionKind::Assignment { identifier, expr } => todo!(),
            ExpressionKind::MemberAccess { expr, ident } => todo!(),
        }
    }

    fn lower_block(&self, stmts: &Vec<Rc<CheckedStatement>>, store_in: Option<&LirVariable>) {
        let stmt_emitter = &self.scope.borrow().upgrade().unwrap().stmt_emitter;

        if stmts.len() > 1 {
            for stmt in stmts[..(stmts.len() - 1)].iter() {
                stmt_emitter.emit(stmt);
            }
        }

        if let Some(last) = stmts.last() {
            match last.stmt.as_ref() {
                StatementKind::Expression { expr, end_semi } if !*end_semi => {
                    self.lower_expr(expr, store_in);
                }
                _ => {
                    stmt_emitter.emit(last);
                }
            }
        };
    }
}
