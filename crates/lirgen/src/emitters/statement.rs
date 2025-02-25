use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use lir::{
    expression::{Expression, StaticExpression},
    operator::UnaryOperator,
    statement::{MethodDeclaration, Statement},
};
use tcast::statement::{Statement as CheckedStatement, StatementKind};

use crate::builder::FunctionBuilder;

use super::function::LirFunctionEmitterScope;

use crate::variable_ref::VariableRef;

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
                    .emit(expr);
            }
            StatementKind::StructDeclaration {
                name,
                fields,
                methods,
                ty,
            } => {
                todo!()
            }
            StatementKind::VariableDeclaration {
                name,
                expr,
                ty,
                is_mut,
            } => {
                let ssa_name = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .expr_emitter
                    .emit(expr);
                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                let ssa_name = match ssa_name.var {
                    VariableRef::Direct(id) => id,
                    VariableRef::Pointer(ptr) => {
                        let load_id = self
                            .scope
                            .borrow()
                            .upgrade()
                            .unwrap()
                            .name_gen
                            .borrow_mut()
                            .generate();
                        self.builder
                            .push(Rc::new(Statement::StaticVariableDeclaration {
                                id: load_id,
                                expr: Rc::new(Expression::Unary {
                                    operator: UnaryOperator::Deref,
                                    expr: StaticExpression::Identifier(ptr.to_string()),
                                    ty: expr.ty.clone(),
                                }),
                                ty: ty.clone(),
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let stmt = Statement::VariableDeclaration {
                    name: id,
                    expr: Rc::new(Expression::Static(
                        Rc::new(StaticExpression::Identifier(ssa_name)),
                        expr.ty.clone(),
                    )),
                    ty: ty.clone(),
                    is_mut: *is_mut,
                };

                self.scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .environment
                    .borrow_mut()
                    .define(name.to_string(), id, *is_mut);

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
                    .emit(expr);

                let ssa_name = match ssa_name.var {
                    VariableRef::Direct(id) => id,
                    VariableRef::Pointer(ptr) => {
                        let load_id = self
                            .scope
                            .borrow()
                            .upgrade()
                            .unwrap()
                            .name_gen
                            .borrow_mut()
                            .generate();
                        self.builder
                            .push(Rc::new(Statement::StaticVariableDeclaration {
                                id: load_id,
                                expr: Rc::new(Expression::Unary {
                                    operator: UnaryOperator::Deref,
                                    expr: StaticExpression::Identifier(ptr.to_string()),
                                    ty: expr.ty.clone(),
                                }),
                                ty: expr.ty.clone(),
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let ret = Statement::Return(Rc::new(StaticExpression::Identifier(ssa_name)));

                self.builder.push(Rc::new(ret));
            }
        }
    }
}
