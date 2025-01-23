use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use common::{Literal, Type};
use lir::{
    expression::{Expression, StaticExpression},
    statement::{Statement, VariableAllocationKind},
};
use tcast::{
    expression::{Expression as CheckedExpression, ExpressionKind},
    fn_attribute::FunctionAttribute,
    statement::{Statement as CheckedStatement, StatementKind},
};

use super::{expression::LirExpressionEmitter, statement::LirStatementEmitter};

#[derive(Debug, Default)]
pub struct LirFunctionEmitterScope {
    pub(crate) expr_emitter: RefCell<LirExpressionEmitter>,
    pub(crate) stmt_emitter: RefCell<LirStatementEmitter>,
}

#[derive(Debug, Default)]
pub struct LirFunctionEmitter {
    scope: Rc<LirFunctionEmitterScope>,
    stmts: RefCell<Vec<Rc<Statement>>>,
}

impl LirFunctionEmitter {
    pub fn new() -> Self {
        let expr_emitter = LirExpressionEmitter::new(Weak::new());
        let stmt_emitter = LirStatementEmitter::new(Weak::new());

        let scope = LirFunctionEmitterScope {
            expr_emitter: RefCell::new(expr_emitter),
            stmt_emitter: RefCell::new(stmt_emitter),
        };

        let scope = Rc::new(scope);

        scope
            .expr_emitter
            .borrow_mut()
            .update_scope(Rc::downgrade(&scope));
        scope
            .stmt_emitter
            .borrow_mut()
            .update_scope(Rc::downgrade(&scope));

        Self {
            scope,
            ..Default::default()
        }
    }
}

impl LirFunctionEmitter {
    pub fn emit(
        &self,
        name: String,
        block: Option<Rc<CheckedExpression>>,
        return_type: Type,
        parameters: Vec<(String, Type)>,
    ) -> Statement {
        if block.is_none() {
            return Statement::ExternalFunctionDeclaration {
                name,
                return_type,
                parameters,
            };
        }

        let block = block.unwrap();

        let produces_value = block.ty != Type::Unit;

        let (mut expr_stmts, ssa_name) = if produces_value {
            let (stmts, name) = self
                .scope
                .expr_emitter
                .borrow_mut()
                .emit_into_variable(&block, None);
            (stmts, Some(name))
        } else {
            (self.scope.expr_emitter.borrow_mut().emit(&block), None)
        };

        self.stmts.borrow_mut().append(&mut expr_stmts);

        let ret = if let Some(ssa_name) = ssa_name {
            Statement::Return(Rc::new(StaticExpression::Identifier(ssa_name)))
        } else {
            Statement::Return(Rc::new(StaticExpression::Literal(Rc::new(Literal::Unit))))
        };

        self.stmts.borrow_mut().push(Rc::new(ret));

        Statement::FunctionDeclaration {
            name,
            block: self.stmts.take(),
            return_type,
            parameters,
        }
    }
}
