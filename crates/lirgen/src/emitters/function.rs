use std::{cell::RefCell, rc::Rc};

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

use crate::{
    var_name_gen::VariableNameGenerator,
    variable::{LirVariable, LirVariableKind},
};

use super::expression::LirExpressionEmitter;

pub struct LirFunctionEmitter {
    stmts: RefCell<Vec<Rc<Statement>>>,
}

impl LirFunctionEmitter {
    pub fn new() -> Self {
        Self {
            stmts: RefCell::default(),
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
            let (stmts, name) = LirExpressionEmitter::new().emit_into_variable(&block, None);
            (stmts, Some(name))
        } else {
            (LirExpressionEmitter::new().emit(&block), None)
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

    // fn lower_stmt(&self, stmt: Rc<CheckedStatement>) {
    //     match stmt.stmt.as_ref() {
    //         StatementKind::Expression { expr, end_semi } => {
    //             if *end_semi {
    //                 // this is an implicit return from a function or a block
    //                 self.lower_expr(expr, None);
    //             } else {
    //                 if expr.effects.is_empty() {
    //                     // if there are no effects
    //                     // do not execute an expression statement (skip it)
    //                     return;
    //                 }

    //                 self.lower_expr(expr, None);
    //             }
    //         }
    //         _ => todo!(),
    //     }
    // }
}
