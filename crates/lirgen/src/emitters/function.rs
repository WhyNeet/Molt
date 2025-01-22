use std::{cell::RefCell, rc::Rc};

use common::Type;
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

pub struct LirFunctionEmitter {
    ssa_name_gen: RefCell<VariableNameGenerator>,
    stmts: RefCell<Vec<Rc<Statement>>>,
}

impl LirFunctionEmitter {
    pub fn new() -> Self {
        Self {
            ssa_name_gen: RefCell::default(),
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

        let expr_result = if block.ty != Type::Unit {
            Some(LirVariable::new(
                LirVariableKind::Temporary,
                block.ty.clone(),
            ))
        } else {
            None
        };

        self.lower_expr(&block, expr_result.as_ref());

        let temp_name = self.ssa_name_gen.borrow_mut().generate();

        if let Some(var) = expr_result {
            let (expr, ty) = var.take();

            let temp = Statement::VariableDeclaration {
                name: temp_name.clone(),
                expr: expr.expect("expected expression to be stored inside a variable."),
                allocation: VariableAllocationKind::SSA,
                ty,
            };

            self.stmts.borrow_mut().push(Rc::new(temp));
        }

        let ret = Statement::Return(Rc::new(StaticExpression::Identifier(temp_name)));

        self.stmts.borrow_mut().push(Rc::new(ret));

        Statement::FunctionDeclaration {
            name,
            block: self.stmts.take(),
            return_type,
            parameters,
        }
    }

    fn lower_stmt(&self, stmt: Rc<CheckedStatement>) {
        match stmt.stmt.as_ref() {
            StatementKind::Expression { expr, end_semi } => {
                if *end_semi {
                    // this is an implicit return from a function or a block
                    self.lower_expr(expr, None);
                } else {
                    if expr.effects.is_empty() {
                        // if there are no effects
                        // do not execute an expression statement (skip it)
                        return;
                    }

                    self.lower_expr(expr, None);
                }
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&self, expr: &CheckedExpression, store_in: Option<&LirVariable>) {
        match expr.expr.as_ref() {
            ExpressionKind::Literal(literal) => {
                let expr = Rc::new(Expression::Static(Rc::new(StaticExpression::Literal(
                    Rc::clone(literal),
                ))));

                if let Some(variable) = store_in {
                    variable.put(expr);
                }
            }
            _ => todo!(),
        }
    }
}
