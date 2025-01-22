use std::{cell::RefCell, rc::Rc};

use lir::{
    expression::{Expression, StaticExpression},
    statement::{Statement, VariableAllocationKind},
};
use tcast::expression::{Expression as CheckedExpression, ExpressionKind};

use crate::{
    var_name_gen::VariableNameGenerator,
    variable::{LirVariable, LirVariableKind},
};

#[derive(Debug, Default)]
pub struct LirExpressionEmitter {
    ssa_name_gen: VariableNameGenerator,
    stmts: Vec<Rc<Statement>>,
}

impl LirExpressionEmitter {
    pub fn new() -> Self {
        Self::default()
    }
}

impl LirExpressionEmitter {
    /// If the variable is `None`, returns
    pub fn emit_into_variable(
        &mut self,
        expr: &CheckedExpression,
        variable: Option<LirVariable>,
    ) -> (Vec<Rc<Statement>>, String) {
        let name = self.ssa_name_gen.generate();

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

        self.stmts.push(Rc::new(ssa));

        (self.stmts.clone(), var_name)
    }

    pub fn emit(&mut self, expr: &CheckedExpression) -> Vec<Rc<Statement>> {
        self.lower_expr(expr, None);

        self.stmts.clone()
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
