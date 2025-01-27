use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use common::{Literal, Type};
use lir::{expression::StaticExpression, statement::Statement};
use tcast::expression::Expression as CheckedExpression;

use crate::{
    builder::FunctionBuilder, environment::Environment, var_name_gen::VariableNameGenerator,
};

use super::{expression::LirExpressionEmitter, statement::LirStatementEmitter};

#[derive(Debug, Default)]
pub struct LirFunctionEmitterScope {
    pub(crate) expr_emitter: LirExpressionEmitter,
    pub(crate) stmt_emitter: LirStatementEmitter,
    pub(crate) name_gen: RefCell<VariableNameGenerator>,
    pub(crate) environment: RefCell<Environment>,
}

#[derive(Debug, Default)]
pub struct LirFunctionEmitter {
    scope: Rc<LirFunctionEmitterScope>,
    builder: Rc<FunctionBuilder>,
}

impl LirFunctionEmitter {
    pub fn new() -> Self {
        let builder = FunctionBuilder::new();
        let builder = Rc::new(builder);

        let expr_emitter = LirExpressionEmitter::new(Weak::new(), Rc::clone(&builder));
        let stmt_emitter = LirStatementEmitter::new(Weak::new(), Rc::clone(&builder));

        let scope = LirFunctionEmitterScope {
            expr_emitter,
            stmt_emitter,
            name_gen: RefCell::new(VariableNameGenerator::new()),
            environment: RefCell::default(),
        };

        let scope = Rc::new(scope);

        scope.expr_emitter.update_scope(Rc::downgrade(&scope));
        scope.stmt_emitter.update_scope(Rc::downgrade(&scope));

        Self { scope, builder }
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

        let parameters = parameters
            .into_iter()
            .map(|(name, ty)| {
                let id = self.scope.name_gen.borrow_mut().generate();
                self.scope.environment.borrow_mut().define(name, id);

                (id.to_string(), ty)
            })
            .collect();

        let block = block.unwrap();
        self.builder.append_block();

        let produces_value = block.ty != Type::Unit;

        let ssa_name = if produces_value {
            let name = self.scope.expr_emitter.emit_into_variable(&block, None);
            Some(name)
        } else {
            self.scope.expr_emitter.emit(&block);
            None
        };

        let ret = if let Some(ssa_name) = ssa_name {
            Statement::Return(Rc::new(StaticExpression::Identifier(ssa_name)))
        } else {
            Statement::Return(Rc::new(StaticExpression::Literal(Rc::new(Literal::Unit))))
        };

        self.builder.push(Rc::new(ret));

        Statement::FunctionDeclaration {
            name,
            blocks: self.builder.into_blocks(),
            return_type,
            parameters,
        }
    }
}
