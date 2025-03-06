use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use common::Type;
use lir::{
    expression::{Expression, StaticExpression},
    operator::{BinaryOperator, UnaryOperator},
    statement::Statement,
};
use tcast::{
    expression::{Expression as CheckedExpression, ExpressionKind},
    statement::{Statement as CheckedStatement, StatementKind},
};

use crate::builder::FunctionBuilder;

use super::{function::LirFunctionEmitterScope, module::LirModuleEmitterScope};
use crate::variable_ref::VariableRef;

pub struct LoweringResult {
    pub(crate) var: VariableRef,
    pub(crate) ty: Type,
}

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
    mod_scope: Rc<LirModuleEmitterScope>,
    cx: RefCell<LirExpressionContext>,
}

impl LirExpressionEmitter {
    pub fn new(
        mod_scope: Rc<LirModuleEmitterScope>,
        scope: Weak<LirFunctionEmitterScope>,
        builder: Rc<FunctionBuilder>,
    ) -> Self {
        Self {
            scope: RefCell::new(scope),
            builder,
            mod_scope,
            ..Default::default()
        }
    }

    pub(crate) fn update_scope(&self, scope: Weak<LirFunctionEmitterScope>) {
        self.scope.replace(scope);
    }
}

impl LirExpressionEmitter {
    pub fn emit(&self, expr: &CheckedExpression) -> LoweringResult {
        self.lower_expr(expr)
    }

    fn lower_expr(&self, expr: &CheckedExpression) -> LoweringResult {
        match expr.expr.as_ref() {
            ExpressionKind::Literal(literal) => {
                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id,
                        expr: Rc::new(Expression::Static(
                            Rc::new(StaticExpression::Literal(Rc::clone(literal))),
                            expr.ty.clone(),
                        )),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Self_ => {
                todo!()
            }
            ExpressionKind::StructInit { name, fields } => {
                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id,
                        expr: Rc::new(Expression::StructInit {
                            name: name.to_string(),
                        }),
                        ty: expr.ty.clone(),
                    }));

                for (field_idx, field_expr) in fields.iter().enumerate() {
                    let ma_id = self
                        .scope
                        .borrow()
                        .upgrade()
                        .unwrap()
                        .name_gen
                        .borrow_mut()
                        .generate();

                    self.builder
                        .push(Rc::new(Statement::StaticVariableDeclaration {
                            id: ma_id,
                            expr: Rc::new(Expression::MemberAccess {
                                expr: Rc::new(StaticExpression::Identifier(id.to_string())),
                                id: field_idx as u64,
                                ty: field_expr.ty.clone(),
                            }),
                            ty: field_expr.ty.clone(),
                        }));

                    let lowered_expr = self.lower_expr(&field_expr);
                    let lowered_expr = match lowered_expr.var {
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
                                    ty: lowered_expr.ty,
                                }));
                            load_id.to_string()
                        }
                        _ => unreachable!(),
                    };

                    self.builder.push(Rc::new(Statement::Store {
                        id: ma_id.to_string(),
                        value: Rc::new(StaticExpression::Identifier(lowered_expr)),
                    }));
                }

                LoweringResult {
                    var: VariableRef::Direct(id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Block(stmts) => self.lower_block(stmts),
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_result = self.lower_expr(left);
                let right_result = self.lower_expr(right);

                // Load values if they're pointers
                let left_val = match left_result.var {
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
                                    ty: left.ty.clone(),
                                }),
                                ty: left_result.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let right_val = match right_result.var {
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
                                    ty: right.ty.clone(),
                                }),
                                ty: right_result.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let result_id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id: result_id,
                        expr: Rc::new(Expression::Binary {
                            left: StaticExpression::Identifier(left_val.to_string()),
                            operator: BinaryOperator::from(operator),
                            right: StaticExpression::Identifier(right_val.to_string()),
                            operand_ty: left.ty.clone(),
                            ty: expr.ty.clone(),
                        }),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(result_id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Identifier(ident) => {
                let (id, is_mut) = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .environment
                    .borrow()
                    .get(ident)
                    .map(|(id, is_mut)| (id.to_string(), is_mut))
                    .or_else(|| {
                        if self.mod_scope.has_global(ident) {
                            Some((ident.to_string(), false))
                        } else {
                            None
                        }
                    })
                    .unwrap();

                if is_mut {
                    LoweringResult {
                        var: VariableRef::Pointer(id),
                        ty: expr.ty.clone(),
                    }
                } else {
                    LoweringResult {
                        var: VariableRef::Direct(id),
                        ty: expr.ty.clone(),
                    }
                }
            }
            ExpressionKind::Unary {
                operator,
                expr: sub_expr,
            } => {
                let low_sub_expr = self.lower_expr(sub_expr);

                let val = match low_sub_expr.var {
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
                                    ty: sub_expr.ty.clone(),
                                }),
                                ty: low_sub_expr.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let result_id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id: result_id,
                        expr: Rc::new(Expression::Unary {
                            expr: StaticExpression::Identifier(val.to_string()),
                            operator: UnaryOperator::from(operator),
                            ty: expr.ty.clone(),
                        }),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(result_id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Grouping(sub_expr) => {
                let low_sub_expr = self.lower_expr(sub_expr);

                LoweringResult {
                    var: low_sub_expr.var,
                    ty: sub_expr.ty.clone(),
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

                let fn_ident = self.mod_scope.get(fn_ident).unwrap().to_string();

                let mut arg_vals = Vec::new();
                for arg in arguments {
                    let arg_result = self.lower_expr(arg);
                    let arg_val = match arg_result.var {
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
                                        expr: StaticExpression::Identifier(ptr),
                                        ty: sub_expr.ty.clone(),
                                    }),
                                    ty: arg_result.ty.clone(),
                                }));
                            load_id.to_string()
                        }
                        _ => unreachable!(),
                    };
                    arg_vals.push(Rc::new(Expression::Static(
                        Rc::new(StaticExpression::Identifier(arg_val.to_string())),
                        arg_result.ty,
                    )));
                }

                let result_id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id: result_id,
                        expr: Rc::new(Expression::Call {
                            expr: Rc::new(Expression::Static(
                                Rc::new(StaticExpression::FnIdentifier(fn_ident)),
                                sub_expr.ty.clone(),
                            )),
                            arguments: arg_vals,
                            ty: expr.ty.clone(),
                        }),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(result_id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Cast { expr, ty } => {
                let low_sub_expr = self.lower_expr(expr);

                let val = match low_sub_expr.var {
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
                                ty: low_sub_expr.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let lir_expr = if ty.is_ptr() || expr.ty == *ty {
                    Expression::Static(Rc::new(StaticExpression::Identifier(val)), ty.clone())
                } else {
                    if expr.ty.numeric_bits().unwrap() > ty.numeric_bits().unwrap() {
                        Expression::Trunc {
                            expr: Rc::new(StaticExpression::Identifier(val)),
                            ty: ty.clone(),
                        }
                    } else {
                        Expression::Ext {
                            expr: Rc::new(StaticExpression::Identifier(val)),
                            ty: ty.clone(),
                        }
                    }
                };

                let result_id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id: result_id,
                        expr: Rc::new(lir_expr),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(result_id.to_string()),
                    ty: expr.ty.clone(),
                }
            }
            ExpressionKind::Conditional {
                condition,
                body,
                alternative,
            } => {
                let low_codition = self.lower_expr(condition);

                let condition_val = match low_codition.var {
                    VariableRef::Direct(id) | VariableRef::Pointer(id) => id,
                    _ => unreachable!(),
                };

                let return_id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                let entry_block_id = self.builder.current_block_id();
                let body_id = self.builder.append_block();
                let body_return_val = self.lower_block(body);
                let body_return_val = if !body_return_val.var.is_none() {
                    match body_return_val.var {
                        VariableRef::Direct(id) | VariableRef::Pointer(id) => Some(id),
                        _ => unreachable!(),
                    }
                } else {
                    None
                };
                if let Some(body_return_val) = body_return_val {
                    self.builder
                        .push(Rc::new(Statement::StaticVariableDeclaration {
                            id: return_id,
                            expr: Rc::new(Expression::Static(
                                Rc::new(StaticExpression::Identifier(body_return_val)),
                                expr.ty.clone(),
                            )),
                            ty: expr.ty.clone(),
                        }));
                }

                let alternative = if let Some(stmts) = alternative {
                    let alternative_id = self.builder.append_block();
                    let alternative_return_val = self.lower_expr(stmts);
                    Some((alternative_id, alternative_return_val))
                } else {
                    None
                };

                let after_conditional_block_id = self.builder.append_block();

                self.builder.position_at_end(body_id);
                self.builder
                    .push(Rc::new(Statement::Goto(after_conditional_block_id)));

                let alternative_return_val = if let Some((alt_id, alt_val)) = alternative.as_ref() {
                    self.builder.position_at_end(*alt_id);
                    self.builder
                        .push(Rc::new(Statement::Goto(after_conditional_block_id)));
                    Some(alt_val)
                } else {
                    None
                };

                let alternative_return_val =
                    if let Some(alternative_return_val) = alternative_return_val {
                        match alternative_return_val.var {
                            VariableRef::Direct(ref id) | VariableRef::Pointer(ref id) => {
                                Some(id.clone())
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        None
                    };
                if let Some(alternative_return_val) = alternative_return_val {
                    self.builder
                        .push(Rc::new(Statement::StaticVariableDeclaration {
                            id: return_id,
                            expr: Rc::new(Expression::Static(
                                Rc::new(StaticExpression::Identifier(alternative_return_val)),
                                expr.ty.clone(),
                            )),
                            ty: expr.ty.clone(),
                        }));
                }

                let branch = Statement::Branch {
                    condition: Rc::new(StaticExpression::Identifier(condition_val)),
                    then: body_id,
                    alternative: alternative
                        .map(|(id, _)| id)
                        .unwrap_or(after_conditional_block_id),
                };
                self.builder.position_at_end(entry_block_id);
                self.builder.push(Rc::new(branch));

                self.builder.position_at_end(after_conditional_block_id);

                LoweringResult {
                    var: VariableRef::Direct(return_id.to_string()),
                    ty: expr.ty.clone(),
                }
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

                self.lower_block(stmts);

                self.builder.push(Rc::new(Statement::Goto(loop_block_id)));

                self.cx.replace(prev_cx);

                self.builder.position_at_end(exit_block_id);

                LoweringResult {
                    var: VariableRef::None,
                    ty: Type::Unit,
                }
            }
            ExpressionKind::Break => {
                let exit_block_id = match &*self.cx.borrow() {
                    LirExpressionContext::Loop { exit_id, .. } => *exit_id,
                    _ => unreachable!(),
                };
                let goto = Statement::Goto(exit_block_id);
                self.builder.push(Rc::new(goto));

                LoweringResult {
                    var: VariableRef::None,
                    ty: Type::Unit,
                }
            }
            ExpressionKind::Continue => {
                let loop_block_id = match &*self.cx.borrow() {
                    LirExpressionContext::Loop { loop_id, .. } => *loop_id,
                    _ => unreachable!(),
                };
                let goto = Statement::Goto(loop_block_id);
                self.builder.push(Rc::new(goto));

                LoweringResult {
                    var: VariableRef::None,
                    ty: Type::Unit,
                }
            }
            ExpressionKind::Assignment { assignee, expr } => {
                let assignee = self.lower_expr(assignee);
                let value = self.lower_expr(expr);

                let val = match value.var {
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
                                ty: value.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let store = Statement::Store {
                    id: match assignee.var {
                        VariableRef::Direct(id) | VariableRef::Pointer(id) => id,
                        _ => unreachable!(),
                    },
                    value: Rc::new(StaticExpression::Identifier(val)),
                };
                self.builder.push(Rc::new(store));

                LoweringResult {
                    var: VariableRef::None,
                    ty: Type::Unit,
                }
            }
            ExpressionKind::MemberAccess { expr, ident } => {
                let lowered_expr = self.lower_expr(expr);

                let val = match lowered_expr.var {
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
                                ty: lowered_expr.ty,
                            }));
                        load_id.to_string()
                    }
                    _ => unreachable!(),
                };

                let id = self
                    .scope
                    .borrow()
                    .upgrade()
                    .unwrap()
                    .name_gen
                    .borrow_mut()
                    .generate();

                let expr_fields = match &expr.ty {
                    Type::Struct { fields, .. } => fields,
                    _ => unreachable!(),
                };

                self.builder
                    .push(Rc::new(Statement::StaticVariableDeclaration {
                        id,
                        expr: Rc::new(Expression::MemberAccess {
                            expr: Rc::new(StaticExpression::Identifier(val)),
                            id: expr_fields
                                .iter()
                                .position(|(name, _)| name == ident)
                                .unwrap() as u64,
                            ty: expr.ty.clone(),
                        }),
                        ty: expr.ty.clone(),
                    }));

                LoweringResult {
                    var: VariableRef::Direct(id.to_string()),
                    ty: expr_fields
                        .iter()
                        .find(|(name, _)| name == ident)
                        .map(|(_, ty)| ty)
                        .unwrap()
                        .clone(),
                }
            }
        }
    }

    fn lower_block(&self, stmts: &Vec<Rc<CheckedStatement>>) -> LoweringResult {
        let stmt_emitter = &self.scope.borrow().upgrade().unwrap().stmt_emitter;

        if stmts.len() > 1 {
            for stmt in stmts[..(stmts.len() - 1)].iter() {
                stmt_emitter.emit(stmt);
            }
        }

        if let Some(last) = stmts.last() {
            match last.stmt.as_ref() {
                StatementKind::Expression { expr, end_semi } if !*end_semi => self.lower_expr(expr),
                _ => {
                    stmt_emitter.emit(last);
                    LoweringResult {
                        var: VariableRef::None,
                        ty: Type::Unit,
                    }
                }
            }
        } else {
            LoweringResult {
                var: VariableRef::None,
                ty: Type::Unit,
            }
        }
    }
}
