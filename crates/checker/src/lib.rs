pub mod environment;

use std::{cell::RefCell, mem, rc::Rc};

use ast::{
    annotation::Annotation,
    expression::Expression,
    literal::{Type, Typed},
    statement::Statement,
};
use environment::Environment;
use iast::{
    effect::Effect,
    expression::{Expression as CheckedExpression, ExpressionKind},
    statement::{Statement as CheckedStatement, StatementKind},
};

fn type_cmp(ty: &Type, expect: &Type, exact: bool) -> bool {
    if exact {
        ty == expect
    } else {
        mem::discriminant(ty) == mem::discriminant(expect)
    }
}

pub struct Checker {
    ast: Vec<Rc<Statement>>,
    environment: RefCell<Rc<Environment>>,
}

impl Checker {
    pub fn new(ast: Vec<Statement>) -> Self {
        Self {
            ast: ast.into_iter().map(Rc::new).collect(),
            environment: RefCell::new(Rc::new(Environment::new())),
        }
    }
}

impl Checker {
    pub fn check(&mut self) -> Vec<CheckedStatement> {
        let mut statements = vec![];

        for idx in 0..self.ast.len() {
            statements.push(self.statement(Rc::clone(&self.ast[idx]), None));
        }

        statements
    }

    fn statement(
        &mut self,
        statement: Rc<Statement>,
        annotations: Option<Vec<Annotation>>,
    ) -> CheckedStatement {
        match statement.as_ref() {
            Statement::Annotated { annotations, stmt } => {
                self.statement(Rc::clone(stmt), Some(annotations.clone()))
            }
            Statement::FunctionDeclaration {
                block,
                name,
                parameters,
                return_type,
            } => self.fun_decl(
                name.clone(),
                block.as_ref().map(Rc::clone),
                parameters,
                return_type.clone(),
                annotations,
            ),
            Statement::VariableDeclaration { expr, name, ty } => {
                self.var_decl(name.to_string(), expr, ty.clone())
            }
            Statement::Expression { expr, end_semi } => {
                self.expression_stmt(Rc::clone(expr), *end_semi)
            }
        }
    }

    fn fun_decl(
        &mut self,
        name: String,
        block: Option<Rc<Expression>>,
        parameters: &Vec<(String, Type)>,
        return_type: Type,
        annotations: Option<Vec<Annotation>>,
    ) -> CheckedStatement {
        let expr = if let Some(expr) = block {
            let fn_env = Environment::with_enclosing(Rc::clone(&*self.environment.borrow()));
            for (param_name, param_type) in parameters {
                fn_env.declare(param_name.clone(), param_type.clone(), vec![]);
            }

            let prev_env = self.environment.replace(Rc::new(fn_env));
            let checked = Some(self.expression(expr, Some(return_type.clone()), true));

            self.environment.replace(prev_env);

            checked
        } else {
            None
        };

        let fn_type = Type::Callable {
            parameters: parameters.iter().map(|(_, ty)| ty.clone()).collect(),
            return_type: Box::new(return_type.clone()),
        };

        let fn_effects = annotations
            .map(|annotations| annotations.into_iter().find(|a| a.name == "effect"))
            .unwrap_or(None)
            .map(|annotation| {
                annotation
                    .arguments
                    .iter()
                    .map(|s| s.as_str())
                    .map(Effect::try_from)
                    .map(Result::unwrap)
                    .collect::<Vec<Effect>>()
            })
            .unwrap_or(vec![]);

        if self
            .environment
            .borrow()
            .declare(name.clone(), fn_type, fn_effects.clone())
        {
            panic!("[function declaration] function `{name}` already exists.");
        }

        if let Some(ref expr) = expr {
            for effect in expr.effects.iter() {
                if !fn_effects.contains(effect) {
                    panic!(
                        "[effect] effect `{}` is not specified inside `@effect` annotation.",
                        effect
                    );
                }
            }
        }

        CheckedStatement {
            effects: fn_effects,
            stmt: Rc::new(StatementKind::FunctionDeclaration {
                name,
                block: expr,
                return_type,
                parameters: parameters.clone(),
            }),
        }
    }

    fn var_decl(
        &mut self,
        name: String,
        expr: &Rc<Expression>,
        ty: Option<Type>,
    ) -> CheckedStatement {
        let checked = self.expression(Rc::clone(expr), ty.clone(), true);

        self.environment.borrow().declare(
            name.clone(),
            ty.unwrap_or(checked.ty.clone()),
            checked.effects.clone(),
        );

        CheckedStatement {
            effects: checked.effects.clone(),
            stmt: Rc::new(StatementKind::VariableDeclaration {
                name,
                ty: checked.ty.clone(),
                expr: checked,
            }),
        }
    }

    fn expression_stmt(&mut self, expr: Rc<Expression>, end_semi: bool) -> CheckedStatement {
        let expr = self.expression(expr, None, false);

        CheckedStatement {
            effects: expr.effects.clone(),
            stmt: Rc::new(StatementKind::Expression { expr, end_semi }),
        }
    }

    fn expression(
        &mut self,
        expr: Rc<Expression>,
        expect_type: Option<Type>,
        exact: bool,
    ) -> CheckedExpression {
        match expr.as_ref() {
            Expression::Break => CheckedExpression {
                expr: Rc::new(ExpressionKind::Break),
                effects: vec![],
                ty: Type::Unit,
            },
            Expression::Continue => CheckedExpression {
                expr: Rc::new(ExpressionKind::Continue),
                effects: vec![],
                ty: Type::Unit,
            },
            Expression::Literal(literal) => CheckedExpression {
                effects: vec![],
                ty: literal.get_type(),
                expr: Rc::new(ExpressionKind::Literal(Rc::clone(literal))),
            },
            Expression::Unary {
                expr: sub_expr,
                operator,
            } => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type.clone(), exact);
                if let Some(expect_type) = expect_type {
                    if !type_cmp(&checked.ty, &expect_type, exact) {
                        panic!(
                            "[unary expression] expected type mismatch: `{:?}`, expected `{:?}`",
                            checked.ty, expect_type
                        )
                    }
                }

                CheckedExpression {
                    effects: checked.effects.clone(),
                    ty: checked.ty.clone(),
                    expr: Rc::new(ExpressionKind::Unary {
                        operator: *operator,
                        expr: Rc::new(checked),
                    }),
                }
            }
            Expression::Binary {
                left,
                right,
                operator,
            } => {
                let left_checked = self.expression(Rc::clone(left), expect_type.clone(), exact);
                let right_checked = self.expression(Rc::clone(right), expect_type.clone(), exact);

                if let Some(expect_type) = expect_type {
                    if !type_cmp(&left_checked.ty, &expect_type, exact)
                        || !type_cmp(&right_checked.ty, &expect_type, exact)
                    {
                        panic!("[binary expression] expected type mismatch")
                    }
                }
                if right_checked.ty != left_checked.ty {
                    panic!("[binary expression] lhs and rhs dont have same types")
                }

                CheckedExpression {
                    effects: [left_checked.effects.clone(), right_checked.effects.clone()].concat(),
                    ty: left_checked.ty.clone(),
                    expr: Rc::new(ExpressionKind::Binary {
                        left: Rc::new(left_checked),
                        operator: *operator,
                        right: Rc::new(right_checked),
                    }),
                }
            }
            Expression::Grouping(sub_expr) => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type.clone(), exact);

                if let Some(expect_type) = expect_type {
                    if !type_cmp(&checked.ty, &expect_type, exact) {
                        panic!("[grouping expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: checked.effects.clone(),
                    ty: checked.ty.clone(),
                    expr: Rc::new(ExpressionKind::Grouping(Rc::new(checked))),
                }
            }
            Expression::MemberAccess { .. } => todo!("member access"),
            Expression::Identifier(ident) => {
                let decl = self
                    .environment
                    .borrow()
                    .get(ident)
                    .expect("[identifier expression] identifier not found");
                if let Some(expect_type) = expect_type {
                    if !type_cmp(&decl.ty, &expect_type, exact) {
                        panic!(
                          "[identifier expression] expected type mismatch: `{:?}`, expected `{:?}`",
                          decl.ty, expect_type
                      )
                    }
                }

                CheckedExpression {
                    effects: decl.effects,
                    ty: decl.ty,
                    expr: Rc::new(ExpressionKind::Identifier(ident.to_string())),
                }
            }
            Expression::Assignment {
                identifier,
                expr: sub_expr,
            } => {
                panic!("assignment is not implemented yet");

                let decl = self
                    .environment
                    .borrow()
                    .get(identifier)
                    .expect("[assignment expression] identifier not found");

                if let Some(ref expect_type) = expect_type {
                    if expect_type != &Type::Unit {
                        panic!("[assignment expression] assignment only produces Unit type as a result");
                    }
                }

                let checked = self.expression(Rc::clone(sub_expr), expect_type, exact);

                if decl.ty != checked.ty {
                    panic!("[cast expression] expected type mismatch")
                }

                CheckedExpression {
                    effects: checked.effects.clone(),
                    ty: decl.ty,
                    expr: Rc::new(ExpressionKind::Assignment {
                        identifier: identifier.to_string(),
                        expr: Rc::new(checked),
                    }),
                }
            }
            Expression::Call {
                expr: sub_expr,
                arguments,
            } => {
                let checked = self.expression(
                    Rc::clone(sub_expr),
                    Some(Type::Callable {
                        parameters: vec![],
                        return_type: Box::new(Type::Unit),
                    }),
                    false,
                );
                let mut effects = checked.effects.clone();

                let (return_type, arguments) = match checked.ty.clone() {
                    Type::Callable {
                        parameters,
                        return_type,
                    } => {
                        if arguments.len() != parameters.len() {
                            panic!(
                                "[call expression] wrong number of arguments: {}, expected {}",
                                arguments.len(),
                                parameters.len()
                            )
                        }

                        let arguments = arguments
                            .iter()
                            .map(Rc::clone)
                            .enumerate()
                            .map(|(idx, arg)| {
                                self.expression(arg, Some(parameters[idx].clone()), true)
                            })
                            .collect::<Vec<CheckedExpression>>();

                        for idx in 0..parameters.len() {
                            for effect in arguments[idx].effects.iter() {
                                if !effects.contains(effect) {
                                    effects.push(*effect)
                                }
                            }
                        }

                        (return_type, arguments)
                    }
                    _ => panic!("[call expression] expression is not callable"),
                };

                if let Some(ref expect_type) = expect_type {
                    if !type_cmp(&return_type, &expect_type, exact) {
                        panic!("[call expression] callable return type mismatch: `{:?}`, expected `{:?}`", return_type, expect_type);
                    }
                }

                CheckedExpression {
                    effects,
                    ty: (&*return_type).clone(),
                    expr: Rc::new(ExpressionKind::Call {
                        expr: Rc::new(checked),
                        arguments,
                    }),
                }
            }
            Expression::Cast { expr: sub_expr, ty } => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type.clone(), exact);

                if let Some(ref expect_type) = expect_type {
                    if !type_cmp(&ty, &expect_type, exact) {
                        panic!("[cast expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: checked.effects.clone(),
                    ty: ty.clone(),
                    expr: Rc::new(ExpressionKind::Cast {
                        expr: Rc::new(checked),
                        ty: ty.clone(),
                    }),
                }
            }
            Expression::Block(stmts) => {
                let (stmts, effects, ty) = self.block_expr(stmts, expect_type);

                CheckedExpression {
                    effects,
                    expr: Rc::new(ExpressionKind::Block(stmts)),
                    ty: ty.unwrap_or(Type::Unit),
                }
            }
            Expression::Loop(stmts) => {
                let (stmts, effects, _) = self.block_expr(stmts, expect_type);

                CheckedExpression {
                    effects,
                    expr: Rc::new(ExpressionKind::Loop(stmts)),
                    ty: Type::Unit,
                }
            }
            Expression::Conditional {
                condition,
                body,
                alternative,
            } => {
                let condition_checked =
                    self.expression(Rc::clone(condition), Some(Type::Bool), true);
                let (stmts, body_effects, body_ty) = self.block_expr(body, expect_type.clone());
                let alternative_checked = alternative
                    .as_ref()
                    .map(|alt| self.expression(Rc::clone(alt), expect_type, true));

                if body_ty.as_ref() != alternative_checked.as_ref().map(|alt| &alt.ty) {
                    panic!("[conditional expression] branch type mismatch")
                }

                CheckedExpression {
                    effects: [
                        condition_checked.effects.clone(),
                        body_effects,
                        alternative_checked
                            .clone()
                            .map(|alt| alt.effects)
                            .unwrap_or(vec![]),
                    ]
                    .concat(),
                    expr: Rc::new(ExpressionKind::Conditional {
                        condition: Rc::new(condition_checked),
                        body: stmts,
                        alternative: alternative_checked.map(Rc::new),
                    }),
                    ty: body_ty.unwrap_or(Type::Unit),
                }
            }
        }
    }

    fn block_expr(
        &mut self,
        stmts: &Vec<Rc<Statement>>,
        expect_type: Option<Type>,
    ) -> (Vec<Rc<CheckedStatement>>, Vec<Effect>, Option<Type>) {
        let environment = Rc::new(Environment::with_enclosing(Rc::clone(
            &self.environment.borrow(),
        )));

        let prev_environment = self.environment.replace(environment);

        let mut checked_stmts = vec![];
        let mut effects = vec![];

        if stmts.len() > 1 {
            for idx in 0..(stmts.len() - 1) {
                let checked = self.statement(Rc::clone(&stmts[idx]), None);
                effects.append(&mut checked.effects.clone());
                checked_stmts.push(Rc::new(checked))
            }
        }

        let ty = if let Some(stmt) = stmts.last() {
            let checked = self.statement(Rc::clone(stmt), None);

            let ty = match checked.stmt.as_ref() {
                StatementKind::Expression { expr, end_semi } if *end_semi == false => {
                    if let Some(expect_type) = expect_type {
                        if expr.ty != expect_type {
                            panic!("[block] expected type mismatch")
                        }
                    }

                    Some(expr.ty.clone())
                }
                _ => None,
            };

            effects.append(&mut checked.effects.clone());
            checked_stmts.push(Rc::new(checked));

            ty
        } else {
            None
        };

        self.environment.replace(prev_environment);

        (checked_stmts, effects, ty)
    }
}
