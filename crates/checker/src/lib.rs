pub mod environment;

use std::{cell::RefCell, rc::Rc};

use ast::{
    annotation::Annotation,
    expression::Expression,
    literal::{Type, Typed},
    statement::Statement,
};
use environment::Environment;
use iast::{
    effect::Effect,
    expression::Expression as CheckedExpression,
    statement::{Statement as CheckedStatement, StatementKind},
};

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
                *return_type,
            ),
            Statement::VariableDeclaration { expr, name, ty } => {
                self.var_decl(name.to_string(), expr, *ty)
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
    ) -> CheckedStatement {
        todo!()
    }

    fn var_decl(
        &mut self,
        name: String,
        expr: &Rc<Expression>,
        ty: Option<Type>,
    ) -> CheckedStatement {
        let checked = self.expression(Rc::clone(expr), ty);

        self.environment
            .borrow()
            .declare(name.clone(), ty.unwrap_or(checked.ty));

        CheckedStatement {
            effects: checked.effects.clone(),
            stmt: Rc::new(StatementKind::VariableDeclaration {
                name,
                ty: Some(checked.ty),
                expr: checked,
            }),
        }
    }

    fn expression_stmt(&mut self, expr: Rc<Expression>, end_semi: bool) -> CheckedStatement {
        let expr = self.expression(expr, None);

        CheckedStatement {
            effects: expr.effects.clone(),
            stmt: Rc::new(StatementKind::Expression { expr, end_semi }),
        }
    }

    fn expression(&mut self, expr: Rc<Expression>, expect_type: Option<Type>) -> CheckedExpression {
        match expr.as_ref() {
            Expression::Break | Expression::Continue => CheckedExpression {
                expr,
                effects: vec![],
                ty: Type::Unit,
            },
            Expression::Literal(literal) => CheckedExpression {
                effects: vec![],
                ty: literal.get_type(),
                expr,
            },
            Expression::Unary { expr: sub_expr, .. } => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type);
                if let Some(expect_type) = expect_type {
                    if checked.ty != expect_type {
                        panic!("[unary expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: checked.effects.clone(),
                    ty: checked.ty,
                    expr,
                }
            }
            Expression::Binary { left, right, .. } => {
                let left_checked = self.expression(Rc::clone(left), expect_type);
                let right_checked = self.expression(Rc::clone(right), expect_type);

                if let Some(expect_type) = expect_type {
                    if left_checked.ty != expect_type || right_checked.ty != expect_type {
                        panic!("[binary expression] expected type mismatch")
                    }
                }
                if right_checked.ty != left_checked.ty {
                    panic!("[binary expression] lhs and rhs dont have same types")
                }

                CheckedExpression {
                    effects: [left_checked.effects, right_checked.effects].concat(),
                    expr,
                    ty: left_checked.ty,
                }
            }
            Expression::Grouping(sub_expr) => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type);

                if let Some(expect_type) = expect_type {
                    if checked.ty != expect_type {
                        panic!("[grouping expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: checked.effects,
                    expr,
                    ty: checked.ty,
                }
            }
            Expression::MemberAccess { expr, ident } => todo!("member access"),
            Expression::Identifier(ident) => {
                let ty = self
                    .environment
                    .borrow()
                    .get(ident)
                    .expect("[identifier expression] identifier not found");
                if let Some(expect_type) = expect_type {
                    if ty != expect_type {
                        panic!("[identifier expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: vec![],
                    ty,
                    expr,
                }
            }
            Expression::Assignment {
                identifier,
                expr: sub_expr,
            } => {
                let ty = self
                    .environment
                    .borrow()
                    .get(identifier)
                    .expect("[assignment expression] identifier not found");

                if let Some(expect_type) = expect_type {
                    if expect_type != Type::Unit {
                        panic!("[assignment expression] assignment only produces Unit type as a result");
                    }
                }

                let checked = self.expression(Rc::clone(sub_expr), expect_type);

                if ty != checked.ty {
                    panic!("[cast expression] expected type mismatch")
                }

                CheckedExpression {
                    effects: checked.effects,
                    ty,
                    expr,
                }
            }
            Expression::Call { expr, arguments } => todo!("function type is not present yet"),
            Expression::Cast { expr: sub_expr, ty } => {
                let checked = self.expression(Rc::clone(sub_expr), expect_type);

                if let Some(expect_type) = expect_type {
                    if *ty != expect_type {
                        panic!("[cast expression] expected type mismatch")
                    }
                }

                CheckedExpression {
                    effects: checked.effects,
                    ty: *ty,
                    expr,
                }
            }
            Expression::Block(stmts) => {
                let (effects, ty) = self.block_expr(stmts, expect_type);

                CheckedExpression {
                    effects,
                    expr,
                    ty: ty.unwrap_or(Type::Unit),
                }
            }
            Expression::Loop(stmts) => {
                let (effects, _) = self.block_expr(stmts, expect_type);

                CheckedExpression {
                    effects,
                    expr,
                    ty: Type::Unit,
                }
            }
            Expression::Conditional {
                condition,
                body,
                alternative,
            } => {
                let condition_checked = self.expression(Rc::clone(condition), expect_type);
                let (body_effects, body_ty) = self.block_expr(body, expect_type);
                let alternative_checked = alternative
                    .as_ref()
                    .map(|alt| self.expression(Rc::clone(alt), expect_type));

                if body_ty != alternative_checked.as_ref().map(|alt| alt.ty) {
                    panic!("[conditional expression] branch type mismatch")
                }

                CheckedExpression {
                    effects: [
                        condition_checked.effects,
                        body_effects,
                        alternative_checked.map(|alt| alt.effects).unwrap_or(vec![]),
                    ]
                    .concat(),
                    expr,
                    ty: body_ty.unwrap_or(Type::Unit),
                }
            }
        }
    }

    fn block_expr(
        &mut self,
        stmts: &Vec<Rc<Statement>>,
        expect_type: Option<Type>,
    ) -> (Vec<Effect>, Option<Type>) {
        let environment = Rc::new(Environment::with_enclosing(Rc::clone(
            &self.environment.borrow(),
        )));

        let prev_environment = self.environment.replace(environment);

        let mut effects = vec![];

        if stmts.len() > 1 {
            for idx in 0..(stmts.len() - 1) {
                let mut checked = self.statement(Rc::clone(&stmts[idx]), None);
                effects.append(&mut checked.effects);
            }
        }

        let ty = if let Some(stmt) = stmts.last() {
            let mut checked = self.statement(Rc::clone(stmt), None);

            let ty = match checked.stmt.as_ref() {
                StatementKind::Expression { expr, end_semi } if *end_semi == false => {
                    if let Some(expect_type) = expect_type {
                        if expr.ty != expect_type {
                            panic!("[block] expected type mismatch")
                        }
                    }

                    Some(expr.ty)
                }
                _ => None,
            };

            effects.append(&mut checked.effects);

            ty
        } else {
            None
        };

        self.environment.replace(prev_environment);

        (effects, ty)
    }
}
