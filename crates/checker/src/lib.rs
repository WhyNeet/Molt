use std::rc::Rc;

use ast::{
    annotation::Annotation,
    expression::Expression,
    literal::{Type, Typed},
    statement::Statement,
};
use iast::{
    effect::Effect,
    expression::Expression as CheckedExpression,
    statement::{Statement as CheckedStatement, StatementKind},
};

pub struct Checker {
    ast: Vec<Statement>,
}

impl Checker {
    pub fn new(ast: Vec<Statement>) -> Self {
        Self { ast }
    }
}

impl Checker {
    pub fn check(self) -> Vec<CheckedStatement> {
        let mut statements = vec![];

        for statement in self.ast.into_iter() {
            statements.push(Checker::statement(statement.into(), None));
        }

        statements
    }

    fn statement(
        statement: Rc<Statement>,
        annotations: Option<Vec<Annotation>>,
    ) -> CheckedStatement {
        match statement.as_ref() {
            Statement::Annotated { annotations, stmt } => {
                Checker::statement(Rc::clone(stmt), Some(annotations.clone()))
            }
            Statement::FunctionDeclaration {
                block,
                name,
                parameters,
                return_type,
            } => Checker::fun_decl(
                name.clone(),
                block.as_ref().map(Rc::clone),
                parameters,
                *return_type,
            ),
            Statement::VariableDeclaration { expr, name, ty } => {
                Checker::var_decl(name.to_string(), expr, *ty)
            }
            Statement::Expression { expr, end_semi } => {
                Checker::expression_stmt(Rc::clone(expr), *end_semi)
            }
        }
    }

    fn fun_decl(
        name: String,
        block: Option<Rc<Expression>>,
        parameters: &Vec<(String, Type)>,
        return_type: Type,
    ) -> CheckedStatement {
        todo!()
    }

    fn var_decl(name: String, expr: &Expression, ty: Option<Type>) -> CheckedStatement {
        todo!()
    }

    fn expression_stmt(expr: Rc<Expression>, end_semi: bool) -> CheckedStatement {
        let expr = Checker::expression(expr, None);

        CheckedStatement {
            effects: expr.effects.clone(),
            stmt: Rc::new(StatementKind::Expression { expr, end_semi }),
        }
    }

    fn expression(expr: Rc<Expression>, expect_type: Option<Type>) -> CheckedExpression {
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
                let checked = Checker::expression(Rc::clone(sub_expr), expect_type);
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
                let left_checked = Checker::expression(Rc::clone(left), expect_type);
                let right_checked = Checker::expression(Rc::clone(right), expect_type);

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
                let checked = Checker::expression(Rc::clone(sub_expr), expect_type);

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
            Expression::MemberAccess { expr, ident } => todo!(),
            Expression::Identifier(ident) => todo!(),
            Expression::Cast { expr: sub_expr, ty } => {
                let checked = Checker::expression(Rc::clone(sub_expr), expect_type);

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
                let (effects, ty) = Checker::block_expr(stmts, expect_type);

                CheckedExpression {
                    effects,
                    expr,
                    ty: ty.unwrap_or(Type::Unit),
                }
            }
            Expression::Loop(stmts) => {
                let (effects, _) = Checker::block_expr(stmts, expect_type);

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
                let condition_checked = Checker::expression(Rc::clone(condition), expect_type);
                let (body_effects, body_ty) = Checker::block_expr(body, expect_type);
                let alternative_checked = alternative
                    .as_ref()
                    .map(|alt| Checker::expression(Rc::clone(alt), expect_type));

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
            _ => todo!(),
        }
    }

    fn block_expr(
        stmts: &Vec<Rc<Statement>>,
        expect_type: Option<Type>,
    ) -> (Vec<Effect>, Option<Type>) {
        let mut effects = vec![];

        if stmts.len() > 1 {
            for idx in 0..(stmts.len() - 1) {
                let mut checked = Checker::statement(Rc::clone(&stmts[idx]), None);
                effects.append(&mut checked.effects);
            }
        }

        let ty = if let Some(stmt) = stmts.last() {
            let mut checked = Checker::statement(Rc::clone(stmt), None);

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

        (effects, ty)
    }
}
