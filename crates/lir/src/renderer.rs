use std::rc::Rc;

use common::{Literal, Type};

use crate::{
    expression::{Expression, StaticExpression},
    module::LirModule,
    statement::Statement,
};

pub struct LirRenderer {
    scope: u64,
}

impl LirRenderer {
    pub fn new() -> Self {
        Self { scope: 0 }
    }

    pub fn render_to_string(&mut self, module: &LirModule) -> String {
        let mut f = Vec::new();

        write!(&mut f as &mut dyn std::io::Write, "=== module `main` ===\n").unwrap();

        for stmt in module.stmts() {
            self.render_stmt(&stmt, &mut f);
        }

        String::from_utf8(f).unwrap()
    }

    fn render_stmt(&mut self, stmt: &Statement, f: &mut impl std::io::Write) {
        match stmt {
            Statement::FunctionDeclaration {
                name,
                blocks,
                return_type,
                parameters,
            } => {
                write!(f, "fun {name}(").unwrap();
                if !parameters.is_empty() {
                    for (name, ty) in parameters[..(parameters.len() - 1)].iter() {
                        write!(f, "{name}: {ty}, ").unwrap();
                    }
                    let last = parameters.last().unwrap();
                    write!(f, "{}: {}", last.0, last.1).unwrap();
                }
                write!(f, ") -> {return_type} {{\n").unwrap();

                for block in blocks {
                    let (idx, block) = (block.0, block.1.borrow());
                    write!(f, "{idx}:\n").unwrap();
                    self.render_block(&block, f);
                }

                write!(f, "}}\n").unwrap();
            }
            Statement::ExternalFunctionDeclaration {
                name,
                return_type,
                parameters,
                is_var_args,
            } => {
                write!(
                    f,
                    "external {}fun {name}(",
                    if *is_var_args { "vararg " } else { "" }
                )
                .unwrap();
                self.render_parameters(parameters, f).unwrap();
                write!(f, ") -> {return_type};\n").unwrap();
            }
            Statement::Goto(id) => write!(f, "goto {id};").unwrap(),
            Statement::Return(expr) => {
                write!(f, "return ").unwrap();
                self.render_static_expression(expr, f);
                write!(f, ";").unwrap();
            }
            Statement::Branch {
                condition,
                then,
                alternative,
            } => {
                write!(f, "if ").unwrap();
                self.render_static_expression(condition, f);
                write!(f, " goto {then} else goto {alternative};").unwrap();
            }
            Statement::VariableDeclaration {
                name,
                expr,
                ty,
                is_mut,
            } => {
                write!(
                    f,
                    "let{} {name}: {ty} = ",
                    if *is_mut { " mut" } else { "" }
                )
                .unwrap();
                self.render_expression(expr, f);
                write!(f, ";").unwrap();
            }
            Statement::StaticVariableDeclaration { id, expr, ty } => {
                write!(f, "ssa {id}: {ty} = ").unwrap();
                self.render_expression(expr, f);
                write!(f, ";").unwrap();
            }
            Statement::GlobalVariableDeclaration { name, expr, ty } => {
                write!(f, "let {name}: {ty} = ").unwrap();
                self.render_static_expression(expr, f);
                write!(f, ";").unwrap();
            }
            Statement::Store { id, value } => {
                write!(f, "store ").unwrap();
                self.render_static_expression(value, f);
                write!(f, " in `{id}`;").unwrap();
            }
            Statement::StructDeclaration {
                name,
                fields,
                methods,
            } => {
                self.scope += 1;

                write!(f, "struct {name} {{\n").unwrap();
                for (name, ty) in fields {
                    write!(
                        f,
                        "{}{name}: {ty};\n",
                        (0..self.scope).map(|_| "  ").collect::<String>()
                    )
                    .unwrap();
                }

                write!(f, "\n").unwrap();

                for (name, decl) in methods {
                    write!(
                        f,
                        "{}fun {name}(",
                        (0..self.scope).map(|_| "  ").collect::<String>()
                    )
                    .unwrap();
                    self.render_parameters(&decl.parameters, f).unwrap();
                    write!(f, ") -> {} {{\n", decl.return_type).unwrap();
                    for block in decl.blocks.iter() {
                        let (idx, block) = (block.0, &*block.1.borrow());
                        write!(
                            f,
                            "{}{idx}:\n",
                            (0..self.scope).map(|_| "  ").collect::<String>()
                        )
                        .unwrap();
                        self.render_block(block, f);
                    }
                    write!(
                        f,
                        "{}}}\n",
                        (0..self.scope).map(|_| "  ").collect::<String>()
                    )
                    .unwrap();
                }

                self.scope -= 1;

                write!(f, "\n}}\n").unwrap();
            }
        }
    }

    fn render_parameters(
        &mut self,
        parameters: &Vec<(String, Type)>,
        f: &mut impl std::io::Write,
    ) -> std::io::Result<()> {
        if !parameters.is_empty() {
            for (name, ty) in parameters[..(parameters.len() - 1)].iter() {
                write!(f, "{name}: {ty}, ")?;
            }
            let last = parameters.last().unwrap();
            write!(f, "{}: {}", last.0, last.1)?;
        }

        Ok(())
    }

    fn render_block(&mut self, block: &Vec<Rc<Statement>>, f: &mut impl std::io::Write) {
        for stmt in block {
            self.scope += 1;
            write!(f, "{}", (0..self.scope).map(|_| "  ").collect::<String>()).unwrap();
            self.render_stmt(stmt, f);
            write!(f, "\n").unwrap();
            self.scope -= 1;
        }
    }

    fn render_expression(&mut self, expression: &Expression, f: &mut impl std::io::Write) {
        match expression {
            Expression::Static(expr, _) => self.render_static_expression(expr, f),
            Expression::Unary { operator, expr, .. } => {
                write!(f, "{operator}").unwrap();
                self.render_static_expression(expr, f);
            }
            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => {
                self.render_static_expression(left, f);
                write!(f, " {operator} ").unwrap();
                self.render_static_expression(right, f);
            }
            Expression::Call {
                expr, arguments, ..
            } => {
                self.render_expression(expr, f);
                write!(f, "(").unwrap();
                if !arguments.is_empty() {
                    for arg in arguments[..(arguments.len() - 1)].iter() {
                        self.render_expression(arg, f);
                        write!(f, ", ").unwrap();
                    }
                    let last = arguments.last().unwrap();
                    self.render_expression(last, f);
                }
                write!(f, ")").unwrap();
            }
            Expression::Ext { expr, ty } => {
                write!(f, "ext ").unwrap();
                self.render_static_expression(expr, f);
                write!(f, " to {ty}").unwrap();
            }
            Expression::Trunc { expr, ty } => {
                write!(f, "trunc ").unwrap();
                self.render_static_expression(expr, f);
                write!(f, " to {ty}").unwrap();
            }
            Expression::MemberAccess { expr, ident, .. } => {
                self.render_expression(expr, f);
                write!(f, ".{ident}").unwrap();
            }
        }
    }

    fn render_static_expression(
        &mut self,
        expression: &StaticExpression,
        f: &mut impl std::io::Write,
    ) {
        match expression {
            StaticExpression::Identifier(ident) | StaticExpression::FnIdentifier(ident) => {
                write!(f, "`{ident}`").unwrap()
            }
            StaticExpression::Literal(literal) => self.render_literal(literal, f).unwrap(),
            StaticExpression::Ptr(expr) => {
                write!(f, "&").unwrap();
                self.render_static_expression(expr, f);
            }
        }
    }

    fn render_literal(
        &mut self,
        literal: &Literal,
        f: &mut impl std::io::Write,
    ) -> std::io::Result<()> {
        match literal {
            Literal::Unit => Ok(()),
            Literal::Str(s) => write!(f, "\"{s}\""),
            Literal::Char(c) => write!(f, "'{c}'"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Number(number) => write!(f, "{number}"),
        }
    }
}
