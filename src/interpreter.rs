use crate::typecheck::Scopes;
use crate::typed_ast::*;
use crate::Spanned;

pub fn interpret(ast: Spanned<TypedAst>) -> Result<(), String> {
    let mut interpreter = Interpreter::new();

    interpreter.interpret_ast(ast)
}

struct Interpreter<'src> {
    vars: Scopes<&'src str, Value>,
}

impl<'src> Interpreter<'src> {
    fn new() -> Self {
        Self {
            vars: Scopes::new(),
        }
    }

    fn interpret_ast(&mut self, ast: Spanned<TypedAst<'src>>) -> Result<(), String> {
        self.vars.push_scope();

        for statement in ast.0.statements.0 {
            self.interpret_statement(statement)?;
        }

        self.vars.pop_scope();

        Ok(())
    }

    fn interpret_statement(&mut self, statement: Spanned<Statement<'src>>) -> Result<(), String> {
        match statement.0 {
            Statement::Expr(expr) => {
                let _ = self.interpret_expr(expr)?;
            }
            Statement::Block(statements) => {
                self.vars.push_scope();

                for statement in statements.0 {
                    self.interpret_statement(statement)?;
                }

                self.vars.pop_scope();
            }
            Statement::Let { name, ty, value } => {
                let value = self.interpret_expr(*value)?;

                self.vars.insert(name.0, value);
            }
            Statement::Assign { name, value } => {
                let value = self.interpret_expr(*value)?;

                let var = self.vars.get_mut(&name.0).unwrap();

                *var = value;
            }
            Statement::Print(expr) => {
                let value = self.interpret_expr(expr)?;

                println!("{}", value);
            }
        }

        Ok(())
    }

    fn interpret_expr(&self, expr: Spanned<Expr>) -> Result<Value, String> {
        match expr.0.expr {
            ExprKind::Var(name) => Ok(self.vars.get(&name.0).unwrap().clone()),
            ExprKind::Literal(literal) => Ok(match literal.0 {
                Literal::Num(n) => Value::Num(n),
            }),
            ExprKind::Prefix { op, expr } => {
                let value = self.interpret_expr(*expr)?;

                match op.0 {
                    PrefixOperator::Negate => match &value {
                        Value::Num(n) => Ok(Value::Num(-n)),
                    },
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs = self.interpret_expr(*lhs)?;
                let rhs = self.interpret_expr(*rhs)?;

                match (lhs, rhs) {
                    (Value::Num(a), Value::Num(b)) => match op.0 {
                        BinaryOperator::Add => Ok(Value::Num(a + b)),
                        BinaryOperator::Subtract => Ok(Value::Num(a - b)),
                        BinaryOperator::Multiply => Ok(Value::Num(a * b)),
                        BinaryOperator::Divide => Ok(Value::Num(a / b)),
                    },
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Num(f64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
        }
    }
}
