use crate::typecheck::Scopes;
use crate::typed_ast::*;

pub fn interpret(ast: TypedAst) -> Result<(), String> {
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

    fn interpret_ast(&mut self, ast: TypedAst<'src>) -> Result<(), String> {
        for statement in ast.statements {
            self.interpret_statement(statement)?;
        }

        Ok(())
    }

    fn interpret_statement(&mut self, statement: Statement<'src>) -> Result<ControlFlow, String> {
        match statement {
            Statement::Error => unreachable!(),
            Statement::Expr(expr) => {
                let _ = self.interpret_expr(expr)?;

                Ok(ControlFlow::Normal)
            }
            Statement::Block(statements) => {
                self.vars.push_scope();

                for statement in statements {
                    match self.interpret_statement(statement)? {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break | ControlFlow::Continue) => return Ok(cf),
                    }
                }

                self.vars.pop_scope();

                Ok(ControlFlow::Normal)
            }
            Statement::Let { name, ty: _, value } => {
                let value = self.interpret_expr(value)?;

                self.vars.insert(name, value);

                Ok(ControlFlow::Normal)
            }
            Statement::Assign { name, value } => {
                let value = self.interpret_expr(value)?;

                let var = self.vars.get_mut(&name).unwrap();

                *var = value;

                Ok(ControlFlow::Normal)
            }
            Statement::Print(expr) => {
                let value = self.interpret_expr(expr)?;

                println!("{}", value);

                Ok(ControlFlow::Normal)
            }
            Statement::Loop(body) => {
                loop {
                    match self.interpret_statement(*body.clone())? {
                        ControlFlow::Normal => {}
                        ControlFlow::Continue => continue,
                        ControlFlow::Break => break,
                    }
                }

                Ok(ControlFlow::Normal)
            }
            Statement::Continue => Ok(ControlFlow::Continue),
            Statement::Break => Ok(ControlFlow::Break),
            Statement::Conditional {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.interpret_expr(condition)?;

                if condition == Value::Bool(true) {
                    match self.interpret_statement(*then)? {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break | ControlFlow::Continue) => return Ok(cf),
                    }
                } else if let Some(otherwise) = otherwise {
                    match self.interpret_statement(*otherwise)? {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break | ControlFlow::Continue) => return Ok(cf),
                    }
                }

                Ok(ControlFlow::Normal)
            }
        }
    }

    fn interpret_expr(&self, expr: Expr) -> Result<Value, String> {
        match expr.expr {
            ExprKind::Error => unreachable!(),
            ExprKind::Var(name) => Ok(self.vars.get(&name).unwrap().clone()),
            ExprKind::Literal(literal) => Ok(match literal {
                Literal::Int(n) => Value::Int(n),
                Literal::Bool(b) => Value::Bool(b),
                Literal::Unit => Value::Unit,
            }),
            ExprKind::Prefix { op, expr } => {
                let value = self.interpret_expr(*expr)?;

                match op {
                    PrefixOp::Negate => match &value {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        _ => unreachable!(),
                    },
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs = self.interpret_expr(*lhs)?;
                let rhs = self.interpret_expr(*rhs)?;

                match (lhs, rhs) {
                    (Value::Int(a), Value::Int(b)) => match op {
                        BinOp::Add => Ok(Value::Int(a + b)),
                        BinOp::Subtract => Ok(Value::Int(a - b)),
                        BinOp::Multiply => Ok(Value::Int(a * b)),
                        BinOp::Divide => Ok(Value::Int(a / b)),
                        BinOp::Equals => Ok(Value::Bool(a == b)),
                        BinOp::NotEquals => Ok(Value::Bool(a != b)),
                        BinOp::LessThan => Ok(Value::Bool(a < b)),
                        BinOp::LessThanOrEqual => Ok(Value::Bool(a <= b)),
                        BinOp::GreaterThan => Ok(Value::Bool(a > b)),
                        BinOp::GreaterThanOrEqual => Ok(Value::Bool(a >= b)),
                        _ => unreachable!(),
                    },
                    (Value::Bool(a), Value::Bool(b)) => match op {
                        BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => {
                            unreachable!()
                        }
                        BinOp::Equals => Ok(Value::Bool(a == b)),
                        BinOp::NotEquals => Ok(Value::Bool(a != b)),
                        BinOp::LessThan => Ok(Value::Bool(!a & b)),
                        BinOp::LessThanOrEqual => Ok(Value::Bool(a <= b)),
                        BinOp::GreaterThan => Ok(Value::Bool(a & !b)),
                        BinOp::GreaterThanOrEqual => Ok(Value::Bool(a >= b)),
                        BinOp::LogicalAnd => Ok(Value::Bool(a && b)),
                        BinOp::LogicalOr => Ok(Value::Bool(a || b)),
                    },
                    _ => unreachable!(),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "#"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ControlFlow {
    Normal,
    Continue,
    Break,
}
