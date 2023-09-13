use crate::typecheck::Scopes;
use crate::typed_ast::*;
use rustc_hash::FxHashMap;

pub fn interpret(ast: TypedAst) {
    let mut interpreter = Interpreter::new();

    interpreter.interpret_ast(ast);
}

struct Interpreter<'src> {
    vars: Scopes<&'src str, Value>,
    fns: FxHashMap<&'src str, Function<'src>>,
}

impl<'src> Interpreter<'src> {
    fn new() -> Self {
        Self {
            vars: Scopes::new(),
            fns: FxHashMap::default(),
        }
    }

    fn interpret_ast(&mut self, ast: TypedAst<'src>) {
        self.fns = ast.functions;

        let main = self.fns.get("main").unwrap().clone();

        self.interpret_function(main, vec![]);
    }

    fn interpret_function(&mut self, func: Function<'src>, args: Vec<Value>) -> Value {
        let before_vars = self.vars.clone();

        self.vars = Scopes::new();

        self.vars.push_scope();

        for (param, arg) in func.params.iter().zip(args) {
            self.vars.insert(param.name, arg);
        }

        let result = match self.interpret_statement(func.body) {
            ControlFlow::Normal => Value::Unit,
            ControlFlow::Continue => panic!("unexpected continue"),
            ControlFlow::Break => panic!("unexpected break"),
            ControlFlow::Return(value) => value,
        };

        self.vars = before_vars;

        result
    }

    fn interpret_statement(&mut self, statement: Statement<'src>) -> ControlFlow {
        match statement {
            Statement::Error => unreachable!(),
            Statement::Expr(expr) => {
                let _ = self.interpret_expr(expr);

                ControlFlow::Normal
            }
            Statement::Block(statements) => {
                self.vars.push_scope();

                for statement in statements {
                    match self.interpret_statement(statement) {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break
                        | ControlFlow::Continue
                        | ControlFlow::Return(_)) => return cf,
                    }
                }

                self.vars.pop_scope();

                ControlFlow::Normal
            }
            Statement::Let { name, ty: _, value } => {
                let value = self.interpret_expr(value);

                self.vars.insert(name, value);

                ControlFlow::Normal
            }
            Statement::Assign { name, value } => {
                let value = self.interpret_expr(value);

                let var = self.vars.get_mut(&name).unwrap();

                *var = value;

                ControlFlow::Normal
            }
            Statement::Print(expr) => {
                let value = self.interpret_expr(expr);

                println!("{}", value);

                ControlFlow::Normal
            }
            Statement::Loop(body) => {
                loop {
                    match self.interpret_statement(*body.clone()) {
                        ControlFlow::Normal => {}
                        ControlFlow::Continue => continue,
                        ControlFlow::Break => break,
                        cf @ ControlFlow::Return(_) => return cf,
                    }
                }

                ControlFlow::Normal
            }
            Statement::Continue => ControlFlow::Continue,
            Statement::Break => ControlFlow::Break,
            Statement::Return(expr) => {
                let value = self.interpret_expr(expr);

                ControlFlow::Return(value)
            }
            Statement::Conditional {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.interpret_expr(condition);

                if condition == Value::Bool(true) {
                    match self.interpret_statement(*then) {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break
                        | ControlFlow::Continue
                        | ControlFlow::Return(_)) => return cf,
                    }
                } else if let Some(otherwise) = otherwise {
                    match self.interpret_statement(*otherwise) {
                        ControlFlow::Normal => (),
                        cf @ (ControlFlow::Break
                        | ControlFlow::Continue
                        | ControlFlow::Return(_)) => return cf,
                    }
                }

                ControlFlow::Normal
            }
        }
    }

    fn interpret_expr(&mut self, expr: Expr) -> Value {
        match expr.expr {
            ExprKind::Error => unreachable!(),
            ExprKind::Var(name) => *self.vars.get(&name).unwrap(),
            ExprKind::Literal(literal) => match literal {
                Literal::Int(n) => Value::Int(n),
                Literal::Bool(b) => Value::Bool(b),
                Literal::Unit => Value::Unit,
            },
            ExprKind::Prefix { op, expr } => {
                let value = self.interpret_expr(*expr);

                match op {
                    PrefixOp::Negate => match &value {
                        Value::Int(n) => Value::Int(-n),
                        _ => unreachable!(),
                    },
                }
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs = self.interpret_expr(*lhs);
                let rhs = self.interpret_expr(*rhs);

                match (lhs, rhs) {
                    (Value::Int(a), Value::Int(b)) => match op {
                        BinOp::Add => Value::Int(a + b),
                        BinOp::Subtract => Value::Int(a - b),
                        BinOp::Multiply => Value::Int(a * b),
                        BinOp::Divide => Value::Int(a / b),
                        BinOp::Equals => Value::Bool(a == b),
                        BinOp::NotEquals => Value::Bool(a != b),
                        BinOp::LessThan => Value::Bool(a < b),
                        BinOp::LessThanOrEqual => Value::Bool(a <= b),
                        BinOp::GreaterThan => Value::Bool(a > b),
                        BinOp::GreaterThanOrEqual => Value::Bool(a >= b),
                        _ => unreachable!(),
                    },
                    (Value::Bool(a), Value::Bool(b)) => match op {
                        BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => {
                            unreachable!()
                        }
                        BinOp::Equals => Value::Bool(a == b),
                        BinOp::NotEquals => Value::Bool(a != b),
                        BinOp::LessThan => Value::Bool(!a & b),
                        BinOp::LessThanOrEqual => Value::Bool(a <= b),
                        BinOp::GreaterThan => Value::Bool(a & !b),
                        BinOp::GreaterThanOrEqual => Value::Bool(a >= b),
                        BinOp::LogicalAnd => Value::Bool(a && b),
                        BinOp::LogicalOr => Value::Bool(a || b),
                    },
                    _ => unreachable!(),
                }
            }
            ExprKind::Call { name, args } => {
                let args = args
                    .into_iter()
                    .map(|arg| self.interpret_expr(arg))
                    .collect::<Vec<_>>();

                let func = self.fns.get(name).unwrap().clone();

                self.interpret_function(func, args)
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
    Return(Value),
}
