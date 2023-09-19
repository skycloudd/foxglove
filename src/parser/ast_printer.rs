use super::ast::*;
use ptree::TreeItem;
use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum AstNode<'src> {
    Ast(Ast<'src>),
    TopLevel(TopLevel<'src>),
    Function(Function<'src>),
    Body(Vec<Statement<'src>>),
    Param(Param<'src>),
    Statement(Statement<'src>),
    Expr(Expr<'src>),
    Literal(Literal),
    Var(String),
    Leaf(String),
    Args(Vec<Expr<'src>>),
    Attrs(Vec<Attr<'src>>),
    Attr(Attr<'src>),
}

impl<'src> From<Ast<'src>> for AstNode<'src> {
    fn from(ast: Ast<'src>) -> AstNode<'src> {
        Self::Ast(ast)
    }
}

impl<'src> From<TopLevel<'src>> for AstNode<'src> {
    fn from(toplevel: TopLevel<'src>) -> AstNode<'src> {
        Self::TopLevel(toplevel)
    }
}

impl<'src> From<Function<'src>> for AstNode<'src> {
    fn from(function: Function<'src>) -> AstNode<'src> {
        Self::Function(function)
    }
}

impl<'src> From<Vec<Statement<'src>>> for AstNode<'src> {
    fn from(body: Vec<Statement<'src>>) -> AstNode<'src> {
        Self::Body(body)
    }
}

impl<'src> From<Param<'src>> for AstNode<'src> {
    fn from(param: Param<'src>) -> AstNode<'src> {
        Self::Param(param)
    }
}

impl<'src> From<Type> for AstNode<'src> {
    fn from(type_: Type) -> AstNode<'src> {
        Self::Leaf(type_.to_string())
    }
}

impl<'src> From<Statement<'src>> for AstNode<'src> {
    fn from(statement: Statement<'src>) -> AstNode<'src> {
        Self::Statement(statement)
    }
}

impl<'src> From<Expr<'src>> for AstNode<'src> {
    fn from(expr: Expr<'src>) -> AstNode<'src> {
        Self::Expr(expr)
    }
}

impl<'src> From<Literal> for AstNode<'src> {
    fn from(literal: Literal) -> AstNode<'src> {
        Self::Literal(literal)
    }
}

impl<'src> From<PrefixOp> for AstNode<'src> {
    fn from(prefix_op: PrefixOp) -> AstNode<'src> {
        Self::Leaf(
            match prefix_op {
                PrefixOp::Negate => "Negate",
            }
            .to_string(),
        )
    }
}

impl<'src> From<BinOp> for AstNode<'src> {
    fn from(bin_op: BinOp) -> AstNode<'src> {
        Self::Leaf(
            match bin_op {
                BinOp::Add => "Add",
                BinOp::Subtract => "Subtract",
                BinOp::Multiply => "Multiply",
                BinOp::Divide => "Divide",
                BinOp::Equals => "Equals",
                BinOp::NotEquals => "NotEquals",
                BinOp::LessThan => "LessThan",
                BinOp::LessThanOrEqual => "LessThanOrEqual",
                BinOp::GreaterThan => "GreaterThan",
                BinOp::GreaterThanOrEqual => "GreaterThanOrEqual",
                BinOp::LogicalAnd => "LogicalAnd",
                BinOp::LogicalOr => "LogicalOr",
            }
            .to_string(),
        )
    }
}

impl<'src> From<String> for AstNode<'src> {
    fn from(name: String) -> AstNode<'src> {
        Self::Leaf(name)
    }
}

impl<'src> From<Vec<Expr<'src>>> for AstNode<'src> {
    fn from(args: Vec<Expr<'src>>) -> AstNode<'src> {
        Self::Args(args)
    }
}

impl<'src> From<Vec<Attr<'src>>> for AstNode<'src> {
    fn from(attrs: Vec<Attr<'src>>) -> AstNode<'src> {
        Self::Attrs(attrs)
    }
}

impl<'src> From<Attr<'src>> for AstNode<'src> {
    fn from(attr: Attr<'src>) -> AstNode<'src> {
        Self::Attr(attr)
    }
}

impl TreeItem for AstNode<'_> {
    type Child = Self;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        write!(
            f,
            "{}",
            style.paint(match self {
                AstNode::Ast(_) => "Ast".to_string(),
                AstNode::TopLevel(_) => "TopLevel".to_string(),
                AstNode::Function(function) => format!(
                    "Function [{}({}): {}]",
                    function.name.0,
                    function
                        .params
                        .0
                        .iter()
                        .map(|param| format!("{}: {}", param.0.name.0, param.0.ty.0))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function.ty.0
                ),
                AstNode::Body(_) => "Body".to_string(),
                AstNode::Param(_) => "Param".to_string(),
                AstNode::Statement(statement) => match statement {
                    Statement::Error => "Error",
                    Statement::Expr(_) => "Expr",
                    Statement::Block(_) => "Block",
                    Statement::Let { .. } => "Let",
                    Statement::Assign { .. } => "Assign",
                    Statement::Loop(_) => "Loop",
                    Statement::Continue => "Continue",
                    Statement::Break => "Break",
                    Statement::Return(_) => "Return",
                    Statement::Conditional { .. } => "Conditional",
                    Statement::While { .. } => "While",
                }
                .to_string(),
                AstNode::Expr(expr) => match expr {
                    Expr::Error => "Error",
                    Expr::Var(_) => "Var",
                    Expr::Literal(_) => "Literal",
                    Expr::Prefix { .. } => "Prefix",
                    Expr::Binary { .. } => "Binary",
                    Expr::Call { .. } => "Call",
                }
                .to_string(),
                AstNode::Literal(literal) => match literal {
                    Literal::Int(_) => "Int",
                    Literal::Bool(_) => "Bool",
                    Literal::Unit => "Unit",
                }
                .to_string(),
                AstNode::Var(_) => "Var".to_string(),
                AstNode::Leaf(name) => name.to_string(),
                AstNode::Args(_) => "Args".to_string(),
                AstNode::Attrs(_) => "Attrs".to_string(),
                AstNode::Attr(_) => "Attr".to_string(),
            })
        )
    }

    fn children(&self) -> Cow<[Self::Child]> {
        Cow::Owned(match self {
            AstNode::Ast(ast) => ast
                .toplevels
                .0
                .iter()
                .map(|toplevel| toplevel.0.clone().into())
                .collect(),
            AstNode::TopLevel(toplevel) => match toplevel {
                TopLevel::Function(function) => vec![function.0.clone().into()],
            },
            AstNode::Function(function) => {
                let attrs = function
                    .attrs
                    .0
                    .iter()
                    .map(|attr| attr.0.clone())
                    .collect::<Vec<_>>()
                    .into();

                let body = function
                    .body
                    .0
                    .iter()
                    .map(|statement| statement.0.clone())
                    .collect::<Vec<_>>()
                    .into();

                vec![attrs, body]
            }
            AstNode::Body(body) => body
                .iter()
                .map(|statement| statement.clone().into())
                .collect(),
            AstNode::Param(param) => {
                vec![param.name.0.to_string().into(), param.ty.0.into()]
            }
            AstNode::Statement(statement) => match statement {
                Statement::Error => vec![],
                Statement::Expr(expr) => vec![expr.0.clone().into()],
                Statement::Block(statements) => statements
                    .0
                    .iter()
                    .map(|statement| statement.0.clone().into())
                    .collect(),
                Statement::Let { name, ty, value } => {
                    let mut vec = vec![AstNode::Var(name.0.to_string())];

                    if let Some(ty) = ty {
                        vec.push(ty.0.into());
                    }

                    vec.push(value.0.clone().into());

                    vec
                }
                Statement::Assign { name, value } => {
                    vec![AstNode::Var(name.0.to_string()), value.0.clone().into()]
                }
                Statement::Loop(statement) => vec![statement.0.clone().into()],
                Statement::Continue => vec![],
                Statement::Break => vec![],
                Statement::Return(maybe_expression) => {
                    if let Some(expression) = maybe_expression {
                        vec![expression.0.clone().into()]
                    } else {
                        vec![]
                    }
                }
                Statement::Conditional {
                    condition,
                    then,
                    otherwise,
                } => {
                    let mut vec = vec![condition.0.clone().into(), then.0.clone().into()];

                    if let Some(otherwise) = otherwise {
                        vec.push(otherwise.0.clone().into());
                    }

                    vec
                }
                Statement::While { condition, body } => {
                    vec![condition.0.clone().into(), body.0.clone().into()]
                }
            },
            AstNode::Expr(expr) => match expr {
                Expr::Error => vec![],
                Expr::Var(var) => vec![var.0.to_string().into()],
                Expr::Literal(literal) => vec![literal.0.into()],
                Expr::Prefix { op, expr } => vec![op.0.into(), expr.0.clone().into()],
                Expr::Binary { op, lhs, rhs } => {
                    vec![op.0.into(), lhs.0.clone().into(), rhs.0.clone().into()]
                }
                Expr::Call { name, args } => {
                    vec![
                        name.0.to_string().into(),
                        args.0
                            .iter()
                            .map(|arg| arg.0.clone())
                            .collect::<Vec<Expr>>()
                            .into(),
                    ]
                }
            },
            AstNode::Literal(literal) => vec![literal.to_string().into()],
            AstNode::Var(var) => vec![var.to_string().into()],
            AstNode::Leaf(_) => vec![],
            AstNode::Args(args) => args
                .iter()
                .map(|arg| arg.clone().into())
                .collect::<Vec<_>>(),
            AstNode::Attrs(attrs) => attrs
                .iter()
                .map(|attr| attr.clone().into())
                .collect::<Vec<_>>(),
            AstNode::Attr(attr) => {
                let mut vec = vec![attr.name.0.to_string().into()];

                if let Some(value) = &attr.value {
                    vec.push(value.0.clone().into());
                }

                vec
            }
        })
    }
}
