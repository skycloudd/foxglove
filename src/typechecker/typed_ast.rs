use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAst<'src> {
    pub functions: HashMap<&'src str, Function<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'src> {
    pub name: &'src str,
    pub params: Vec<Param<'src>>,
    pub ty: Type,
    pub body: Vec<Statement<'src>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Param<'src> {
    pub name: &'src str,
    pub ty: Type,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Error,
    Expr(Expr<'src>),
    Block(Vec<Statement<'src>>),
    Let {
        name: &'src str,
        ty: Type,
        value: Expr<'src>,
    },
    Assign {
        name: &'src str,
        value: Expr<'src>,
    },
    Loop(Box<Statement<'src>>),
    Continue,
    Break,
    Return(Expr<'src>),
    Conditional {
        condition: Expr<'src>,
        then: Box<Statement<'src>>,
        otherwise: Option<Box<Statement<'src>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'src> {
    pub expr: ExprKind<'src>,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'src> {
    Error,
    Var(&'src str),
    Literal(Literal),
    Prefix {
        op: PrefixOp,
        expr: Box<Expr<'src>>,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr<'src>>,
        rhs: Box<Expr<'src>>,
    },
    Call {
        name: &'src str,
        args: Vec<Expr<'src>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Unit,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrefixOp {
    Negate,
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrefixOp::Negate => "-",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Subtract => "-",
                BinOp::Multiply => "*",
                BinOp::Divide => "/",
                BinOp::Equals => "==",
                BinOp::NotEquals => "!=",
                BinOp::LessThan => "<",
                BinOp::LessThanOrEqual => "<=",
                BinOp::GreaterThan => ">",
                BinOp::GreaterThanOrEqual => ">=",
                BinOp::LogicalAnd => "&&",
                BinOp::LogicalOr => "||",
            }
        )
    }
}
