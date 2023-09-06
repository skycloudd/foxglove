use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAst<'src> {
    pub statements: s!(Vec<s!(Statement<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Error,
    Expr(s!(Expr<'src>)),
    Block(s!(Vec<s!(Statement<'src>)>)),
    Let {
        name: s!(&'src str),
        ty: s!(Type),
        value: s!(Expr<'src>),
    },
    Assign {
        name: s!(&'src str),
        value: s!(Expr<'src>),
    },
    Print(s!(Expr<'src>)),
    Loop(Box<s!(Statement<'src>)>),
    Continue,
    Break,
    Conditional {
        condition: s!(Expr<'src>),
        then: Box<s!(Statement<'src>)>,
        otherwise: Option<Box<s!(Statement<'src>)>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'src> {
    pub expr: ExprKind<'src>,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'src> {
    Error,
    Var(s!(&'src str)),
    Literal(s!(Literal)),
    Prefix {
        op: s!(PrefixOp),
        expr: Box<s!(Expr<'src>)>,
    },
    Binary {
        op: s!(BinOp),
        lhs: Box<s!(Expr<'src>)>,
        rhs: Box<s!(Expr<'src>)>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Unit,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrefixOp {
    Negate,
}

impl std::fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Negate => write!(f, "-"),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Subtract => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
            BinOp::Equals => write!(f, "=="),
            BinOp::NotEquals => write!(f, "!="),
            BinOp::LessThan => write!(f, "<"),
            BinOp::LessThanOrEqual => write!(f, "<="),
            BinOp::GreaterThan => write!(f, ">"),
            BinOp::GreaterThanOrEqual => write!(f, ">="),
            BinOp::LogicalAnd => write!(f, "&&"),
            BinOp::LogicalOr => write!(f, "||"),
        }
    }
}

macro_rules! s {
    ($e:ty) => {
        Spanned<$e>
    };
}

use s;
