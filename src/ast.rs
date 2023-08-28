use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'src> {
    pub statements: s!(Vec<s!(Statement<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Expr(s!(Expr<'src>)),
    Block(s!(Vec<s!(Statement<'src>)>)),
    Let {
        name: s!(&'src str),
        ty: Option<s!(Type)>,
        value: s!(Expr<'src>),
    },
    Assign {
        name: s!(&'src str),
        value: s!(Expr<'src>),
    },
    Print(Option<s!(Expr<'src>)>),
    Loop(Box<s!(Statement<'src>)>),
    Continue,
    Break,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Num,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
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
    Num(i32),
    Bool(bool),
    Unit,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrefixOp {
    Negate,
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

macro_rules! s {
    ($e:ty) => {
        Spanned<$e>
    };
}

use s;
