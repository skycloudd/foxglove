use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'src> {
    pub functions: s!(Vec<s!(Function<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'src> {
    pub name: s!(&'src str),
    pub params: s!(Vec<s!(Param<'src>)>),
    pub ty: s!(Type),
    pub body: s!(Statement<'src>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param<'src> {
    pub name: s!(&'src str),
    pub ty: s!(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Error,
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
    Return(Option<s!(Expr<'src>)>),
    Conditional {
        condition: s!(Expr<'src>),
        then: Box<s!(Statement<'src>)>,
        otherwise: Option<Box<s!(Statement<'src>)>>,
    },
    While {
        condition: s!(Expr<'src>),
        body: Box<s!(Statement<'src>)>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
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
    Call {
        name: s!(&'src str),
        args: s!(Vec<s!(Expr<'src>)>),
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
