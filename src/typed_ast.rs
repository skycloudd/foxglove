use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAst<'src> {
    pub statements: s!(Vec<s!(Statement<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'src> {
    Expr(s!(Expr<'src>)),
    Block(s!(Vec<s!(Statement<'src>)>)),
    Let {
        name: s!(&'src str),
        ty: s!(Type),
        value: Box<s!(Expr<'src>)>,
    },
    Assign {
        name: s!(&'src str),
        value: Box<s!(Expr<'src>)>,
    },
    Print(s!(Expr<'src>)),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Num,
    Bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'src> {
    pub expr: ExprKind<'src>,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'src> {
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
    Num(f64),
    Bool(bool),
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
