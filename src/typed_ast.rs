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
        value: Box<s!(Expr<'src>)>,
    },
    Print(s!(Expr<'src>)),
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
        op: s!(PrefixOperator),
        expr: Box<s!(Expr<'src>)>,
    },
    Binary {
        op: s!(BinaryOperator),
        lhs: Box<s!(Expr<'src>)>,
        rhs: Box<s!(Expr<'src>)>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Num,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    Num(f64),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrefixOperator {
    Negate,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

macro_rules! s {
    ($e:ty) => {
        Spanned<$e>
    };
}

use s;
