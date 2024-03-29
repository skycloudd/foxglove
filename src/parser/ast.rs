use crate::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'src> {
    pub toplevels: s!(Vec<s!(TopLevel<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TopLevel<'src> {
    Function(s!(Function<'src>)),
    Extern(s!(Extern<'src>)),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'src> {
    pub attrs: s!(Vec<s!(Attr<'src>)>),
    pub name: s!(&'src str),
    pub params: s!(Vec<s!(Param<'src>)>),
    pub ty: s!(Type),
    pub body: s!(Vec<s!(Statement<'src>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Extern<'src> {
    pub attrs: s!(Vec<s!(Attr<'src>)>),
    pub name: s!(&'src str),
    pub params: s!(Vec<s!(Param<'src>)>),
    pub ty: s!(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attr<'src> {
    pub name: s!(&'src str),
    pub value: Option<s!(Expr<'src>)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Param<'src> {
    pub name: s!(&'src str),
    pub ty: s!(Type),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "unit"),
        }
    }
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

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{i}"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Unit => write!(f, "#"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrefixOp {
    Negate,
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

macro_rules! s {
    ($e:ty) => {
        Spanned<$e>
    };
}

use s;
