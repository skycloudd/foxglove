use rustc_hash::FxHashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedAst<'src> {
    pub functions: FxHashMap<&'src str, Function<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'src> {
    pub name: &'src str,
    pub params: Vec<Param<'src>>,
    pub ty: Type,
    pub body: Vec<Statement<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param<'src> {
    pub name: &'src str,
    pub ty: Type,
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
    Print(Expr<'src>),
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
