use petgraph::graph::{Graph, NodeIndex};
use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct Functions<'src> {
    pub functions: FxHashMap<&'src str, Function<'src>>,
}

#[derive(Clone, Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub params: Vec<Param<'src>>,
    pub ty: Type,
    pub body: Cfg<'src>,
}

#[derive(Clone, Debug)]
pub struct Cfg<'src> {
    pub graph: Graph<BasicBlock<'src>, ()>,
    pub entry: NodeIndex,
}

#[derive(Clone, Debug)]
pub struct BasicBlock<'src> {
    pub statements: Vec<Statement<'src>>,
    pub terminator: Terminator<'src>,
}

#[derive(Clone, Debug)]
pub enum Terminator<'src> {
    Return(Expr<'src>),
    Jump(NodeIndex),
    Branch {
        condition: Expr<'src>,
        then: NodeIndex,
        otherwise: NodeIndex,
    },
}

#[derive(Clone, Debug)]
pub enum Statement<'src> {
    Error,
    Expr(Expr<'src>),
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
}

#[derive(Clone, Debug)]
pub struct Expr<'src> {
    pub expr: ExprKind<'src>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Param<'src> {
    pub name: &'src str,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    Unit,
}

#[derive(Clone, Debug)]
pub enum PrefixOp {
    Negate,
}

#[derive(Clone, Debug)]
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
