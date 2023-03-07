use crate::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub items: Spanned<Vec<Spanned<Item>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fn {
        sig: Spanned<FunctionSignature>,
        body: Spanned<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: Spanned<Ident>,
    pub params: Spanned<Vec<Spanned<Param>>>,
    pub ret_ty: Spanned<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Spanned<Vec<Spanned<Self>>>),
    Expr(Spanned<Expr>),
    VarDecl(Spanned<Ident>, Option<Spanned<Type>>, Spanned<Expr>),
    Assign(Spanned<AssignmentTarget>, Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    IfElse {
        cond: Spanned<Expr>,
        then: Box<Spanned<Self>>,
        else_: Option<Box<Spanned<Self>>>,
    },
    While {
        cond: Spanned<Expr>,
        body: Box<Spanned<Self>>,
    },
    For {
        var: Spanned<Ident>,
        in_: Spanned<Expr>,
        body: Box<Spanned<Self>>,
    },
    Break,
    Continue,
    Loop(Box<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Error,
    Literal(Spanned<Literal>),
    Var(Spanned<Ident>),
    List(Spanned<Vec<Spanned<Self>>>),
    Binary {
        lhs: Box<Spanned<Self>>,
        op: Spanned<BinaryOp>,
        rhs: Box<Spanned<Self>>,
    },
    Prefix {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<Self>>,
    },
    Postfix {
        expr: Box<Spanned<Self>>,
        op: Spanned<PostfixOp>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Var(Spanned<Ident>),
    Index(Box<Spanned<Self>>, Box<Spanned<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Ident(Spanned<Ident>),
    Int,
    Float,
    Bool,
    List(Box<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Range,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    Error,
    Call(Spanned<Vec<Spanned<Expr>>>),
    Index(Box<Spanned<Expr>>),
}

pub type Ident = String;
