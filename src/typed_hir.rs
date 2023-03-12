use crate::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedHir {
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
    VarDecl(Spanned<Ident>, Spanned<Type>, Spanned<Expr>),
    Assign(Spanned<AssignmentTarget>, Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    IfElse {
        cond: Spanned<Expr>,
        then: Box<Spanned<Self>>,
        else_: Option<Box<Spanned<Self>>>,
    },
    Break,
    Continue,
    Loop(Box<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Spanned<Literal>),
    Var(Spanned<Ident>),
    List(Spanned<Vec<Spanned<Expr>>>),
    Binary {
        lhs: Box<Spanned<Expr>>,
        op: Spanned<BinaryOp>,
        rhs: Box<Spanned<Expr>>,
    },
    Prefix {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<Expr>>,
    },
    Postfix {
        expr: Box<Spanned<Expr>>,
        op: Spanned<PostfixOp>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentTarget {
    pub kind: AssignmentTargetKind,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTargetKind {
    Var(Spanned<Ident>),
    Index(Box<Spanned<AssignmentTarget>>, Box<Spanned<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
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
    Function {
        params: Vec<Spanned<Self>>,
        ret_ty: Box<Spanned<Self>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
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
    Call(Spanned<Vec<Spanned<Expr>>>),
    Index(Box<Spanned<Expr>>),
}

pub type Ident = String;
