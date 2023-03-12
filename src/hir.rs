use crate::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub struct Hir {
    pub items: Spanned<Vec<Spanned<Item>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fn {
        attrs: Spanned<Vec<Spanned<Attribute>>>,
        sig: Spanned<FunctionSignature>,
        body: Spanned<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Spanned<Ident>,
    pub value: Spanned<Expr>,
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
    Break,
    Continue,
    Loop(Box<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
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
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl Literal {
    pub fn ty(&self) -> Type {
        match self {
            Literal::Unit => Type::Unit,
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
        }
    }
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

mod pretty_printing {
    use super::*;
    use pretty::termcolor::{Ansi, Color, ColorSpec};
    use pretty::RcDoc;

    impl Hir {
        pub fn to_pretty(&self, width: usize) -> Result<String, Box<dyn std::error::Error>> {
            let mut w = Ansi::new(Vec::new());

            self.to_doc().render_colored(width, &mut w)?;

            Ok(String::from_utf8(w.into_inner())?)
        }

        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            RcDoc::text("[")
                .append(RcDoc::line())
                .append(RcDoc::intersperse(
                    self.items.0.iter().map(|item| item.0.to_doc()),
                    RcDoc::line().append(RcDoc::line()),
                ))
                .append(RcDoc::line())
                .append(RcDoc::text("]"))
        }
    }

    impl Item {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                Item::Fn { attrs, sig, body } => {
                    RcDoc::intersperse(attrs.0.iter().map(|attr| attr.0.to_doc()), RcDoc::line())
                        .append(RcDoc::line())
                        .append(
                            RcDoc::text("fn")
                                .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                                .append(RcDoc::space())
                                .append(sig.0.to_doc())
                                .append(RcDoc::line())
                                .append(body.0.to_doc())
                                .nest(4),
                        )
                }
            }
        }
    }

    impl Attribute {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            RcDoc::text("@")
                .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                .append(
                    RcDoc::text("[").annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                )
                .append(self.name.0.clone())
                .append(RcDoc::space())
                .append(RcDoc::text("="))
                .append(RcDoc::space())
                .append(self.value.0.to_doc())
                .append(
                    RcDoc::text("]").annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                )
        }
    }

    impl FunctionSignature {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            RcDoc::text(&self.name.0)
                .append(RcDoc::space())
                .append(
                    RcDoc::text("|").annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone()),
                )
                .append(
                    RcDoc::intersperse(
                        self.params.0.iter().map(|param| param.0.to_doc()),
                        RcDoc::text(",").append(RcDoc::space()),
                    )
                    .group(),
                )
                .append(
                    RcDoc::text("|>").annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone()),
                )
                .append(RcDoc::space())
                .append(self.ret_ty.0.to_doc())
        }
    }

    impl Param {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            RcDoc::text(&self.name.0)
                .append(
                    RcDoc::text(":").annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone()),
                )
                .append(RcDoc::space())
                .append(self.ty.0.to_doc())
        }
    }

    impl Type {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                Type::Unit => RcDoc::text("()"),
                Type::Ident(i) => RcDoc::text(&i.0),
                Type::Int => RcDoc::text("int"),
                Type::Float => RcDoc::text("float"),
                Type::Bool => RcDoc::text("bool"),
                Type::List(t) => RcDoc::text("[")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone())
                    .append(t.0.to_doc())
                    .append(
                        RcDoc::text("]")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
            }
            .annotate(ColorSpec::new().set_fg(Some(Color::Blue)).clone())
        }
    }

    impl Statement {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                Statement::Block(stmts) => RcDoc::text("{")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone())
                    .append(RcDoc::line())
                    .append(RcDoc::intersperse(
                        stmts.0.iter().map(|stmt| stmt.0.to_doc()),
                        RcDoc::line(),
                    ))
                    .nest(4)
                    .append(RcDoc::line())
                    .append(
                        RcDoc::text("}")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
                Statement::Expr(expr) => expr.0.to_doc().append(RcDoc::text(";")),
                Statement::VarDecl(name, ty, expr) => RcDoc::text("var")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Cyan)).clone())
                    .append(RcDoc::space())
                    .append(&name.0)
                    .append(match ty {
                        Some(ty) => RcDoc::text(":")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                            .append(RcDoc::space())
                            .append(ty.0.to_doc()),
                        None => RcDoc::nil(),
                    })
                    .append(RcDoc::space())
                    .append(RcDoc::text("="))
                    .append(RcDoc::space())
                    .append(expr.0.to_doc())
                    .append(RcDoc::text(";")),
                Statement::Assign(target, expr) => target
                    .0
                    .to_doc()
                    .append(RcDoc::space())
                    .append(RcDoc::text("="))
                    .append(RcDoc::space())
                    .append(expr.0.to_doc())
                    .append(RcDoc::text(";")),
                Statement::Return(expr) => RcDoc::text("return")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                    .append(match expr {
                        Some(expr) => RcDoc::space().append(expr.0.to_doc()),
                        None => RcDoc::nil(),
                    })
                    .append(RcDoc::text(";")),
                Statement::IfElse { cond, then, else_ } => RcDoc::text("if")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                    .append(RcDoc::space())
                    .append(cond.0.to_doc())
                    .append(RcDoc::line())
                    .append(then.0.to_doc())
                    .nest(4)
                    .append(RcDoc::line())
                    .append(match else_ {
                        Some(else_) => RcDoc::text("else")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                            .append(RcDoc::line())
                            .append(else_.0.to_doc())
                            .nest(4),
                        None => RcDoc::nil(),
                    }),
                Statement::Break => RcDoc::text("break")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                    .append(RcDoc::text(";")),
                Statement::Continue => RcDoc::text("continue")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                    .append(RcDoc::text(";")),
                Statement::Loop(body) => RcDoc::text("loop")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Red)).clone())
                    .append(RcDoc::space())
                    .append(RcDoc::line())
                    .append(body.0.to_doc())
                    .nest(4),
            }
        }
    }

    impl Expr {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                Expr::Literal(lit) => lit
                    .0
                    .to_doc()
                    .annotate(ColorSpec::new().set_fg(Some(Color::Magenta)).clone()),
                Expr::Var(name) => RcDoc::text(&name.0),
                Expr::List(items) => RcDoc::text("[")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone())
                    .append(
                        RcDoc::intersperse(
                            items.0.iter().map(|item| item.0.to_doc()),
                            RcDoc::text(",").append(RcDoc::space()),
                        )
                        .group(),
                    )
                    .append(
                        RcDoc::text("]")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
                Expr::Binary { lhs, op, rhs } => lhs
                    .0
                    .to_doc()
                    .append(RcDoc::space())
                    .append(op.0.to_doc())
                    .append(RcDoc::space())
                    .append(rhs.0.to_doc()),
                Expr::Prefix { op, expr } => op.0.to_doc().append(expr.0.to_doc()),
                Expr::Postfix { expr, op } => if let PostfixOp::Call(_) = op.0 {
                    expr.0
                        .to_doc()
                        .annotate(ColorSpec::new().set_fg(Some(Color::Green)).clone())
                } else {
                    expr.0.to_doc()
                }
                .append(op.0.to_doc()),
            }
        }
    }

    impl Literal {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                Literal::Unit => RcDoc::text("()"),
                Literal::Int(i) => RcDoc::as_string(i),
                Literal::Float(f) => RcDoc::as_string(f),
                Literal::Bool(b) => RcDoc::as_string(b),
            }
        }
    }

    impl BinaryOp {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                BinaryOp::Add => RcDoc::text("+"),
                BinaryOp::Sub => RcDoc::text("-"),
                BinaryOp::Mul => RcDoc::text("*"),
                BinaryOp::Div => RcDoc::text("/"),
                BinaryOp::Eq => RcDoc::text("=="),
                BinaryOp::Neq => RcDoc::text("!="),
                BinaryOp::Lt => RcDoc::text("<"),
                BinaryOp::Gt => RcDoc::text(">"),
                BinaryOp::Lte => RcDoc::text("<="),
                BinaryOp::Gte => RcDoc::text(">="),
            }
        }
    }

    impl PrefixOp {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                PrefixOp::Pos => RcDoc::text("+"),
                PrefixOp::Neg => RcDoc::text("-"),
            }
        }
    }

    impl PostfixOp {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                PostfixOp::Call(args) => RcDoc::text("(")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone())
                    .append(
                        RcDoc::intersperse(
                            args.0.iter().map(|arg| arg.0.to_doc()),
                            RcDoc::text(",").append(RcDoc::space()),
                        )
                        .group(),
                    )
                    .append(
                        RcDoc::text(")")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
                PostfixOp::Index(index) => RcDoc::text("[")
                    .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone())
                    .append(index.0.to_doc())
                    .append(
                        RcDoc::text("]")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
            }
        }
    }

    impl AssignmentTarget {
        pub fn to_doc(&self) -> RcDoc<ColorSpec> {
            match self {
                AssignmentTarget::Var(name) => RcDoc::text(&name.0),
                AssignmentTarget::Index(target, index) => target
                    .0
                    .to_doc()
                    .append(
                        RcDoc::text("[")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    )
                    .append(index.0.to_doc())
                    .append(
                        RcDoc::text("]")
                            .annotate(ColorSpec::new().set_fg(Some(Color::Yellow)).clone()),
                    ),
            }
        }
    }
}
