#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Token<'src> {
    Num(f64),
    Operator(Operator),
    Control(Control),
    Keyword(Keyword),
    Ident(&'src str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{}", n),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Control(ctrl) => write!(f, "{}", ctrl),
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::Ident(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Control {
    Semicolon,
    Colon,
    Equals,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
}

impl std::fmt::Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Control::Semicolon => write!(f, ";"),
            Control::Colon => write!(f, ":"),
            Control::Equals => write!(f, "="),
            Control::LeftParen => write!(f, "("),
            Control::RightParen => write!(f, ")"),
            Control::LeftCurly => write!(f, "{{"),
            Control::RightCurly => write!(f, "}}"),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Keyword {
    Print,
    Let,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Print => write!(f, "print"),
            Keyword::Let => write!(f, "let"),
        }
    }
}
