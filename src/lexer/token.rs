#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'src> {
    Int(i32),
    Unit,
    Operator(Operator),
    Control(Control),
    Keyword(Keyword),
    Ident(&'src str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::Unit => write!(f, "#"),
            Token::Operator(op) => write!(f, "{}", op),
            Token::Control(ctrl) => write!(f, "{}", ctrl),
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::Ident(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Equals => write!(f, "=="),
            Operator::NotEquals => write!(f, "!="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessThanOrEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterThanOrEqual => write!(f, ">="),
            Operator::LogicalAnd => write!(f, "&&"),
            Operator::LogicalOr => write!(f, "||"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Control {
    Semicolon,
    Colon,
    Equals,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,
    Comma,
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
            Control::LeftSquare => write!(f, "["),
            Control::RightSquare => write!(f, "]"),
            Control::Comma => write!(f, ","),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Print,
    Var,
    True,
    False,
    Loop,
    Continue,
    Break,
    Return,
    If,
    Else,
    While,
    Func,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Print => write!(f, "print"),
            Keyword::Var => write!(f, "var"),
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Break => write!(f, "break"),
            Keyword::Return => write!(f, "return"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::While => write!(f, "while"),
            Keyword::Func => write!(f, "func"),
        }
    }
}
