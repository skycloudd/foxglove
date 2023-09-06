use crate::token::*;
use crate::{Span, Spanned};
use chumsky::prelude::*;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let literal = text::int(10).from_str().unwrapped().map(Token::Int).boxed();

    let unit = just("#").to(Token::Unit).boxed();

    let op = choice((
        just("==").to(Operator::Equals),
        just("!=").to(Operator::NotEquals),
        just("<=").to(Operator::LessThanOrEqual),
        just(">=").to(Operator::GreaterThanOrEqual),
        just("&&").to(Operator::LogicalAnd),
        just("||").to(Operator::LogicalOr),
        just("<").to(Operator::LessThan),
        just(">").to(Operator::GreaterThan),
        just("+").to(Operator::Plus),
        just("-").to(Operator::Minus),
        just("*").to(Operator::Star),
        just("/").to(Operator::Slash),
    ))
    .map(Token::Operator)
    .boxed();

    let control = choice((
        just(";").to(Token::Control(Control::Semicolon)),
        just(":").to(Token::Control(Control::Colon)),
        just("=").to(Token::Control(Control::Equals)),
        just("(").to(Token::Control(Control::LeftParen)),
        just(")").to(Token::Control(Control::RightParen)),
        just("{").to(Token::Control(Control::LeftCurly)),
        just("}").to(Token::Control(Control::RightCurly)),
        just("[").to(Token::Control(Control::LeftSquare)),
        just("]").to(Token::Control(Control::RightSquare)),
        just(",").to(Token::Control(Control::Comma)),
    ))
    .boxed();

    let ident = text::ident()
        .map(|ident| match ident {
            "print" => Token::Keyword(Keyword::Print),
            "let" => Token::Keyword(Keyword::Let),
            "true" => Token::Keyword(Keyword::True),
            "false" => Token::Keyword(Keyword::False),
            "loop" => Token::Keyword(Keyword::Loop),
            "continue" => Token::Keyword(Keyword::Continue),
            "break" => Token::Keyword(Keyword::Break),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "while" => Token::Keyword(Keyword::While),
            _ => Token::Ident(ident),
        })
        .boxed();

    let token = choice((literal, unit, op, control, ident)).boxed();

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
