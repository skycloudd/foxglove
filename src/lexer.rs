use crate::token::*;
use crate::{Span, Spanned};
use chumsky::prelude::*;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let literal = literal_lexer();

    let unit = just("#").to(Token::Unit).boxed();

    let op = op_lexer();

    let control = control_lexer();

    let keyword = keyword_lexer();

    let ident = text::ident().map(Token::Ident).boxed();

    let token = choice((literal, unit, op, control, keyword, ident)).boxed();

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

fn literal_lexer<'src>(
) -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .from_str()
        .unwrapped()
        .map(Token::Num)
        .boxed();

    choice((num,)).boxed()
}

fn op_lexer<'src>() -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>>
{
    choice((
        just("==").to(Operator::Equals),
        just("!=").to(Operator::NotEquals),
        just("<").to(Operator::LessThan),
        just("<=").to(Operator::LessThanOrEqual),
        just(">").to(Operator::GreaterThan),
        just(">=").to(Operator::GreaterThanOrEqual),
        just("&&").to(Operator::LogicalAnd),
        just("||").to(Operator::LogicalOr),
        just("+").to(Operator::Plus),
        just("-").to(Operator::Minus),
        just("*").to(Operator::Star),
        just("/").to(Operator::Slash),
    ))
    .map(Token::Operator)
    .boxed()
}

fn control_lexer<'src>(
) -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>> {
    choice((
        just(";").to(Token::Control(Control::Semicolon)),
        just(":").to(Token::Control(Control::Colon)),
        just("=").to(Token::Control(Control::Equals)),
        just("(").to(Token::Control(Control::LeftParen)),
        just(")").to(Token::Control(Control::RightParen)),
        just("{").to(Token::Control(Control::LeftCurly)),
        just("}").to(Token::Control(Control::RightCurly)),
        just(",").to(Token::Control(Control::Comma)),
    ))
    .boxed()
}

fn keyword_lexer<'src>(
) -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>> {
    choice((
        just("print").to(Keyword::Print),
        just("let").to(Keyword::Let),
        just("true").to(Keyword::True),
        just("false").to(Keyword::False),
    ))
    .map(Token::Keyword)
    .boxed()
}
