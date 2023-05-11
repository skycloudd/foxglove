use crate::ast::*;
use crate::token::{Control, Keyword, Operator, Token};
use crate::{Span, Spanned};
use chumsky::input::SpannedInput;
use chumsky::prelude::*;

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Ast<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    program_parser()
        .map_with_span(|statements, span| (Ast { statements }, span))
        .boxed()
}

fn program_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Vec<Spanned<Statement<'src>>>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    statement_parser()
        .repeated()
        .collect()
        .then_ignore(end())
        .map_with_span(|statements, span| (statements, span))
        .boxed()
}

fn statement_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Statement<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    recursive(|statement| {
        let expr = expression_parser()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(Statement::Expr)
            .boxed();

        let block = statement
            .repeated()
            .collect()
            .delimited_by(
                just(Token::Control(Control::LeftCurly)),
                just(Token::Control(Control::RightCurly)),
            )
            .map_with_span(|statements, span| (statements, span))
            .map(Statement::Block)
            .boxed();

        let let_ = ident_parser()
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expression_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|(name, value)| Statement::Let {
                name,
                value: Box::new(value),
            })
            .boxed();

        let print = just(Token::Keyword(Keyword::Print))
            .ignore_then(expression_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(Statement::Print)
            .boxed();

        choice((expr, block, let_, print))
            .map_with_span(|statement, span| (statement, span))
            .boxed()
    })
}

fn expression_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    recursive(|expression| {
        let var = ident_parser()
            .map(Expr::Var)
            .map_with_span(|expr, span| (expr, span))
            .boxed();

        let literal = literal_parser()
            .map(Expr::Literal)
            .map_with_span(|expr, span| (expr, span))
            .boxed();

        let parenthesized_expr = expression
            .delimited_by(
                just(Token::Control(Control::LeftParen)),
                just(Token::Control(Control::RightParen)),
            )
            .boxed();

        let atom = choice((var, literal, parenthesized_expr)).boxed();

        let prefix_op = just(Token::Operator(Operator::Minus))
            .to(PrefixOperator::Negate)
            .map_with_span(|op, span| (op, span))
            .boxed();

        let prefix = prefix_op
            .repeated()
            .foldr(atom, |op, expr| {
                let span = op.1.start..expr.1.end;

                (
                    Expr::Prefix {
                        op,
                        expr: Box::new(expr),
                    },
                    span.into(),
                )
            })
            .boxed();

        let factor_op = choice((
            just(Token::Operator(Operator::Star)).to(BinaryOperator::Multiply),
            just(Token::Operator(Operator::Slash)).to(BinaryOperator::Divide),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let factor = prefix
            .clone()
            .foldl(factor_op.then(prefix).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    },
                    span.into(),
                )
            })
            .boxed();

        let sum_op = choice((
            just(Token::Operator(Operator::Plus)).to(BinaryOperator::Add),
            just(Token::Operator(Operator::Minus)).to(BinaryOperator::Subtract),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let sum = factor
            .clone()
            .foldl(sum_op.then(factor).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    },
                    span.into(),
                )
            })
            .boxed();

        sum
    })
}

fn literal_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Literal>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    select! {
        Token::Num(n) => Literal::Num(n),
    }
    .map_with_span(|literal, span| (literal, span))
    .boxed()
}

fn ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<&'src str>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    select! { Token::Ident(ident) => ident }
        .map_with_span(|ident, span| (ident, span))
        .boxed()
}
