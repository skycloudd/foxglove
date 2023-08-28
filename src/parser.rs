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
            .clone()
            .repeated()
            .collect()
            .delimited_by(
                just(Token::Control(Control::LeftCurly)),
                just(Token::Control(Control::RightCurly)),
            )
            .map_with_span(|statements, span| (statements, span))
            .map(Statement::Block)
            .boxed();

        let let_ = just(Token::Keyword(Keyword::Let))
            .ignore_then(ident_parser())
            .then(
                just(Token::Control(Control::Colon))
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expression_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|((name, ty), value)| Statement::Let { name, ty, value })
            .boxed();

        let assign = ident_parser()
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expression_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|(name, value)| Statement::Assign { name, value })
            .boxed();

        let print = just(Token::Keyword(Keyword::Print))
            .ignore_then(expression_parser().or_not())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(Statement::Print)
            .boxed();

        let loop_ = just(Token::Keyword(Keyword::Loop))
            .ignore_then(statement)
            .map(|body| Statement::Loop(Box::new(body)))
            .boxed();

        let continue_ = just(Token::Keyword(Keyword::Continue))
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|_| Statement::Continue)
            .boxed();

        let break_ = just(Token::Keyword(Keyword::Break))
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|_| Statement::Break)
            .boxed();

        choice((expr, block, let_, assign, print, loop_, continue_, break_))
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
            .clone()
            .delimited_by(
                just(Token::Control(Control::LeftParen)),
                just(Token::Control(Control::RightParen)),
            )
            .boxed();

        let atom = choice((var, literal, parenthesized_expr)).boxed();

        let prefix_op = just(Token::Operator(Operator::Minus))
            .to(PrefixOp::Negate)
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
            just(Token::Operator(Operator::Star)).to(BinOp::Multiply),
            just(Token::Operator(Operator::Slash)).to(BinOp::Divide),
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
            just(Token::Operator(Operator::Plus)).to(BinOp::Add),
            just(Token::Operator(Operator::Minus)).to(BinOp::Subtract),
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

        let relational_op = choice((
            just(Token::Operator(Operator::LessThan)).to(BinOp::LessThan),
            just(Token::Operator(Operator::LessThanOrEqual)).to(BinOp::LessThanOrEqual),
            just(Token::Operator(Operator::GreaterThan)).to(BinOp::GreaterThan),
            just(Token::Operator(Operator::GreaterThanOrEqual)).to(BinOp::GreaterThanOrEqual),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let relational = sum
            .clone()
            .foldl(relational_op.then(sum).repeated(), |lhs, (op, rhs)| {
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

        let equality_op = choice((
            just(Token::Operator(Operator::Equals)).to(BinOp::Equals),
            just(Token::Operator(Operator::NotEquals)).to(BinOp::NotEquals),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let equality = relational
            .clone()
            .foldl(equality_op.then(relational).repeated(), |lhs, (op, rhs)| {
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

        let logical_and_op = just(Token::Operator(Operator::LogicalAnd))
            .to(BinOp::LogicalAnd)
            .map_with_span(|op, span| (op, span))
            .boxed();

        let logical_and = equality
            .clone()
            .foldl(
                logical_and_op.then(equality).repeated(),
                |lhs, (op, rhs)| {
                    let span = lhs.1.start..rhs.1.end;

                    (
                        Expr::Binary {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        },
                        span.into(),
                    )
                },
            )
            .boxed();

        let logical_or_op = just(Token::Operator(Operator::LogicalOr))
            .to(BinOp::LogicalOr)
            .map_with_span(|op, span| (op, span))
            .boxed();

        let logical_or = logical_and
            .clone()
            .foldl(
                logical_or_op.then(logical_and).repeated(),
                |lhs, (op, rhs)| {
                    let span = lhs.1.start..rhs.1.end;

                    (
                        Expr::Binary {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        },
                        span.into(),
                    )
                },
            )
            .boxed();

        logical_or
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
        Token::Keyword(Keyword::True) => Literal::Bool(true),
        Token::Keyword(Keyword::False) => Literal::Bool(false),
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

fn type_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Type>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    select! {
        Token::Ident("num") => Type::Num,
        Token::Ident("bool") => Type::Bool,
        Token::Unit => Type::Unit,
    }
    .map_with_span(|ty, span| (ty, span))
    .boxed()
}
