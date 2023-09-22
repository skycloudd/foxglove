use self::ast::*;
use crate::lexer::token::{Control, Keyword, Operator, Token};
use crate::{Span, Spanned};
use chumsky::input::SpannedInput;
use chumsky::prelude::*;

pub mod ast;
pub mod ast_printer;

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserError<'tokens, 'src>>
{
    program_parser()
        .map_with_span(|toplevels, span| (Ast { toplevels }, span))
        .boxed()
}

fn program_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Vec<Spanned<TopLevel<'src>>>>,
    ParserError<'tokens, 'src>,
> {
    let function =
        function_parser().map_with_span(|function, span| (TopLevel::Function(function), span));

    let extern_ = extern_parser().map_with_span(|extern_, span| (TopLevel::Extern(extern_), span));

    choice((function, extern_))
        .repeated()
        .collect()
        .then_ignore(end())
        .map_with_span(|toplevels, span| (toplevels, span))
        .boxed()
}

fn function_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Function<'src>>, ParserError<'tokens, 'src>>
{
    let body = (statement_parser()
        .repeated()
        .collect()
        .map_with_span(|body: Vec<Spanned<Statement>>, span| (body, span))
        .then(expression_parser().or_not()))
    .delimited_by(
        just(Token::Control(Control::LeftCurly)),
        just(Token::Control(Control::RightCurly)),
    );

    attrs_parser()
        .then_ignore(just(Token::Keyword(Keyword::Func)))
        .then(ident_parser())
        .then(params_parser())
        .then(
            just(Token::Control(Control::Colon))
                .ignore_then(type_parser())
                .or_not(),
        )
        .then(body)
        .map_with_span(
            |((((attrs, name), params), ty), (mut body, maybe_expr)), span| {
                body.0.push(match maybe_expr {
                    Some(expr) => (Statement::Return(Some(expr.clone())), expr.1),
                    None => {
                        let span = (body.1.end..body.1.end).into();

                        (
                            Statement::Return(Some((Expr::Literal((Literal::Unit, span)), span))),
                            span,
                        )
                    }
                });

                (
                    Function {
                        attrs,
                        name,
                        params,
                        ty: match ty {
                            Some(ty) => ty,
                            None => (Type::Unit, name.1),
                        },
                        body,
                    },
                    span,
                )
            },
        )
}

fn extern_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Extern<'src>>, ParserError<'tokens, 'src>>
{
    attrs_parser()
        .then_ignore(just(Token::Keyword(Keyword::Extern)))
        .then(ident_parser())
        .then(params_parser())
        .then_ignore(just(Token::Control(Control::Colon)))
        .then(type_parser())
        .then_ignore(just(Token::Control(Control::Semicolon)))
        .map_with_span(|(((attrs, name), params), ty), span| {
            (
                Extern {
                    attrs,
                    name,
                    params,
                    ty,
                },
                span,
            )
        })
}

fn attrs_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Vec<Spanned<Attr<'src>>>>,
    ParserError<'tokens, 'src>,
> {
    attr_parser()
        .repeated()
        .collect()
        .map_with_span(|attrs, span| (attrs, span))
        .boxed()
}

fn attr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Attr<'src>>, ParserError<'tokens, 'src>>
{
    just(Token::Hash)
        .ignore_then(
            ident_parser()
                .then(
                    just(Token::Control(Control::Equals))
                        .ignore_then(expression_parser())
                        .or_not(),
                )
                .delimited_by(
                    just(Token::Control(Control::LeftSquare)),
                    just(Token::Control(Control::RightSquare)),
                ),
        )
        .map_with_span(|(name, value), span| (Attr { name, value }, span))
        .boxed()
}

fn params_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Vec<Spanned<Param<'src>>>>,
    ParserError<'tokens, 'src>,
> {
    param_parser()
        .separated_by(just(Token::Control(Control::Comma)))
        .allow_trailing()
        .collect()
        .delimited_by(
            just(Token::Control(Control::LeftParen)),
            just(Token::Control(Control::RightParen)),
        )
        .map_with_span(|params, span| (params, span))
        .recover_with(via_parser(nested_delimiters(
            Token::Control(Control::LeftParen),
            Token::Control(Control::RightParen),
            [
                (
                    Token::Control(Control::LeftCurly),
                    Token::Control(Control::RightCurly),
                ),
                (
                    Token::Control(Control::LeftSquare),
                    Token::Control(Control::RightSquare),
                ),
            ],
            |span| (vec![], span),
        )))
}

fn param_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Param<'src>>, ParserError<'tokens, 'src>>
{
    ident_parser()
        .then_ignore(just(Token::Control(Control::Colon)))
        .then(type_parser())
        .map_with_span(|(name, ty), span| (Param { name, ty }, span))
        .boxed()
}

fn statement_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Statement<'src>>, ParserError<'tokens, 'src>>
{
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

        let let_ = just(Token::Keyword(Keyword::Var))
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

        let loop_ = just(Token::Keyword(Keyword::Loop))
            .ignore_then(
                block
                    .clone()
                    .map_with_span(|statements, span| (statements, span)),
            )
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

        let return_ = just(Token::Keyword(Keyword::Return))
            .ignore_then(expression_parser().or_not())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(Statement::Return)
            .boxed();

        let if_ = just(Token::Keyword(Keyword::If))
            .ignore_then(expression_parser())
            .then(
                block
                    .clone()
                    .map_with_span(|statements, span| (statements, span)),
            )
            .then(
                (just(Token::Keyword(Keyword::Else)).ignore_then(
                    block
                        .clone()
                        .map_with_span(|statements, span| (statements, span)),
                ))
                .or_not(),
            )
            .map(|((condition, then), otherwise)| Statement::Conditional {
                condition,
                then: Box::new(then),
                otherwise: otherwise.map(Box::new),
            })
            .boxed();

        let while_ = just(Token::Keyword(Keyword::While))
            .ignore_then(expression_parser())
            .then(statement)
            .map(|(condition, body)| Statement::While {
                condition,
                body: Box::new(body),
            })
            .boxed();

        choice((
            expr, block, let_, assign, loop_, continue_, break_, return_, if_, while_,
        ))
        .recover_with(via_parser(nested_delimiters(
            Token::Control(Control::LeftCurly),
            Token::Control(Control::RightCurly),
            [
                (
                    Token::Control(Control::LeftParen),
                    Token::Control(Control::RightParen),
                ),
                (
                    Token::Control(Control::LeftSquare),
                    Token::Control(Control::RightSquare),
                ),
            ],
            |_| Statement::Error,
        )))
        .recover_with(skip_then_retry_until(
            any().ignored(),
            just(Token::Control(Control::Semicolon))
                .or(just(Token::Control(Control::RightCurly)))
                .ignored()
                .or(end()),
        ))
        .map_with_span(|statement, span| (statement, span))
        .boxed()
    })
}

fn expression_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expr<'src>>, ParserError<'tokens, 'src>>
{
    recursive(|expression| {
        let call = ident_parser()
            .then(
                expression
                    .clone()
                    .separated_by(just(Token::Control(Control::Comma)))
                    .allow_trailing()
                    .collect()
                    .delimited_by(
                        just(Token::Control(Control::LeftParen)),
                        just(Token::Control(Control::RightParen)),
                    )
                    .map_with_span(|args, span| (args, span)),
            )
            .map(|(name, args)| Expr::Call { name, args })
            .map_with_span(|expr, span| (expr, span))
            .boxed();

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

        let atom = choice((call, var, literal, parenthesized_expr)).boxed();

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

        logical_or.recover_with(via_parser(nested_delimiters(
            Token::Control(Control::LeftParen),
            Token::Control(Control::RightParen),
            [
                (
                    Token::Control(Control::LeftCurly),
                    Token::Control(Control::RightCurly),
                ),
                (
                    Token::Control(Control::LeftSquare),
                    Token::Control(Control::RightSquare),
                ),
            ],
            |span| (Expr::Error, span),
        )))
    })
}

fn literal_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Literal>, ParserError<'tokens, 'src>>
{
    select! {
        Token::Int(n) => Literal::Int(n),
        Token::Keyword(Keyword::True) => Literal::Bool(true),
        Token::Keyword(Keyword::False) => Literal::Bool(false),
        Token::Hash => Literal::Unit,
    }
    .map_with_span(|literal, span| (literal, span))
    .boxed()
}

fn ident_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<&'src str>, ParserError<'tokens, 'src>>
{
    select! { Token::Ident(ident) => ident }
        .map_with_span(|ident, span| (ident, span))
        .boxed()
}

fn type_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Type>, ParserError<'tokens, 'src>> {
    select! {
        Token::Ident("int") => Type::Int,
        Token::Ident("bool") => Type::Bool,
        Token::Hash => Type::Unit,
    }
    .map_with_span(|ty, span| (ty, span))
    .boxed()
}
