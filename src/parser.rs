use crate::ast::{
    AssignmentTarget, Ast, BinaryOp, Expr, FunctionSignature, Ident, Item, Literal, Param,
    PostfixOp, PrefixOp, Statement, Type,
};
use crate::lexer::{Control, Keyword, Operator, Token};
use crate::Spanned;
use chumsky::Parser as ChumskyParser;
use chumsky::{prelude::*, Stream};

pub fn parse(
    tokens: Vec<Spanned<Token>>,
    len: usize,
) -> (Option<Spanned<Ast>>, Vec<Simple<Token>>) {
    parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()))
}

pub fn parser() -> impl ChumskyParser<Token, Spanned<Ast>, Error = Simple<Token>> {
    item_parser()
        .repeated()
        .then_ignore(end())
        .recover_with(skip_then_retry_until([]).consume_end())
        .map_with_span(|items, span| (items, span))
        .map_with_span(|items, span| (Ast { items }, span))
        .boxed()
}

fn item_parser() -> impl ChumskyParser<Token, Spanned<Item>, Error = Simple<Token>> {
    let function = just(Token::Keyword(Keyword::Fn))
        .ignore_then(function_signature_parser())
        .then(statement_parser())
        .map(|(sig, body)| Item::Fn { sig, body })
        .boxed();

    choice((function,))
        .map_with_span(|item, span| (item, span))
        .boxed()
}

fn statement_parser() -> impl ChumskyParser<Token, Spanned<Statement>, Error = Simple<Token>> {
    recursive(|stmt| {
        let block = stmt
            .clone()
            .repeated()
            .delimited_by(
                just(Token::Control(Control::OpenCurlyBrace)),
                just(Token::Control(Control::CloseCurlyBrace)),
            )
            .map_with_span(|stmts, span| (stmts, span))
            .map(Statement::Block)
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let expr = expr_parser()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(Statement::Expr)
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let var_decl = just(Token::Keyword(Keyword::Var))
            .ignore_then(ident_parser())
            .then(
                (just(Token::Control(Control::Colon)))
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|((name, ty), expr)| Statement::VarDecl(name, ty, expr))
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let assign = assignment_target_parser()
            .then_ignore(just(Token::Control(Control::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|(target, expr)| Statement::Assign(target, expr))
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let return_stmt = just(Token::Keyword(Keyword::Return))
            .ignore_then(expr_parser().or_not())
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|expr| Statement::Return(expr))
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let if_else = just(Token::Keyword(Keyword::If))
            .ignore_then(expr_parser())
            .then(stmt.clone().map(Box::new))
            .then(
                just(Token::Keyword(Keyword::Else))
                    .ignore_then(stmt.clone())
                    .map(Box::new)
                    .or_not(),
            )
            .map(|((cond, then), else_)| Statement::IfElse { cond, then, else_ })
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let while_ = just(Token::Keyword(Keyword::While))
            .ignore_then(expr_parser())
            .then(stmt.clone().map(Box::new))
            .map(|(cond, body)| Statement::While { cond, body })
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let for_ = just(Token::Keyword(Keyword::For))
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Keyword(Keyword::In)))
            .then(expr_parser())
            .then(stmt.clone().map(Box::new))
            .map(|((var, in_), body)| Statement::For { var, in_, body })
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let break_ = just(Token::Keyword(Keyword::Break))
            .ignored()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|_| Statement::Break)
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let continue_ = just(Token::Keyword(Keyword::Continue))
            .ignored()
            .then_ignore(just(Token::Control(Control::Semicolon)))
            .map(|_| Statement::Continue)
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        let loop_ = just(Token::Keyword(Keyword::Loop))
            .ignore_then(stmt.clone().map(Box::new))
            .map(Statement::Loop)
            .map_with_span(|stmt, span| (stmt, span))
            .boxed();

        choice((
            block,
            assign,
            var_decl,
            expr,
            return_stmt,
            if_else,
            while_,
            for_,
            break_,
            continue_,
            loop_,
        ))
        .boxed()
    })
}

fn expr_parser() -> impl ChumskyParser<Token, Spanned<Expr>, Error = Simple<Token>> {
    recursive(|expr| {
        let literal = select! {
            Token::Int(n) => Literal::Int(n.parse().unwrap_or(0)),
            Token::Float(n) => Literal::Float(n.parse().unwrap_or(0.0)),
            Token::Bool(b) => Literal::Bool(b),
        }
        .map_with_span(|lit, span| (lit, span))
        .map_with_span(|lit, span| (Expr::Literal(lit), span));

        let var = ident_parser().map_with_span(|ident, span| (Expr::Var(ident), span));

        let parenthesized_expr = expr
            .clone()
            .delimited_by(
                just(Token::Control(Control::OpenParen)),
                just(Token::Control(Control::CloseParen)),
            )
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenParen),
                Token::Control(Control::CloseParen),
                [
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                    (
                        Token::Control(Control::OpenSquareBracket),
                        Token::Control(Control::CloseSquareBracket),
                    ),
                ],
                |span| (Expr::Error, span),
            ))
            .boxed();

        let list = expr
            .clone()
            .separated_by(just(Token::Control(Control::Comma)))
            .allow_trailing()
            .delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            )
            .map_with_span(|elems, span| (elems, span))
            .map_with_span(|elems, span| (Expr::List(elems), span))
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenSquareBracket),
                Token::Control(Control::CloseSquareBracket),
                [
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                    (
                        Token::Control(Control::OpenParen),
                        Token::Control(Control::CloseParen),
                    ),
                ],
                |span| (Expr::Error, span),
            ))
            .boxed();

        let atom = choice((literal, var, parenthesized_expr, list)).boxed();

        let call_args = expr
            .clone()
            .separated_by(just(Token::Control(Control::Comma)))
            .allow_trailing()
            .delimited_by(
                just(Token::Control(Control::OpenParen)),
                just(Token::Control(Control::CloseParen)),
            )
            .map_with_span(|args, span| (args, span))
            .map_with_span(|args, span| (PostfixOp::Call(args), span))
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenParen),
                Token::Control(Control::CloseParen),
                [
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                    (
                        Token::Control(Control::OpenSquareBracket),
                        Token::Control(Control::CloseSquareBracket),
                    ),
                ],
                |span| (PostfixOp::Error, span),
            ))
            .boxed();

        let index = expr
            .clone()
            .delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            )
            .map_with_span(|index, span| (PostfixOp::Index(Box::new(index)), span))
            .recover_with(nested_delimiters(
                Token::Control(Control::OpenSquareBracket),
                Token::Control(Control::CloseSquareBracket),
                [
                    (
                        Token::Control(Control::OpenParen),
                        Token::Control(Control::CloseParen),
                    ),
                    (
                        Token::Control(Control::OpenCurlyBrace),
                        Token::Control(Control::CloseCurlyBrace),
                    ),
                ],
                |span| (PostfixOp::Error, span),
            ))
            .boxed();

        let op = choice((call_args, index)).boxed();

        let postfix = atom
            .then(op.repeated())
            .foldl(|expr, op| {
                let span = expr.1.start..op.1.end;

                (
                    Expr::Postfix {
                        expr: Box::new(expr),
                        op,
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Plus)).to(PrefixOp::Pos),
            just(Token::Operator(Operator::Minus)).to(PrefixOp::Neg),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let prefix = op
            .repeated()
            .then(postfix)
            .foldr(|op, a| {
                let span = op.1.start..a.1.end;

                (
                    Expr::Prefix {
                        op,
                        expr: Box::new(a),
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Star)).to(BinaryOp::Mul),
            just(Token::Operator(Operator::Slash)).to(BinaryOp::Div),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let product = prefix
            .clone()
            .then(op.then(prefix).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(a),
                        op,
                        rhs: Box::new(b),
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Plus)).to(BinaryOp::Add),
            just(Token::Operator(Operator::Minus)).to(BinaryOp::Sub),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(a),
                        op,
                        rhs: Box::new(b),
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Lt)).to(BinaryOp::Lt),
            just(Token::Operator(Operator::Lte)).to(BinaryOp::Lte),
            just(Token::Operator(Operator::Gt)).to(BinaryOp::Gt),
            just(Token::Operator(Operator::Gte)).to(BinaryOp::Gte),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let relational = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(a),
                        op,
                        rhs: Box::new(b),
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((
            just(Token::Operator(Operator::Eq)).to(BinaryOp::Eq),
            just(Token::Operator(Operator::Neq)).to(BinaryOp::Neq),
        ))
        .map_with_span(|op, span| (op, span))
        .boxed();

        let equality = relational
            .clone()
            .then(op.then(relational).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(a),
                        op,
                        rhs: Box::new(b),
                    },
                    span,
                )
            })
            .boxed();

        let op = choice((just(Token::Operator(Operator::Range)).to(BinaryOp::Range),))
            .map_with_span(|op, span| (op, span))
            .boxed();

        let range = equality
            .clone()
            .then(op.then(equality).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;

                (
                    Expr::Binary {
                        lhs: Box::new(a),
                        op,
                        rhs: Box::new(b),
                    },
                    span,
                )
            })
            .boxed();

        range
    })
}

fn function_signature_parser(
) -> impl ChumskyParser<Token, Spanned<FunctionSignature>, Error = Simple<Token>> {
    ident_parser()
        .then(
            param_parser()
                .separated_by(just(Token::Control(Control::Comma)))
                .allow_trailing()
                .delimited_by(
                    just(Token::Control(Control::Pipe)),
                    just(Token::Control(Control::PipeArrow)),
                )
                .map_with_span(|params, span| (params, span)),
        )
        .then(type_parser().or_not().map_with_span(|ty, span| {
            if let Some(ty) = ty {
                ty
            } else {
                (Type::Unit, span)
            }
        }))
        .map_with_span(|((name, params), ret_ty), span| {
            (
                FunctionSignature {
                    name,
                    params,
                    ret_ty,
                },
                span,
            )
        })
        .boxed()
}

fn param_parser() -> impl ChumskyParser<Token, Spanned<Param>, Error = Simple<Token>> {
    ident_parser()
        .then(just(Token::Control(Control::Colon)))
        .then(type_parser())
        .map_with_span(|((name, _), ty), span| (Param { name, ty }, span))
        .boxed()
}

fn ident_parser() -> impl ChumskyParser<Token, Spanned<Ident>, Error = Simple<Token>> {
    filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident),
        _ => Err(Simple::expected_input_found(span, [], Some(tok))),
    })
    .map_with_span(|name, span| (name, span))
    .boxed()
}

fn type_parser() -> impl ChumskyParser<Token, Spanned<Type>, Error = Simple<Token>> {
    recursive(|type_| {
        let unit = just(Token::Control(Control::Unit))
            .ignored()
            .map(|_| Type::Unit)
            .map_with_span(|kind, span| (kind, span))
            .boxed();

        let other = ident_parser()
            .map(|name| match name.0.as_str() {
                "i32" => Type::Int,
                "f32" => Type::Float,
                "bool" => Type::Bool,
                _ => Type::Ident(name),
            })
            .map_with_span(|kind, span| (kind, span))
            .boxed();

        let list = type_
            .delimited_by(
                just(Token::Control(Control::OpenSquareBracket)),
                just(Token::Control(Control::CloseSquareBracket)),
            )
            .map_with_span(|inner, span| (Type::List(Box::new(inner)), span))
            .boxed();

        choice((unit, list, other)).boxed()
    })
}

fn assignment_target_parser(
) -> impl ChumskyParser<Token, Spanned<AssignmentTarget>, Error = Simple<Token>> {
    let ident_assignment = ident_parser()
        .map(AssignmentTarget::Var)
        .map_with_span(|kind, span| (kind, span))
        .boxed();

    let index = expr_parser()
        .delimited_by(
            just(Token::Control(Control::OpenSquareBracket)),
            just(Token::Control(Control::CloseSquareBracket)),
        )
        .recover_with(nested_delimiters(
            Token::Control(Control::OpenSquareBracket),
            Token::Control(Control::CloseSquareBracket),
            [
                (
                    Token::Control(Control::OpenParen),
                    Token::Control(Control::CloseParen),
                ),
                (
                    Token::Control(Control::OpenCurlyBrace),
                    Token::Control(Control::CloseCurlyBrace),
                ),
            ],
            |span| (Expr::Error, span),
        ))
        .boxed();

    ident_assignment
        .then(index.repeated())
        .foldl(|lhs, rhs| {
            let span = lhs.1.start..rhs.1.end;

            (AssignmentTarget::Index(Box::new(lhs), Box::new(rhs)), span)
        })
        .boxed()
}
