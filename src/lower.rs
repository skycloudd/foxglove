use crate::ast::{self, Ast};
use crate::hir::{
    AssignmentTarget, Attribute, BinaryOp, Expr, FunctionSignature, Hir, Item, Literal, Param,
    PostfixOp, PrefixOp, Statement, Type,
};
use crate::Spanned;

pub fn lower(ast: Spanned<Ast>) -> Spanned<Hir> {
    (
        Hir {
            items: (
                ast.0.items.0.into_iter().map(lower_item).collect(),
                ast.1.clone(),
            ),
        },
        ast.1,
    )
}

fn lower_item(item: Spanned<ast::Item>) -> Spanned<Item> {
    (
        match item.0 {
            ast::Item::Fn { attrs, sig, body } => Item::Fn {
                attrs: (attrs.0.into_iter().map(lower_attribute).collect(), attrs.1),
                sig: lower_function_signature(sig),
                body: lower_statement(body),
            },
        },
        item.1,
    )
}

fn lower_attribute(attr: Spanned<ast::Attribute>) -> Spanned<Attribute> {
    (
        Attribute {
            name: attr.0.name,
            value: lower_expr(attr.0.value),
        },
        attr.1,
    )
}

fn lower_function_signature(sig: Spanned<ast::FunctionSignature>) -> Spanned<FunctionSignature> {
    (
        FunctionSignature {
            name: sig.0.name,
            params: (
                sig.0.params.0.into_iter().map(lower_param).collect(),
                sig.0.params.1,
            ),
            ret_ty: lower_type(sig.0.ret_ty),
        },
        sig.1,
    )
}

fn lower_param(param: Spanned<ast::Param>) -> Spanned<Param> {
    (
        Param {
            name: param.0.name,
            ty: lower_type(param.0.ty),
        },
        param.1,
    )
}

fn lower_statement(stmt: Spanned<ast::Statement>) -> Spanned<Statement> {
    (
        match stmt.0 {
            ast::Statement::Block(stmts) => {
                Statement::Block((stmts.0.into_iter().map(lower_statement).collect(), stmts.1))
            }
            ast::Statement::Expr(expr) => Statement::Expr(lower_expr(expr)),
            ast::Statement::VarDecl(name, ty, expr) => {
                Statement::VarDecl(name, ty.map(lower_type), lower_expr(expr))
            }
            ast::Statement::Assign(target, expr) => {
                Statement::Assign(lower_assignment_target(target), lower_expr(expr))
            }
            ast::Statement::Return(expr) => Statement::Return(expr.map(lower_expr)),
            ast::Statement::IfElse { cond, then, else_ } => Statement::IfElse {
                cond: lower_expr(cond),
                then: Box::new(lower_statement(*then)),
                else_: else_.map(|else_| Box::new(lower_statement(*else_))),
            },
            ast::Statement::While { cond, body } => {
                let cond = lower_expr(cond);
                let body = lower_statement(*body);

                let if_true_break_else_body = Statement::IfElse {
                    cond: (
                        Expr::Binary {
                            lhs: Box::new(cond.clone()),
                            op: (BinaryOp::Eq, cond.1.clone()),
                            rhs: Box::new((
                                Expr::Literal((Literal::Bool(false), stmt.1.clone())),
                                cond.1.clone(),
                            )),
                        },
                        cond.1.clone(),
                    ),
                    then: Box::new((Statement::Break, cond.1)),
                    else_: Some(Box::new(body)),
                };

                Statement::Loop(Box::new((if_true_break_else_body, stmt.1.clone())))
            }
            ast::Statement::For {
                var,
                start,
                end,
                body,
            } => {
                let var = var;
                let start = lower_expr(start);
                let end = lower_expr(end);
                let body = lower_statement(*body);

                let loop_body = Statement::IfElse {
                    cond: (
                        Expr::Binary {
                            lhs: Box::new((
                                Expr::Var((var.clone().0, var.1.clone())),
                                var.1.clone(),
                            )),
                            op: (BinaryOp::Lt, end.1.clone()),
                            rhs: Box::new(end),
                        },
                        stmt.1.clone(),
                    ),
                    then: Box::new((
                        Statement::Block((
                            vec![
                                body.clone(),
                                (
                                    Statement::Assign(
                                        (
                                            AssignmentTarget::Var((var.clone().0, var.1.clone())),
                                            var.1.clone(),
                                        ),
                                        (
                                            Expr::Binary {
                                                lhs: Box::new((
                                                    Expr::Var((var.clone().0, var.1.clone())),
                                                    var.1.clone(),
                                                )),
                                                op: (BinaryOp::Add, stmt.1.clone()),
                                                rhs: Box::new((
                                                    Expr::Literal((
                                                        Literal::Int(1),
                                                        stmt.1.clone(),
                                                    )),
                                                    stmt.1.clone(),
                                                )),
                                            },
                                            stmt.1.clone(),
                                        ),
                                    ),
                                    stmt.1.clone(),
                                ),
                            ],
                            stmt.1.clone(),
                        )),
                        stmt.1.clone(),
                    )),
                    else_: Some(Box::new((Statement::Break, stmt.1.clone()))),
                };

                Statement::Block((
                    vec![
                        (Statement::VarDecl(var.clone(), None, start), var.1),
                        (
                            Statement::Loop(Box::new((loop_body, body.1.clone()))),
                            body.1,
                        ),
                    ],
                    stmt.1.clone(),
                ))
            }
            ast::Statement::Break => Statement::Break,
            ast::Statement::Continue => Statement::Continue,
            ast::Statement::Loop(stmt) => Statement::Loop(Box::new(lower_statement(*stmt))),
        },
        stmt.1,
    )
}

fn lower_expr(expr: Spanned<ast::Expr>) -> Spanned<Expr> {
    (
        match expr.0 {
            ast::Expr::Error => unreachable!(),
            ast::Expr::Literal(l) => Expr::Literal(lower_literal(l)),
            ast::Expr::Var(name) => Expr::Var(name),
            ast::Expr::List(exprs) => {
                Expr::List((exprs.0.into_iter().map(lower_expr).collect(), exprs.1))
            }
            ast::Expr::Binary { lhs, op, rhs } => Expr::Binary {
                lhs: Box::new(lower_expr(*lhs)),
                op: lower_binary_op(op),
                rhs: Box::new(lower_expr(*rhs)),
            },
            ast::Expr::Prefix { op, expr } => Expr::Prefix {
                op: lower_prefix_op(op),
                expr: Box::new(lower_expr(*expr)),
            },
            ast::Expr::Postfix { expr, op } => Expr::Postfix {
                expr: Box::new(lower_expr(*expr)),
                op: lower_postfix_op(op),
            },
        },
        expr.1.clone(),
    )
}

fn lower_binary_op(op: Spanned<ast::BinaryOp>) -> Spanned<BinaryOp> {
    (
        match op.0 {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::Neq => BinaryOp::Neq,
            ast::BinaryOp::Lt => BinaryOp::Lt,
            ast::BinaryOp::Gt => BinaryOp::Gt,
            ast::BinaryOp::Lte => BinaryOp::Lte,
            ast::BinaryOp::Gte => BinaryOp::Gte,
        },
        op.1,
    )
}

fn lower_prefix_op(op: Spanned<ast::PrefixOp>) -> Spanned<PrefixOp> {
    (
        match op.0 {
            ast::PrefixOp::Pos => PrefixOp::Pos,
            ast::PrefixOp::Neg => PrefixOp::Neg,
        },
        op.1,
    )
}

fn lower_postfix_op(op: Spanned<ast::PostfixOp>) -> Spanned<PostfixOp> {
    (
        match op.0 {
            ast::PostfixOp::Error => unreachable!(),
            ast::PostfixOp::Call(exprs) => {
                PostfixOp::Call((exprs.0.into_iter().map(lower_expr).collect(), exprs.1))
            }
            ast::PostfixOp::Index(expr) => {
                let expr = Box::new(lower_expr(*expr));
                PostfixOp::Index(expr)
            }
        },
        op.1.clone(),
    )
}

fn lower_literal(lit: Spanned<ast::Literal>) -> Spanned<Literal> {
    (
        match lit.0 {
            ast::Literal::Unit => Literal::Unit,
            ast::Literal::Int(i) => Literal::Int(i),
            ast::Literal::Float(f) => Literal::Float(f),
            ast::Literal::Bool(b) => Literal::Bool(b),
        },
        lit.1,
    )
}

fn lower_assignment_target(target: Spanned<ast::AssignmentTarget>) -> Spanned<AssignmentTarget> {
    (
        match target.0 {
            ast::AssignmentTarget::Var(i) => AssignmentTarget::Var(i),
            ast::AssignmentTarget::Index(target, expr) => {
                let target = lower_assignment_target(*target);
                let expr = lower_expr(*expr);

                AssignmentTarget::Index(Box::new(target), Box::new(expr))
            }
        },
        target.1.clone(),
    )
}

fn lower_type(ty: Spanned<ast::Type>) -> Spanned<Type> {
    (
        match ty.0 {
            ast::Type::Unit => Type::Unit,
            ast::Type::Ident(i) => Type::Ident(i),
            ast::Type::Int => Type::Int,
            ast::Type::Float => Type::Float,
            ast::Type::Bool => Type::Bool,
            ast::Type::List(l) => Type::List(Box::new(lower_type(*l))),
        },
        ty.1.clone(),
    )
}
