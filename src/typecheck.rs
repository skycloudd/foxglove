use crate::ast::{self, Ast};
use crate::error::{Error, TypecheckError};
use crate::typed_ast::*;
use crate::Spanned;
use rustc_hash::FxHashMap;
use std::hash::Hash;

pub fn typecheck(ast: Spanned<Ast>) -> Result<Spanned<TypedAst>, Vec<Error>> {
    let mut checker = Typechecker::new();

    checker.typecheck_ast(ast)
}

struct Typechecker<'a> {
    engine: Engine,
    bindings: Scopes<&'a str, TypeId>,
}

impl<'a> Typechecker<'a> {
    fn new() -> Self {
        Self {
            engine: Engine::new(),
            bindings: Scopes::new(),
        }
    }

    fn typecheck_ast<'src: 'a>(
        &mut self,
        ast: Spanned<Ast<'src>>,
    ) -> Result<Spanned<TypedAst<'src>>, Vec<Error>> {
        let mut statements = vec![];
        let mut errors = vec![];

        self.bindings.push_scope();

        for statement in ast.0.statements.0 {
            match self.typecheck_statement(statement) {
                Ok(stmt) => statements.push(stmt.0),
                Err(err) => errors.push(err),
            }
        }

        self.bindings.pop_scope();

        if errors.is_empty() {
            Ok((TypedAst { statements }, ast.1))
        } else {
            Err(errors)
        }
    }

    fn typecheck_statement<'src: 'a>(
        &mut self,
        stmt: Spanned<ast::Statement<'src>>,
    ) -> Result<Spanned<Statement<'src>>, Error> {
        Ok((
            match stmt.0 {
                ast::Statement::Error => Statement::Error,
                ast::Statement::Expr(expr) => {
                    let expr = self.typecheck_expr(expr)?;

                    Statement::Expr(expr.0)
                }
                ast::Statement::Block(statements) => {
                    self.bindings.push_scope();

                    let statements = statements
                        .0
                        .into_iter()
                        .map(|stmt| self.typecheck_statement(stmt))
                        .map(|stmt| stmt.map(|stmt| stmt.0))
                        .collect::<Result<Vec<_>, _>>()?;

                    self.bindings.pop_scope();

                    Statement::Block(statements)
                }
                ast::Statement::Let { name, ty, value } => {
                    let value = self.typecheck_expr(value)?;
                    let value_ty = self.engine.insert(type_to_typeinfo((value.0.ty, value.1)));

                    let ty = ty.map(|ty| self.lower_type(ty));

                    let ty = match ty {
                        Some(ty) => {
                            let ty = self.engine.insert(type_to_typeinfo(ty));

                            self.engine.unify(value_ty, ty)?;

                            ty
                        }
                        None => value_ty,
                    };

                    self.bindings.insert(name.0, ty);

                    Statement::Let {
                        name: name.0,
                        ty: self.engine.reconstruct(ty)?.0,
                        value: value.0,
                    }
                }
                ast::Statement::Assign { name, value } => {
                    let value = self.typecheck_expr(value)?;
                    let value_ty = self.engine.insert(type_to_typeinfo((value.0.ty, value.1)));

                    let ty =
                        self.bindings
                            .get(&name.0)
                            .ok_or(TypecheckError::UndefinedVariable {
                                name: name.0.to_string(),
                                span: name.1,
                            })?;

                    self.engine.unify(value_ty, *ty)?;

                    Statement::Assign {
                        name: name.0,
                        value: value.0,
                    }
                }
                ast::Statement::Print(expr) => {
                    let expr = match expr {
                        Some(expr) => expr,
                        None => (
                            ast::Expr::Literal((
                                ast::Literal::Unit,
                                (stmt.1.end..stmt.1.end).into(),
                            )),
                            (stmt.1.end..stmt.1.end).into(),
                        ),
                    };

                    let expr = self.typecheck_expr(expr)?;

                    Statement::Print(expr.0)
                }
                ast::Statement::Loop(stmt) => {
                    let stmt = self.typecheck_statement(*stmt)?;

                    Statement::Loop(Box::new(stmt.0))
                }
                ast::Statement::Continue => Statement::Continue,
                ast::Statement::Break => Statement::Break,
                ast::Statement::Conditional {
                    condition,
                    then,
                    otherwise,
                } => {
                    let condition = self.typecheck_expr(condition)?;
                    let condition_ty = self
                        .engine
                        .insert(type_to_typeinfo((condition.0.ty, condition.1)));
                    let bool_ty = self
                        .engine
                        .insert(type_to_typeinfo((Type::Bool, condition.1)));

                    self.engine.unify(condition_ty, bool_ty)?;

                    let then = self.typecheck_statement(*then)?;

                    let otherwise = match otherwise {
                        Some(otherwise) => {
                            let otherwise = self.typecheck_statement(*otherwise)?;

                            Some(Box::new(otherwise.0))
                        }
                        None => None,
                    };

                    Statement::Conditional {
                        condition: condition.0,
                        then: Box::new(then.0),
                        otherwise,
                    }
                }
                ast::Statement::While { condition, body } => {
                    let condition = self.typecheck_expr(condition)?;

                    let condition_ty = self
                        .engine
                        .insert(type_to_typeinfo((condition.0.ty, condition.1)));
                    let bool_ty = self
                        .engine
                        .insert(type_to_typeinfo((Type::Bool, condition.1)));

                    self.engine.unify(condition_ty, bool_ty)?;

                    let body = self.typecheck_statement(*body)?;

                    Statement::Loop(Box::new(Statement::Conditional {
                        condition: condition.0,
                        then: Box::new(body.0),
                        otherwise: Some(Box::new(Statement::Break)),
                    }))
                }
            },
            stmt.1,
        ))
    }

    fn typecheck_expr<'src>(
        &mut self,
        expr: Spanned<ast::Expr<'src>>,
    ) -> Result<Spanned<Expr<'src>>, Error> {
        Ok((
            match expr.0 {
                ast::Expr::Error => Expr {
                    expr: ExprKind::Error,
                    ty: Type::Unit,
                },
                ast::Expr::Var(name) => {
                    let ty =
                        self.bindings
                            .get(&name.0)
                            .ok_or(TypecheckError::UndefinedVariable {
                                name: name.0.to_string(),
                                span: name.1,
                            })?;

                    Expr {
                        expr: ExprKind::Var(name.0),
                        ty: self.engine.reconstruct(*ty)?.0,
                    }
                }
                ast::Expr::Literal(literal) => {
                    let literal = self.lower_literal(literal);

                    Expr {
                        expr: ExprKind::Literal(literal.0),
                        ty: literal.0.ty(),
                    }
                }
                ast::Expr::Prefix { op, expr } => {
                    let op = self.lower_prefix_operator(op);

                    let expr = self.typecheck_expr(*expr)?;
                    let expr_id = self.engine.insert(type_to_typeinfo((expr.0.ty, expr.1)));
                    let expr_ty = self.engine.reconstruct(expr_id)?;

                    let ty = expr_ty.0.get_prefix_type(op)?;

                    Expr {
                        expr: ExprKind::Prefix {
                            op: op.0,
                            expr: Box::new(expr.0),
                        },
                        ty,
                    }
                }
                ast::Expr::Binary { op, lhs, rhs } => {
                    let op = self.lower_binary_operator(op);

                    let lhs = self.typecheck_expr(*lhs)?;
                    let lhs_id = self.engine.insert(type_to_typeinfo((lhs.0.ty, lhs.1)));

                    let rhs = self.typecheck_expr(*rhs)?;
                    let rhs_id = self.engine.insert(type_to_typeinfo((rhs.0.ty, rhs.1)));

                    self.engine.unify(lhs_id, rhs_id)?;

                    let lhs_ty = self.engine.reconstruct(lhs_id)?;
                    let rhs_ty = self.engine.reconstruct(rhs_id)?;

                    let ty = lhs_ty.0.get_binary_type(op, &rhs_ty.0)?;

                    Expr {
                        expr: ExprKind::Binary {
                            op: op.0,
                            lhs: Box::new(lhs.0),
                            rhs: Box::new(rhs.0),
                        },
                        ty,
                    }
                }
            },
            expr.1,
        ))
    }

    fn lower_literal(&self, literal: Spanned<ast::Literal>) -> Spanned<Literal> {
        (
            match literal.0 {
                ast::Literal::Int(n) => Literal::Int(n),
                ast::Literal::Bool(b) => Literal::Bool(b),
                ast::Literal::Unit => Literal::Unit,
            },
            literal.1,
        )
    }

    fn lower_prefix_operator(&self, op: Spanned<ast::PrefixOp>) -> Spanned<PrefixOp> {
        (
            match op.0 {
                ast::PrefixOp::Negate => PrefixOp::Negate,
            },
            op.1,
        )
    }

    fn lower_binary_operator(&self, op: Spanned<ast::BinOp>) -> Spanned<BinOp> {
        (
            match op.0 {
                ast::BinOp::Add => BinOp::Add,
                ast::BinOp::Subtract => BinOp::Subtract,
                ast::BinOp::Multiply => BinOp::Multiply,
                ast::BinOp::Divide => BinOp::Divide,
                ast::BinOp::Equals => BinOp::Equals,
                ast::BinOp::NotEquals => BinOp::NotEquals,
                ast::BinOp::LessThan => BinOp::LessThan,
                ast::BinOp::LessThanOrEqual => BinOp::LessThanOrEqual,
                ast::BinOp::GreaterThan => BinOp::GreaterThan,
                ast::BinOp::GreaterThanOrEqual => BinOp::GreaterThanOrEqual,
                ast::BinOp::LogicalAnd => BinOp::LogicalAnd,
                ast::BinOp::LogicalOr => BinOp::LogicalOr,
            },
            op.1,
        )
    }

    fn lower_type(&self, ty: Spanned<ast::Type>) -> Spanned<Type> {
        (
            match ty.0 {
                ast::Type::Int => Type::Int,
                ast::Type::Bool => Type::Bool,
                ast::Type::Unit => Type::Unit,
            },
            ty.1,
        )
    }
}

struct Engine {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: FxHashMap::default(),
        }
    }

    fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
        let var_a = self.vars[&a];
        let var_b = self.vars[&b];

        match (var_a.0, var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),

            (TypeInfo::Unknown, _) => {
                self.vars.insert(a, (TypeInfo::Ref(b), var_b.1));
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars.insert(b, (TypeInfo::Ref(a), var_a.1));
                Ok(())
            }

            (TypeInfo::Int, TypeInfo::Int) => Ok(()),

            (TypeInfo::Bool, TypeInfo::Bool) => Ok(()),

            (a, b) => Err(TypecheckError::TypeMismatch {
                span1: var_a.1,
                span2: var_b.1,
                ty1: a,
                ty2: b,
            }
            .into()),
        }
    }

    fn reconstruct(&mut self, id: TypeId) -> Result<Spanned<Type>, Error> {
        let var = self.vars[&id];

        Ok((
            match var.0 {
                TypeInfo::Unknown => {
                    return Err(TypecheckError::CannotInferType { span: var.1 }.into())
                }
                TypeInfo::Ref(id) => self.reconstruct(id)?.0,
                TypeInfo::Int => Type::Int,
                TypeInfo::Bool => Type::Bool,
                TypeInfo::Unit => Type::Unit,
            },
            var.1,
        ))
    }
}

type TypeId = usize;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Int,
    Bool,
    Unit,
}

fn type_to_typeinfo(ty: Spanned<Type>) -> Spanned<TypeInfo> {
    (
        match ty.0 {
            Type::Int => TypeInfo::Int,
            Type::Bool => TypeInfo::Bool,
            Type::Unit => TypeInfo::Unit,
        },
        ty.1,
    )
}

#[derive(Clone, Debug)]
pub struct Scopes<K, V>(Vec<FxHashMap<K, V>>);

impl<K: Eq + Hash, V> Scopes<K, V> {
    pub fn new() -> Scopes<K, V> {
        Scopes(vec![FxHashMap::default()])
    }

    pub fn push_scope(&mut self) {
        self.0.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.0.last_mut().unwrap().insert(k, v);
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        for scope in self.0.iter().rev() {
            if let Some(v) = scope.get(k) {
                return Some(v);
            }
        }

        None
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        for scope in self.0.iter_mut().rev() {
            if let Some(v) = scope.get_mut(k) {
                return Some(v);
            }
        }

        None
    }
}

impl Type {
    fn get_prefix_type(&self, op: Spanned<PrefixOp>) -> Result<Type, Error> {
        match self {
            Type::Int => match op.0 {
                PrefixOp::Negate => Ok(Type::Int),
            },
            Type::Bool | Type::Unit => Err(TypecheckError::CannotApplyUnaryOperator {
                span: op.1,
                op: op.0,
                ty: *self,
            }
            .into()),
        }
    }

    fn get_binary_type(&self, op: Spanned<BinOp>, rhs: &Type) -> Result<Type, Error> {
        let lhs = self;

        match (lhs, rhs) {
            (Type::Int, Type::Int) => match op.0 {
                BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => Ok(Type::Int),
                BinOp::Equals
                | BinOp::NotEquals
                | BinOp::LessThan
                | BinOp::LessThanOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanOrEqual => Ok(Type::Bool),
                BinOp::LogicalAnd | BinOp::LogicalOr => {
                    Err(TypecheckError::CannotApplyBinaryOperator {
                        span: op.1,
                        op: op.0,
                        ty1: *lhs,
                        ty2: *rhs,
                    }
                    .into())
                }
            },
            (Type::Bool, Type::Bool) => match op.0 {
                BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => {
                    Err(TypecheckError::CannotApplyBinaryOperator {
                        span: op.1,
                        op: op.0,
                        ty1: *lhs,
                        ty2: *rhs,
                    }
                    .into())
                }
                BinOp::Equals
                | BinOp::NotEquals
                | BinOp::LessThan
                | BinOp::LessThanOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanOrEqual
                | BinOp::LogicalAnd
                | BinOp::LogicalOr => Ok(Type::Bool),
            },
            (lhs, rhs) => Err(TypecheckError::CannotApplyBinaryOperator {
                span: op.1,
                op: op.0,
                ty1: *lhs,
                ty2: *rhs,
            }
            .into()),
        }
    }
}

impl Literal {
    fn ty(&self) -> Type {
        match self {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            Literal::Unit => Type::Unit,
        }
    }
}
