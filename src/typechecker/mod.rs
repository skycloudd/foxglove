use self::typed_ast::*;
use crate::error::{Error, TypecheckError};
use crate::parser::ast::{self, Ast};
use crate::Spanned;
use std::collections::HashMap;
use std::hash::Hash;

pub mod typed_ast;

pub fn typecheck(ast: Spanned<Ast>) -> Result<Spanned<TypedAst>, Vec<Error>> {
    let mut checker = Typechecker::new();

    checker.typecheck_ast(ast)
}

struct Typechecker<'a> {
    engine: Engine,
    fns: HashMap<&'a str, (Vec<TypeId>, TypeId)>,
    bindings: Scopes<&'a str, TypeId>,
    current_fn: Option<&'a str>,
    is_in_loop: bool,
}

impl<'a> Typechecker<'a> {
    fn new() -> Self {
        Self {
            engine: Engine::new(),
            fns: HashMap::new(),
            bindings: Scopes::new(),
            current_fn: None,
            is_in_loop: false,
        }
    }

    fn typecheck_ast<'src: 'a>(
        &mut self,
        ast: Spanned<Ast<'src>>,
    ) -> Result<Spanned<TypedAst<'src>>, Vec<Error>> {
        let mut toplevels = HashMap::new();
        let mut errors = vec![];

        for toplevel in ast.0.toplevels.0 {
            match toplevel.0 {
                ast::TopLevel::Function(function) => {
                    let name = function.0.name.0;
                    self.current_fn = Some(name);

                    let (func, tc_errs) = self.typecheck_function(function);

                    toplevels.insert(name, TopLevel::Function(func.0));

                    errors.extend(tc_errs);
                }
            }
        }

        self.current_fn = None;

        if toplevels.get("main").is_none() {
            errors.push(TypecheckError::MissingMainFunction((ast.1.end..ast.1.end).into()).into());
        }

        if errors.is_empty() {
            Ok((TypedAst { toplevels }, ast.1))
        } else {
            Err(errors)
        }
    }

    fn typecheck_function<'src: 'a>(
        &mut self,
        function: Spanned<ast::Function<'src>>,
    ) -> (Spanned<Function<'src>>, Vec<Error>) {
        let mut errors = vec![];

        self.bindings.push_scope();

        let mut attrs = vec![];

        for attr in function.0.attrs.0 {
            let value =
                attr.0
                    .value
                    .as_ref()
                    .map(|value| match self.typecheck_expr(value.clone()) {
                        Ok(value) => value.0,
                        Err(err) => {
                            errors.push(err);

                            Expr {
                                expr: ExprKind::Error,
                                ty: Type::Unit,
                            }
                        }
                    });

            let kind = match attr.0.name.0 {
                "export" => AttrKind::Export,
                _ => {
                    errors.push(
                        TypecheckError::UnknownAttribute {
                            span: attr.0.name.1,
                            name: attr.0.name.0.to_string(),
                        }
                        .into(),
                    );

                    AttrKind::Error
                }
            };

            match kind {
                AttrKind::Error => {}
                AttrKind::Export => {
                    if let Some((_, span)) = attr.0.value {
                        errors.push(
                            TypecheckError::AttributeHasValue {
                                span,
                                name: attr.0.name.0.to_string(),
                            }
                            .into(),
                        );
                    }
                }
            }

            attrs.push(Attr { kind, value });
        }

        let mut params = vec![];
        let mut param_types = vec![];

        for param in &function.0.params.0 {
            let ty = self.lower_type(param.0.ty);
            let ty_id = self.engine.insert(type_to_typeinfo(ty));

            param_types.push(ty_id);

            self.bindings.insert(param.0.name.0, ty_id);

            params.push(Param {
                name: param.0.name.0,
                ty: ty.0,
            });
        }

        let ret_ty_type = self.lower_type(function.0.ty);
        let ret_ty = self.engine.insert(type_to_typeinfo(ret_ty_type));

        if function.0.name.0 == "main" {
            if !param_types.is_empty() {
                errors.push(
                    TypecheckError::MainFunctionHasParameters {
                        span: function.0.params.1,
                    }
                    .into(),
                );
            }

            if ret_ty_type.0 != Type::Int {
                errors.push(
                    TypecheckError::MainFunctionHasWrongReturnType {
                        span: function.0.ty.1,
                        expected: Type::Int,
                        found: ret_ty_type.0,
                    }
                    .into(),
                );
            }
        }

        self.fns.insert(function.0.name.0, (param_types, ret_ty));

        let mut body = vec![];

        for stmt in function.0.body.0 {
            body.push(match self.typecheck_statement(stmt) {
                Ok(stmt) => stmt.0,
                Err(err) => {
                    errors.push(err);

                    Statement::Error
                }
            });
        }

        self.bindings.pop_scope();

        (
            (
                Function {
                    attrs,
                    name: function.0.name.0,
                    params,
                    ty: ret_ty_type.0,
                    body,
                },
                function.1,
            ),
            errors,
        )
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
                ast::Statement::Loop(stmt) => {
                    self.is_in_loop = true;

                    let stmt = self.typecheck_statement(*stmt)?;

                    self.is_in_loop = false;

                    Statement::Loop(Box::new(stmt.0))
                }
                ast::Statement::Continue => {
                    if !self.is_in_loop {
                        return Err(TypecheckError::ContinueOutsideOfLoop(stmt.1).into());
                    } else {
                        Statement::Continue
                    }
                }
                ast::Statement::Break => {
                    if !self.is_in_loop {
                        return Err(TypecheckError::BreakOutsideOfLoop(stmt.1).into());
                    } else {
                        Statement::Break
                    }
                }
                ast::Statement::Return(expr) => {
                    let ret_expr = match expr {
                        Some(expr) => self.typecheck_expr(expr)?,
                        None => (
                            Expr {
                                expr: ExprKind::Literal(Literal::Unit),
                                ty: Type::Unit,
                            },
                            stmt.1,
                        ),
                    };

                    let ret_ty = self.fns.get(self.current_fn.unwrap()).unwrap().1;

                    let ret_expr_ty = self
                        .engine
                        .insert(type_to_typeinfo((ret_expr.0.ty, ret_expr.1)));

                    self.engine.unify(ret_ty, ret_expr_ty)?;

                    Statement::Return(ret_expr.0)
                }
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
                ast::Expr::Call { name, args } => {
                    let args = args
                        .0
                        .into_iter()
                        .map(|arg| self.typecheck_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    let (param_types, ret_ty) =
                        self.fns
                            .get(&name.0)
                            .ok_or(TypecheckError::UndefinedFunction {
                                name: name.0.to_string(),
                                span: name.1,
                            })?;

                    if args.len() != param_types.len() {
                        return Err(TypecheckError::IncorrectNumberOfArguments {
                            span: name.1,
                            expected: param_types.len(),
                            found: args.len(),
                        }
                        .into());
                    }

                    for (arg, param_ty) in args.iter().zip(param_types.iter()) {
                        let arg_id = self.engine.insert(type_to_typeinfo((arg.0.ty, arg.1)));

                        self.engine.unify(arg_id, *param_ty)?;
                    }

                    let ret_ty = self.engine.reconstruct(*ret_ty)?;

                    Expr {
                        expr: ExprKind::Call {
                            name: name.0,
                            args: args.into_iter().map(|arg| arg.0).collect(),
                        },
                        ty: ret_ty.0,
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
    vars: HashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: HashMap::new(),
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

            (TypeInfo::Unit, TypeInfo::Unit) => Ok(()),

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
pub struct Scopes<K, V>(Vec<HashMap<K, V>>);

impl<K: Eq + Hash, V> Scopes<K, V> {
    pub fn new() -> Scopes<K, V> {
        Scopes(vec![])
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::new());
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
