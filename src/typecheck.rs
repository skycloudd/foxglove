use crate::error::Report;
use crate::hir::{self, Hir};
use crate::typed_hir::{
    AssignmentTarget, AssignmentTargetKind, Attribute, BinaryOp, Expr, ExprKind, FunctionSignature,
    Ident, Item, Literal, Param, PostfixOp, PrefixOp, Statement, Type, TypedHir,
};
use crate::Spanned;
use std::collections::HashMap;

pub fn typecheck(hir: Spanned<Hir>) -> Result<Spanned<TypedHir>, Report> {
    let mut engine = Engine::default();

    let mut items = vec![];

    let mut bindings = HashMap::new();

    for item in hir.0.items.0 {
        items.push(tc_item(&mut engine, &mut bindings, item)?);
    }

    let thir = (
        TypedHir {
            items: (items, hir.0.items.1),
        },
        hir.1,
    );

    Ok(thir)
}

fn tc_item(
    engine: &mut Engine,
    bindings: &mut HashMap<Ident, TypeId>,
    item: Spanned<hir::Item>,
) -> Result<Spanned<Item>, Report> {
    Ok((
        match item.0 {
            hir::Item::Fn { attrs, sig, body } => {
                let attrs = tc_attributes(engine, bindings, attrs)?;

                let sig = tc_function_signature(sig)?;

                let mut args = vec![];

                let ret = type_to_typeinfo(sig.0.ret_ty.clone())?;

                for param in &sig.0.params.0 {
                    let arg_ty = type_to_typeinfo(param.0.ty.clone())?;

                    args.push(arg_ty);
                }

                bindings.insert(
                    sig.0.name.0.clone(),
                    engine.insert((
                        TypeInfo::Function {
                            args: args.clone(),
                            ret: Box::new(ret),
                        },
                        sig.1.clone(),
                    )),
                );

                let mut bindings = bindings.clone();

                for (i, param) in sig.0.params.0.iter().enumerate() {
                    bindings.insert(
                        param.0.name.0.clone(),
                        engine.insert((args[i].0.clone(), param.1.clone())),
                    );
                }

                let body = tc_statement(engine, &mut bindings, &sig.0.ret_ty, body)?;

                Item::Fn { attrs, sig, body }
            }
        },
        item.1,
    ))
}

fn tc_attributes(
    engine: &mut Engine,
    bindings: &mut HashMap<Ident, TypeId>,
    attrs: Spanned<Vec<Spanned<hir::Attribute>>>,
) -> Result<Spanned<Vec<Spanned<Attribute>>>, Report> {
    let mut tcd_attrs = vec![];

    for attr in attrs.0 {
        tcd_attrs.push((
            Attribute {
                name: attr.0.name,
                value: tc_expr(engine, bindings, attr.0.value)?,
            },
            attr.1,
        ))
    }

    Ok((tcd_attrs, attrs.1))
}

fn tc_function_signature(
    sig: Spanned<hir::FunctionSignature>,
) -> Result<Spanned<FunctionSignature>, Report> {
    let mut params = vec![];

    for param in sig.0.params.0 {
        params.push(tc_param(param)?);
    }

    Ok((
        FunctionSignature {
            name: sig.0.name,
            params: (params, sig.0.params.1),
            ret_ty: tc_type(sig.0.ret_ty)?,
        },
        sig.1,
    ))
}

fn tc_param(param: Spanned<hir::Param>) -> Result<Spanned<Param>, Report> {
    Ok((
        Param {
            name: param.0.name,
            ty: tc_type(param.0.ty)?,
        },
        param.1,
    ))
}

fn tc_type(ty: Spanned<hir::Type>) -> Result<Spanned<Type>, Report> {
    Ok((
        match ty.0 {
            hir::Type::Unit => Type::Unit,
            hir::Type::Ident(ident) => Type::Ident(ident),
            hir::Type::Int => Type::Int,
            hir::Type::Float => Type::Float,
            hir::Type::Bool => Type::Bool,
            hir::Type::List(ty) => Type::List(Box::new(tc_type(*ty)?)),
        },
        ty.1,
    ))
}

fn tc_statement(
    engine: &mut Engine,
    bindings: &mut HashMap<Ident, TypeId>,
    current_fn_ret: &Spanned<Type>,
    stmt: Spanned<hir::Statement>,
) -> Result<Spanned<Statement>, Report> {
    Ok((
        match stmt.0 {
            hir::Statement::Block(stmts) => {
                let mut bindings = bindings.clone();

                let mut body = vec![];

                for stmt in stmts.0 {
                    body.push(tc_statement(engine, &mut bindings, current_fn_ret, stmt)?);
                }

                Statement::Block((body, stmts.1))
            }
            hir::Statement::Expr(expr) => Statement::Expr(tc_expr(engine, bindings, expr)?),
            hir::Statement::VarDecl(name, ty, expr) => {
                let ty_ty = match ty {
                    Some(ty) => type_to_typeinfo(tc_type(ty)?)?,
                    None => (TypeInfo::Unknown, stmt.1.clone()),
                };
                let ty_ty = engine.insert(ty_ty);

                let expr = tc_expr(engine, bindings, expr)?;
                let expr_ty = type_to_typeinfo((expr.0.ty.clone(), expr.1.clone()))?;
                let expr_ty = engine.insert(expr_ty);

                engine.unify(ty_ty, expr_ty)?;

                bindings.insert(name.0.clone(), ty_ty);

                Statement::VarDecl(name, engine.reconstruct(ty_ty)?, expr)
            }
            hir::Statement::Assign(target, expr) => {
                let target = tc_assignment_target(engine, bindings, target)?;
                let expr = tc_expr(engine, bindings, expr)?;

                let target_ty = type_to_typeinfo((target.0.ty.clone(), target.1.clone()))?;
                let target_ty = engine.insert(target_ty);

                let expr_ty = type_to_typeinfo((expr.0.ty.clone(), expr.1.clone()))?;
                let expr_ty = engine.insert(expr_ty);

                engine.unify(target_ty, expr_ty)?;

                match target.0.kind {
                    AssignmentTargetKind::Var(ref ident) => {
                        bindings.insert(ident.0.clone(), target_ty);
                    }
                    AssignmentTargetKind::Index { .. } => {}
                }

                Statement::Assign(target, expr)
            }
            hir::Statement::Return(expr) => {
                let expr = match expr {
                    Some(expr) => Some(tc_expr(engine, bindings, expr)?),
                    None => None,
                };

                let expr_ty = type_to_typeinfo(match expr {
                    Some(ref expr) => (expr.0.ty.clone(), expr.1.clone()),
                    None => (Type::Unit, stmt.1.clone()),
                })?;
                let expr_ty = engine.insert(expr_ty);

                let function_return_ty = type_to_typeinfo(current_fn_ret.clone())?;
                let function_return_ty = engine.insert(function_return_ty);

                engine.unify(expr_ty, function_return_ty)?;

                Statement::Return(expr)
            }
            hir::Statement::IfElse { cond, then, else_ } => {
                let cond = tc_expr(engine, bindings, cond)?;
                let then = tc_statement(engine, bindings, current_fn_ret, *then)?;
                let else_ = match else_ {
                    Some(else_) => Some(Box::new(tc_statement(
                        engine,
                        bindings,
                        current_fn_ret,
                        *else_,
                    )?)),
                    None => None,
                };

                let cond_ty = type_to_typeinfo((cond.0.ty.clone(), cond.1.clone()))?;
                let cond_ty = engine.insert(cond_ty);

                let bool_ty = engine.insert((TypeInfo::Bool, cond.1.clone()));

                engine.unify(cond_ty, bool_ty)?;

                Statement::IfElse {
                    cond,
                    then: Box::new(then),
                    else_,
                }
            }
            hir::Statement::Break => Statement::Break,
            hir::Statement::Continue => Statement::Continue,
            hir::Statement::Loop(stmt) => Statement::Loop(Box::new(tc_statement(
                engine,
                bindings,
                current_fn_ret,
                *stmt,
            )?)),
        },
        stmt.1.clone(),
    ))
}

fn tc_expr(
    engine: &mut Engine,
    bindings: &HashMap<Ident, TypeId>,
    expr: Spanned<hir::Expr>,
) -> Result<Spanned<Expr>, Report> {
    Ok((
        match expr.0 {
            hir::Expr::Literal(lit) => Expr {
                ty: tc_type((lit.0.ty(), lit.1.clone()))?.0,
                kind: ExprKind::Literal(tc_literal(lit)?),
            },
            hir::Expr::Var(name) => {
                let ty = match bindings.get(&name.0) {
                    Some(id) => engine.reconstruct(*id)?.0,
                    None => todo!("error cant find var {} at {:?}", name.0, name.1),
                };

                Expr {
                    ty,
                    kind: ExprKind::Var(name),
                }
            }
            hir::Expr::List(list) => {
                let mut elements = vec![];

                for element in list.0 {
                    elements.push(tc_expr(engine, bindings, element)?);
                }

                let inner_ty = TypeInfo::Unknown;
                let inner_ty = engine.insert((inner_ty, list.1.clone()));

                for element in &elements {
                    let element_ty = type_to_typeinfo((element.0.ty.clone(), element.1.clone()))?;
                    let element_ty = engine.insert(element_ty);

                    engine.unify(inner_ty, element_ty)?;
                }

                Expr {
                    ty: Type::List(Box::new(engine.reconstruct(inner_ty)?)),
                    kind: ExprKind::List((elements, list.1)),
                }
            }
            hir::Expr::Binary { lhs, op, rhs } => {
                let lhs = tc_expr(engine, bindings, *lhs)?;
                let op = tc_binary_op(op)?;
                let rhs = tc_expr(engine, bindings, *rhs)?;

                let lhs_ty = type_to_typeinfo((lhs.0.ty.clone(), lhs.1.clone()))?;
                let lhs_ty = engine.insert(lhs_ty);
                let rhs_ty = type_to_typeinfo((rhs.0.ty.clone(), rhs.1.clone()))?;
                let rhs_ty = engine.insert(rhs_ty);

                engine.unify(lhs_ty, rhs_ty)?;

                let ty = match op.0 {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        engine.reconstruct(lhs_ty)?
                    }
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte => {
                        let bool_ty = engine.insert((TypeInfo::Bool, expr.1.clone()));
                        engine.reconstruct(bool_ty)?
                    }
                };

                Expr {
                    ty: ty.0,
                    kind: ExprKind::Binary {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    },
                }
            }
            hir::Expr::Prefix { op, expr } => {
                let op = tc_prefix_op(op)?;
                let expr = tc_expr(engine, bindings, *expr)?;

                let expr_ty = type_to_typeinfo((expr.0.ty.clone(), expr.1.clone()))?;
                let expr_ty = engine.insert(expr_ty);

                let ty = match op.0 {
                    PrefixOp::Pos | PrefixOp::Neg => engine.reconstruct(expr_ty)?,
                };

                Expr {
                    ty: ty.0,
                    kind: ExprKind::Prefix {
                        op,
                        expr: Box::new(expr),
                    },
                }
            }
            hir::Expr::Postfix { expr, op } => {
                let expr = tc_expr(engine, bindings, *expr)?;
                let op = tc_postfix_op(engine, bindings, op)?;

                let ty = match op.0 {
                    PostfixOp::Index(_) => match expr.0.ty {
                        Type::List(ref ty) => ty.clone(),
                        _ => todo!("error"),
                    },
                    PostfixOp::Call(ref args) => match expr.0.ty {
                        Type::Function {
                            ref params,
                            ref ret_ty,
                        } => {
                            if params.len() != args.0.len() {
                                todo!("error")
                            } else {
                                for (param, arg) in params.iter().zip(args.0.iter()) {
                                    let param_ty =
                                        type_to_typeinfo((param.0.clone(), param.1.clone()))?;
                                    let param_ty = engine.insert(param_ty);

                                    let arg_ty =
                                        type_to_typeinfo((arg.0.ty.clone(), arg.1.clone()))?;
                                    let arg_ty = engine.insert(arg_ty);

                                    engine.unify(param_ty, arg_ty)?;
                                }
                            }

                            ret_ty.clone()
                        }
                        _ => todo!("error"),
                    },
                };

                Expr {
                    ty: ty.0.clone(),
                    kind: ExprKind::Postfix {
                        expr: Box::new(expr),
                        op,
                    },
                }
            }
        },
        expr.1,
    ))
}

fn tc_binary_op(op: Spanned<hir::BinaryOp>) -> Result<Spanned<BinaryOp>, Report> {
    Ok((
        match op.0 {
            hir::BinaryOp::Add => BinaryOp::Add,
            hir::BinaryOp::Sub => BinaryOp::Sub,
            hir::BinaryOp::Mul => BinaryOp::Mul,
            hir::BinaryOp::Div => BinaryOp::Div,
            hir::BinaryOp::Eq => BinaryOp::Eq,
            hir::BinaryOp::Neq => BinaryOp::Neq,
            hir::BinaryOp::Lt => BinaryOp::Lt,
            hir::BinaryOp::Gt => BinaryOp::Gt,
            hir::BinaryOp::Lte => BinaryOp::Lte,
            hir::BinaryOp::Gte => BinaryOp::Gte,
        },
        op.1,
    ))
}

fn tc_prefix_op(op: Spanned<hir::PrefixOp>) -> Result<Spanned<PrefixOp>, Report> {
    Ok((
        match op.0 {
            hir::PrefixOp::Pos => PrefixOp::Pos,
            hir::PrefixOp::Neg => PrefixOp::Neg,
        },
        op.1,
    ))
}

fn tc_postfix_op(
    engine: &mut Engine,
    bindings: &HashMap<Ident, TypeId>,
    op: Spanned<hir::PostfixOp>,
) -> Result<Spanned<PostfixOp>, Report> {
    Ok((
        match op.0 {
            hir::PostfixOp::Call(args) => {
                let args = (
                    args.0
                        .into_iter()
                        .map(|arg| tc_expr(engine, bindings, arg))
                        .collect::<Result<Vec<_>, _>>()?,
                    args.1,
                );

                PostfixOp::Call(args)
            }
            hir::PostfixOp::Index(index) => {
                let index = tc_expr(engine, bindings, *index)?;

                let index_ty = type_to_typeinfo((index.0.ty.clone(), index.1.clone()))?;
                let index_ty = engine.insert(index_ty);
                let int_ty = engine.insert((TypeInfo::Int, index.1.clone()));

                engine.unify(index_ty, int_ty)?;

                PostfixOp::Index(Box::new(index))
            }
        },
        op.1.clone(),
    ))
}

fn tc_assignment_target(
    engine: &mut Engine,
    bindings: &HashMap<Ident, TypeId>,
    target: Spanned<hir::AssignmentTarget>,
) -> Result<Spanned<AssignmentTarget>, Report> {
    Ok((
        match target.0 {
            hir::AssignmentTarget::Var(ident) => AssignmentTarget {
                ty: match bindings.get(&ident.0) {
                    Some(ty) => engine.reconstruct(*ty)?.0,
                    None => todo!("error var not found"),
                },
                kind: AssignmentTargetKind::Var(ident.clone()),
            },
            hir::AssignmentTarget::Index(target, index) => AssignmentTarget {
                ty: match tc_assignment_target(engine, bindings, *target.clone())?
                    .0
                    .ty
                {
                    Type::List(ty) => match tc_expr(engine, bindings, *index.clone())?.0.ty {
                        Type::Int => ty.0,
                        _ => todo!("error index not int"),
                    },
                    _ => todo!("error target not list"),
                },
                kind: AssignmentTargetKind::Index(
                    Box::new(tc_assignment_target(engine, bindings, *target)?),
                    Box::new(tc_expr(engine, bindings, *index)?),
                ),
            },
        },
        target.1,
    ))
}

fn tc_literal(lit: Spanned<hir::Literal>) -> Result<Spanned<Literal>, Report> {
    Ok((
        match lit.0 {
            hir::Literal::Unit => Literal::Unit,
            hir::Literal::Int(lit) => Literal::Int(lit),
            hir::Literal::Float(lit) => Literal::Float(lit),
            hir::Literal::Bool(lit) => Literal::Bool(lit),
        },
        lit.1,
    ))
}

#[derive(Default)]
struct Engine {
    id_counter: usize,
    vars: HashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    pub fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Report> {
        let var_a = self.vars[&a].clone();
        let var_b = self.vars[&b].clone();

        match (var_a.0.clone(), var_b.0.clone()) {
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

            (TypeInfo::Unit, TypeInfo::Unit) => Ok(()),

            (TypeInfo::Ident(a), TypeInfo::Ident(b)) if a == b => Ok(()),

            (TypeInfo::Int, TypeInfo::Int) => Ok(()),

            (TypeInfo::Float, TypeInfo::Float) => Ok(()),

            (TypeInfo::Bool, TypeInfo::Bool) => Ok(()),

            (TypeInfo::List(a), TypeInfo::List(b)) => {
                let a = self.insert(*a);
                let b = self.insert(*b);

                self.unify(a, b)
            }

            (_, _) => Err(Report::type_conflict(var_a, var_b)),
        }
    }

    pub fn reconstruct(&mut self, id: TypeId) -> Result<Spanned<Type>, Report> {
        let var = self.vars[&id].clone();

        match var.0 {
            TypeInfo::Unknown => Err(Report::cannot_infer_type(var.1)),
            TypeInfo::Ref(id) => Ok((self.reconstruct(id)?.0, var.1)),
            TypeInfo::Unit => Ok((Type::Unit, var.1)),
            TypeInfo::Ident(ident) => Ok((Type::Ident(ident), var.1)),
            TypeInfo::Int => Ok((Type::Int, var.1)),
            TypeInfo::Float => Ok((Type::Float, var.1)),
            TypeInfo::Bool => Ok((Type::Bool, var.1)),
            TypeInfo::List(ty) => Ok((
                Type::List(Box::new({
                    let ty = self.insert(*ty);
                    self.reconstruct(ty)?
                })),
                var.1,
            )),
            TypeInfo::Function { args, ret } => Ok((
                Type::Function {
                    params: args
                        .into_iter()
                        .map(|ty| {
                            let ty = self.insert(ty);
                            self.reconstruct(ty)
                        })
                        .collect::<Result<Vec<Spanned<Type>>, Report>>()?,
                    ret_ty: Box::new({
                        let ret = self.insert(*ret);
                        self.reconstruct(ret)?
                    }),
                },
                var.1,
            )),
        }
    }
}

fn type_to_typeinfo(ty: Spanned<Type>) -> Result<Spanned<TypeInfo>, Report> {
    Ok((
        match ty.0 {
            Type::Unit => TypeInfo::Unit,
            Type::Ident(ident) => TypeInfo::Ident(ident),
            Type::Int => TypeInfo::Int,
            Type::Float => TypeInfo::Float,
            Type::Bool => TypeInfo::Bool,
            Type::List(ty) => TypeInfo::List(Box::new(type_to_typeinfo(*ty)?)),
            Type::Function { params, ret_ty } => TypeInfo::Function {
                args: params
                    .into_iter()
                    .map(type_to_typeinfo)
                    .collect::<Result<Vec<_>, _>>()?,
                ret: Box::new(type_to_typeinfo(*ret_ty)?),
            },
        },
        ty.1.clone(),
    ))
}

type TypeId = usize;

#[derive(Clone, Debug)]
pub enum TypeInfo {
    Unknown,
    Ref(TypeId),
    Unit,
    Ident(Spanned<TypeIdent>),
    Int,
    Float,
    Bool,
    List(Box<Spanned<Self>>),
    Function {
        args: Vec<Spanned<Self>>,
        ret: Box<Spanned<Self>>,
    },
}

pub type TypeIdent = String;

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Unknown => write!(f, "?"),
            TypeInfo::Ref(id) => write!(f, "{{{id}}}"),
            TypeInfo::Unit => write!(f, "()"),
            TypeInfo::Ident(ident) => write!(f, "{}", ident.0),
            TypeInfo::Int => write!(f, "int"),
            TypeInfo::Float => write!(f, "float"),
            TypeInfo::Bool => write!(f, "bool"),
            TypeInfo::List(ty) => write!(f, "[{}]", ty.0),
            TypeInfo::Function { args, ret } => {
                write!(
                    f,
                    "fn |{}|> {}",
                    args.iter()
                        .map(|a| a.0.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    ret.0
                )
            }
        }
    }
}
