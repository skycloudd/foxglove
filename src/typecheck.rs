use std::collections::HashMap;

use crate::error::Report;
use crate::hir::{self, Hir};
use crate::typed_hir::{
    Expr, ExprKind, FunctionSignature, Item, Literal, Param, Statement, Type, TypedHir,
};
use crate::Spanned;

pub fn typecheck(hir: &Spanned<Hir>) -> Result<TypedHir, Report> {
    let mut engine = Engine::default();

    let mut items = vec![];

    for item in &hir.0.items.0 {
        items.push(tc_item(&mut engine, item)?);
    }

    let thir = TypedHir {
        items: (items, hir.0.items.1.clone()),
    };

    Ok(thir)
}

fn tc_item(engine: &mut Engine, item: &Spanned<hir::Item>) -> Result<Spanned<Item>, Report> {
    Ok((
        match item.0 {
            hir::Item::Fn { ref sig, ref body } => Item::Fn {
                sig: tc_function_signature(sig)?,
                body: tc_statement(engine, body)?,
            },
        },
        item.1.clone(),
    ))
}

fn tc_function_signature(
    sig: &Spanned<hir::FunctionSignature>,
) -> Result<Spanned<FunctionSignature>, Report> {
    let mut params = vec![];

    for param in &sig.0.params.0 {
        params.push(tc_param(param)?);
    }

    Ok((
        FunctionSignature {
            name: sig.0.name.clone(),
            params: (params, sig.0.params.1.clone()),
            ret_ty: tc_type(&sig.0.ret_ty)?,
        },
        sig.1.clone(),
    ))
}

fn tc_param(param: &Spanned<hir::Param>) -> Result<Spanned<Param>, Report> {
    Ok((
        Param {
            name: param.0.name.clone(),
            ty: tc_type(&param.0.ty)?,
        },
        param.1.clone(),
    ))
}

fn tc_type(ty: &Spanned<hir::Type>) -> Result<Spanned<Type>, Report> {
    Ok((
        match ty.0 {
            hir::Type::Unit => Type::Unit,
            hir::Type::Ident(ref ident) => Type::Ident(ident.clone()),
            hir::Type::Int => Type::Int,
            hir::Type::Float => Type::Float,
            hir::Type::Bool => Type::Bool,
            hir::Type::List(ref ty) => Type::List(Box::new(tc_type(ty)?)),
        },
        ty.1.clone(),
    ))
}

fn tc_statement(
    engine: &mut Engine,
    stmt: &Spanned<hir::Statement>,
) -> Result<Spanned<Statement>, Report> {
    Ok((
        match stmt.0 {
            hir::Statement::Block(ref stmts) => {
                let mut body = vec![];

                for stmt in &stmts.0 {
                    body.push(tc_statement(engine, stmt)?);
                }

                Statement::Block((body, stmts.1.clone()))
            }
            hir::Statement::Expr(ref expr) => Statement::Expr(tc_expr(engine, expr)?),
            hir::Statement::VarDecl(ref name, ref ty, ref expr) => {
                let ty_ty = match ty {
                    Some(ty) => engine.convert_to_typeinfo(tc_type(ty)?),
                    None => (TypeInfo::Unknown, stmt.1.clone()),
                };
                let expr_ty = tc_expr(engine, expr)?.0.ty;
                let expr_ty = engine.convert_to_typeinfo((expr_ty, expr.1.clone()));

                let ty_id = engine.insert(ty_ty);
                let expr_id = engine.insert(expr_ty);

                engine.unify(ty_id, expr_id)?;

                Statement::VarDecl(
                    name.clone(),
                    engine.reconstruct(ty_id)?,
                    tc_expr(engine, expr)?,
                )
            }
            hir::Statement::Assign(ref target, ref expr) => todo!(),
            hir::Statement::Return(ref expr) => todo!(),
            hir::Statement::IfElse {
                ref cond,
                ref then,
                ref else_,
            } => todo!(),
            hir::Statement::For {
                ref var,
                ref in_,
                ref body,
            } => todo!(),
            hir::Statement::Break => todo!(),
            hir::Statement::Continue => todo!(),
            hir::Statement::Loop(ref stmts) => todo!(),
        },
        stmt.1.clone(),
    ))
}

fn tc_expr(engine: &mut Engine, expr: &Spanned<hir::Expr>) -> Result<Spanned<Expr>, Report> {
    Ok((
        match expr.0 {
            hir::Expr::Error => unreachable!(),
            hir::Expr::Literal(ref lit) => Expr {
                ty: tc_type(&(lit.0.ty(), lit.1.clone()))?.0,
                kind: ExprKind::Literal(tc_literal(lit)?),
            },
            _ => todo!(),
        },
        expr.1.clone(),
    ))
}

fn tc_literal(lit: &Spanned<hir::Literal>) -> Result<Spanned<Literal>, Report> {
    Ok((
        match lit.0 {
            hir::Literal::Unit => Literal::Unit,
            hir::Literal::Int(lit) => Literal::Int(lit),
            hir::Literal::Float(lit) => Literal::Float(lit),
            hir::Literal::Bool(lit) => Literal::Bool(lit),
        },
        lit.1.clone(),
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

            (TypeInfo::List(a), TypeInfo::List(b)) => self.unify(a, b),

            (_, _) => Err(Report::type_conflict(var_a, var_b)),
        }
    }

    pub fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Report> {
        let var = self.vars[&id].clone();

        match var.0 {
            TypeInfo::Unknown => Err(Report::cannot_infer_type(var.1.clone())),
            TypeInfo::Ref(id) => Ok((self.reconstruct(id)?.0, var.1)),
            TypeInfo::Unit => Ok((Type::Unit, var.1)),
            TypeInfo::Ident(ident) => Ok((Type::Ident(ident), var.1)),
            TypeInfo::Int => Ok((Type::Int, var.1)),
            TypeInfo::Float => Ok((Type::Float, var.1)),
            TypeInfo::Bool => Ok((Type::Bool, var.1)),
            TypeInfo::List(ty) => Ok((Type::List(Box::new(self.reconstruct(ty)?)), var.1)),
        }
    }

    fn convert_to_typeinfo(&mut self, ty: Spanned<Type>) -> Spanned<TypeInfo> {
        (
            match ty.0 {
                Type::Unit => TypeInfo::Unit,
                Type::Ident(ident) => TypeInfo::Ident(ident),
                Type::Int => TypeInfo::Int,
                Type::Float => TypeInfo::Float,
                Type::Bool => TypeInfo::Bool,
                Type::List(ty) => {
                    let ty = self.convert_to_typeinfo(*ty);

                    TypeInfo::List(self.insert(ty))
                }
            },
            ty.1,
        )
    }
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
    List(TypeId),
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
            TypeInfo::List(ty) => write!(f, "[{ty}]"),
        }
    }
}
