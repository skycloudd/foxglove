use super::typed_ast::Type;
use crate::error::{Error, TypecheckError};
use crate::Spanned;
use std::collections::HashMap;

pub struct Engine {
    id_counter: usize,
    vars: HashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            id_counter: 0,
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
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

            (TypeInfo::Int, TypeInfo::Int)
            | (TypeInfo::Bool, TypeInfo::Bool)
            | (TypeInfo::Unit, TypeInfo::Unit) => Ok(()),

            (a, b) => Err(TypecheckError::TypeMismatch {
                span1: var_a.1,
                span2: var_b.1,
                ty1: a,
                ty2: b,
            }
            .into()),
        }
    }

    pub fn reconstruct(&mut self, id: TypeId) -> Result<Spanned<Type>, Error> {
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

pub type TypeId = usize;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Int,
    Bool,
    Unit,
}

pub fn type_to_typeinfo(ty: Spanned<Type>) -> Spanned<TypeInfo> {
    (
        match ty.0 {
            Type::Int => TypeInfo::Int,
            Type::Bool => TypeInfo::Bool,
            Type::Unit => TypeInfo::Unit,
        },
        ty.1,
    )
}
