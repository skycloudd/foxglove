use crate::cfg::*;
use crate::typed_ast::{self, TypedAst};
use rustc_hash::FxHashMap;

pub fn convert_functions(typed_ast: TypedAst) -> Functions {
    let mut functions = FxHashMap::default();

    for (name, func) in typed_ast.functions {
        let params = func
            .params
            .into_iter()
            .map(|param| Param {
                name: param.name,
                ty: param.ty.into(),
            })
            .collect();

        let cfg = create_cfg(func.body);

        let function = Function {
            name,
            params,
            ty: func.ty.into(),
            body: cfg,
        };

        functions.insert(name, function);
    }

    Functions { functions }
}

pub fn create_cfg(statements: Vec<typed_ast::Statement>) -> Cfg {
    todo!();
}

impl From<typed_ast::Type> for Type {
    fn from(ty: typed_ast::Type) -> Self {
        match ty {
            typed_ast::Type::Int => Type::Int,
            typed_ast::Type::Bool => Type::Bool,
            typed_ast::Type::Unit => Type::Unit,
        }
    }
}
