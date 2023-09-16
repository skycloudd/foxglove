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

        let cfg_builder = CfgBuilder::new();

        let cfg = cfg_builder.create_cfg(func.body);

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

struct CfgBuilder<'src> {
    blocks: Vec<BasicBlock<'src>>,
    current_block: NodeIndex,
}

impl<'src> CfgBuilder<'src> {
    fn new() -> Self {
        Self {
            blocks: vec![],
            current_block: 0,
        }
    }

    fn create_cfg(self, statements: Vec<typed_ast::Statement>) -> Cfg {
        todo!()
    }
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
