use crate::typed_ast;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

pub struct JIT {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
}

impl JIT {
    pub fn new() -> Self {
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });

        let isa = isa_builder
            .finish(settings::Flags::new(settings::builder()))
            .unwrap();

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);

        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        }
    }
}

impl From<typed_ast::Type> for types::Type {
    fn from(ty: typed_ast::Type) -> Self {
        match ty {
            typed_ast::Type::Int => INT_TYPE,
            typed_ast::Type::Bool => BOOL_TYPE,
            typed_ast::Type::Unit => UNIT_TYPE,
        }
    }
}

const INT_TYPE: types::Type = types::I32;
const BOOL_TYPE: types::Type = types::I8;
const UNIT_TYPE: types::Type = types::I8;
