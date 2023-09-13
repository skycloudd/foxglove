use crate::typed_ast;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

pub struct Jit {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
}

impl Jit {
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
