use crate::cfg::{self, *};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use petgraph::graph::{Graph, NodeIndex};
use rustc_hash::FxHashMap;

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

    pub fn compile(&mut self, functions: Functions) -> *const u8 {
        let mut main_id = None;

        for (name, func) in functions.functions {
            for param in func.params {
                self.ctx
                    .func
                    .signature
                    .params
                    .push(AbiParam::new(param.ty.into()));
            }

            self.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(func.ty.into()));

            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

            let entry_block = builder.create_block();

            builder.append_block_params_for_function_params(entry_block);

            builder.switch_to_block(entry_block);

            builder.seal_block(entry_block);

            let mut translator = FunctionTranslator {
                builder,
                module: &mut self.module,
                graph: func.body.graph,
                entry: func.body.entry,
                vars: FxHashMap::default(),
                var_index: 0,
            };

            translator.translate_cfg();

            translator.builder.finalize();

            let id = self
                .module
                .declare_function(name, Linkage::Export, &self.ctx.func.signature)
                .unwrap();

            self.module.define_function(id, &mut self.ctx).unwrap();

            if name == "main" {
                main_id = Some(id);
            }

            println!("declared function: {}", name);
            println!("{}", &self.ctx.func);

            self.module.clear_context(&mut self.ctx);
        }

        self.module.finalize_definitions().unwrap();

        self.module.get_finalized_function(main_id.unwrap())
    }
}

struct FunctionTranslator<'a, 'src: 'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,
    graph: Graph<BasicBlock<'src>, ()>,
    entry: NodeIndex,
    vars: FxHashMap<&'src str, Variable>,
    var_index: usize,
}

impl<'a, 'src> FunctionTranslator<'a, 'src> {
    fn translate_cfg(&mut self) {
        let start_block = self.graph[self.entry].clone();

        self.translate_bb(start_block);
    }

    fn translate_bb(&mut self, bb: BasicBlock<'src>) {
        for statement in bb.statements {
            self.translate_statement(statement);
        }

        match bb.terminator {
            Terminator::Return(expr) => {
                let value = self.translate_expr(expr);

                self.builder.ins().return_(&[value]);
            }
            Terminator::Jump(idx) => {
                let block = self.builder.create_block();

                self.builder.ins().jump(block, &[]);

                self.builder.seal_block(block);

                self.builder.switch_to_block(block);

                self.translate_bb(self.graph[idx].clone());
            }
            Terminator::Branch {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.translate_expr(condition);

                let then_block = self.builder.create_block();
                let otherwise_block = self.builder.create_block();

                self.builder
                    .ins()
                    .brif(condition, then_block, &[], otherwise_block, &[]);

                self.builder.seal_block(then_block);
                self.builder.seal_block(otherwise_block);

                self.builder.switch_to_block(then_block);

                self.translate_bb(self.graph[then].clone());

                self.builder.switch_to_block(otherwise_block);

                self.translate_bb(self.graph[otherwise].clone());

                let join_block = self.builder.create_block();

                self.builder.ins().jump(join_block, &[]);

                self.builder.seal_block(join_block);

                self.builder.switch_to_block(join_block);
            }
        }
    }

    fn translate_statement(&mut self, statement: Statement<'src>) {
        match statement {
            Statement::Error => unreachable!(),
            Statement::Expr(expr) => {
                self.translate_expr(expr);
            }
            Statement::Let { name, ty, value } => {
                let var = self.variable();

                self.builder.declare_var(var, ty.into());

                let value = self.translate_expr(value);

                self.builder.def_var(var, value);

                self.vars.insert(name, var);
            }
            Statement::Assign { name, value } => {
                let var = self.vars.get(&name).unwrap().clone();

                let value = self.translate_expr(value);

                self.builder.def_var(var, value);
            }
            Statement::Print(_) => panic!("print statement not implemented in jit"),
        }
    }

    fn translate_expr(&mut self, expression: Expr) -> Value {
        match expression.expr {
            ExprKind::Error => unreachable!(),
            ExprKind::Var(name) => {
                let var = self.vars.get(&name).unwrap().clone();

                self.builder.use_var(var)
            }
            ExprKind::Literal(literal) => match literal {
                Literal::Int(v) => self.builder.ins().iconst(INT_TYPE, v as i64),
                Literal::Bool(v) => self.builder.ins().iconst(BOOL_TYPE, v as i64),
                Literal::Unit => self.builder.ins().iconst(UNIT_TYPE, 0),
            },
            ExprKind::Prefix { op, expr } => {
                let translated_expr = self.translate_expr(*expr.clone());

                match op {
                    PrefixOp::Negate => match expr.ty {
                        cfg::Type::Int => self.builder.ins().ineg(translated_expr),
                        cfg::Type::Bool => unreachable!(),
                        cfg::Type::Unit => unreachable!(),
                    },
                }
            }
            ExprKind::Binary { op, lhs, rhs } => match (&lhs.ty, &rhs.ty) {
                (cfg::Type::Int, cfg::Type::Int) => {
                    let lhs = self.translate_expr(*lhs);
                    let rhs = self.translate_expr(*rhs);

                    match op {
                        BinOp::Add => self.builder.ins().iadd(lhs, rhs),
                        BinOp::Subtract => self.builder.ins().isub(lhs, rhs),
                        BinOp::Multiply => self.builder.ins().imul(lhs, rhs),
                        BinOp::Divide => self.builder.ins().sdiv(lhs, rhs),
                        BinOp::Equals => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        BinOp::NotEquals => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        BinOp::LessThan => self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        BinOp::LessThanOrEqual => {
                            self.builder
                                .ins()
                                .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                        }
                        BinOp::GreaterThan => {
                            self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                        }
                        BinOp::GreaterThanOrEqual => {
                            self.builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                        }
                        BinOp::LogicalAnd | BinOp::LogicalOr => unreachable!(),
                    }
                }
                (cfg::Type::Bool, cfg::Type::Bool) => {
                    let lhs = self.translate_expr(*lhs);
                    let rhs = self.translate_expr(*rhs);

                    match op {
                        BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => {
                            unreachable!()
                        }
                        BinOp::Equals => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        BinOp::NotEquals => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        BinOp::LessThan => {
                            self.builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs)
                        }
                        BinOp::LessThanOrEqual => {
                            self.builder
                                .ins()
                                .icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs)
                        }
                        BinOp::GreaterThan => {
                            self.builder
                                .ins()
                                .icmp(IntCC::UnsignedGreaterThan, lhs, rhs)
                        }
                        BinOp::GreaterThanOrEqual => {
                            self.builder
                                .ins()
                                .icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs)
                        }
                        BinOp::LogicalAnd => self.builder.ins().band(lhs, rhs),
                        BinOp::LogicalOr => self.builder.ins().bor(lhs, rhs),
                    }
                }
                (cfg::Type::Unit, cfg::Type::Unit) => match op {
                    BinOp::Add
                    | BinOp::Subtract
                    | BinOp::Multiply
                    | BinOp::Divide
                    | BinOp::Equals
                    | BinOp::NotEquals
                    | BinOp::LessThan
                    | BinOp::LessThanOrEqual
                    | BinOp::GreaterThan
                    | BinOp::GreaterThanOrEqual
                    | BinOp::LogicalAnd
                    | BinOp::LogicalOr => unreachable!(),
                },
                _ => unreachable!(),
            },
            ExprKind::Call { name, args } => todo!(),
        }
    }

    fn variable(&mut self) -> Variable {
        let var = Variable::new(self.var_index);

        self.var_index += 1;

        var
    }
}

impl From<cfg::Type> for types::Type {
    fn from(ty: cfg::Type) -> Self {
        match ty {
            cfg::Type::Int => INT_TYPE,
            cfg::Type::Bool => BOOL_TYPE,
            cfg::Type::Unit => UNIT_TYPE,
        }
    }
}

const INT_TYPE: types::Type = types::I32;
const BOOL_TYPE: types::Type = types::I8;
const UNIT_TYPE: types::Type = types::I8;
