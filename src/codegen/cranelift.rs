use crate::typechecker::typed_ast::{self, *};
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashMap;

pub struct Codegen<'a> {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    module: &'a mut dyn Module,
}

impl<'a> Codegen<'a> {
    pub fn new(module: &'a mut impl Module) -> Self {
        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module: module,
        }
    }

    pub fn compile(&mut self, typed_ast: TypedAst) -> Option<FuncId> {
        let mut main_id = None;

        for (name, toplevel) in typed_ast.toplevels {
            match toplevel {
                TopLevel::Function(function) => {
                    for param in &function.params {
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
                        .push(AbiParam::new(function.ty.into()));

                    let mut builder =
                        FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

                    let entry_block = builder.create_block();

                    builder.append_block_params_for_function_params(entry_block);

                    builder.switch_to_block(entry_block);

                    builder.seal_block(entry_block);

                    let mut translator = FunctionTranslator {
                        builder,
                        module: &mut *self.module,
                        vars: HashMap::new(),
                        var_index: 0,
                        loop_block: None,
                        loop_exit_block: None,
                        did_break_or_continue: false,
                        did_return: false,
                    };

                    translator.translate(function.body, entry_block, function.params);

                    translator.builder.finalize();

                    let id = self
                        .module
                        .declare_function(name, Linkage::Export, &self.ctx.func.signature)
                        .unwrap();

                    self.module.define_function(id, &mut self.ctx).unwrap();

                    if name == "main" {
                        main_id = Some(id);
                    }

                    self.module.clear_context(&mut self.ctx);
                }
            }
        }

        main_id
    }
}

struct FunctionTranslator<'a, 'src: 'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut dyn Module,
    vars: HashMap<&'src str, Variable>,
    var_index: usize,
    loop_block: Option<Block>,
    loop_exit_block: Option<Block>,
    did_break_or_continue: bool,
    did_return: bool,
}

impl<'a, 'src> FunctionTranslator<'a, 'src> {
    fn translate(
        &mut self,
        statements: Vec<Statement<'src>>,
        entry_block: Block,
        params: Vec<Param<'src>>,
    ) {
        for (i, param) in params.iter().enumerate() {
            let var = self.variable();

            self.builder.declare_var(var, param.ty.into());

            self.builder
                .def_var(var, self.builder.block_params(entry_block)[i]);

            self.vars.insert(param.name, var);
        }

        for statement in statements {
            self.translate_statement(statement);
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
                let var = *self.vars.get(name).unwrap();

                let value = self.translate_expr(value);

                self.builder.def_var(var, value);
            }
            Statement::Block(statements) => {
                for statement in statements {
                    self.translate_statement(statement);
                }
            }
            Statement::Loop(statement) => {
                let loop_block = self.builder.create_block();
                let loop_exit_block = self.builder.create_block();

                self.loop_block = Some(loop_block);
                self.loop_exit_block = Some(loop_exit_block);

                self.builder.ins().jump(loop_block, &[]);

                self.builder.switch_to_block(loop_block);

                self.translate_statement(*statement);

                self.builder.ins().jump(loop_block, &[]);

                self.builder.seal_block(loop_block);
                self.builder.seal_block(loop_exit_block);

                self.loop_block = None;
                self.loop_exit_block = None;

                self.builder.switch_to_block(loop_exit_block);

                self.did_break_or_continue = false;
            }
            Statement::Continue => {
                self.builder.ins().jump(self.loop_block.unwrap(), &[]);

                self.did_break_or_continue = true;
            }
            Statement::Break => {
                self.builder.ins().jump(self.loop_exit_block.unwrap(), &[]);

                self.did_break_or_continue = true;
            }
            Statement::Return(expr) => {
                let value = self.translate_expr(expr);

                self.builder.ins().return_(&[value]);

                self.did_return = true;
            }
            Statement::Conditional {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.translate_expr(condition);

                let then_block = self.builder.create_block();
                let otherwise_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                self.builder
                    .ins()
                    .brif(condition, then_block, &[], otherwise_block, &[]);

                self.builder.seal_block(then_block);

                self.builder.switch_to_block(then_block);

                self.translate_statement(*then);

                if !self.did_break_or_continue && !self.did_return {
                    self.builder.ins().jump(merge_block, &[]);
                } else {
                    self.did_break_or_continue = false;
                    self.did_return = false;
                }

                self.builder.seal_block(otherwise_block);

                self.builder.switch_to_block(otherwise_block);

                if let Some(otherwise) = otherwise {
                    self.translate_statement(*otherwise);
                }

                if !self.did_break_or_continue && !self.did_return {
                    self.builder.ins().jump(merge_block, &[]);
                } else {
                    self.did_break_or_continue = false;
                    self.did_return = false;
                }

                self.builder.seal_block(merge_block);

                self.builder.switch_to_block(merge_block);
            }
        }
    }

    fn translate_expr(&mut self, expression: Expr) -> Value {
        match expression.expr {
            ExprKind::Error => unreachable!(),
            ExprKind::Var(name) => self.builder.use_var(*self.vars.get(name).unwrap()),
            ExprKind::Literal(literal) => match literal {
                Literal::Int(v) => self.builder.ins().iconst(INT_TYPE, v as i64),
                Literal::Bool(v) => self.builder.ins().iconst(BOOL_TYPE, v as i64),
                Literal::Unit => self.builder.ins().iconst(UNIT_TYPE, 0),
            },
            ExprKind::Prefix { op, expr } => {
                let expr_ty = expr.ty;

                let translated_expr = self.translate_expr(*expr);

                match op {
                    PrefixOp::Negate => match expr_ty {
                        typed_ast::Type::Int => self.builder.ins().ineg(translated_expr),
                        typed_ast::Type::Bool => unreachable!(),
                        typed_ast::Type::Unit => unreachable!(),
                    },
                }
            }
            ExprKind::Binary { op, lhs, rhs } => match (&lhs.ty, &rhs.ty) {
                (typed_ast::Type::Int, typed_ast::Type::Int) => {
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
                (typed_ast::Type::Bool, typed_ast::Type::Bool) => {
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
                (typed_ast::Type::Unit, typed_ast::Type::Unit) => match op {
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
            ExprKind::Call { name, args } => {
                let mut sig = self.module.make_signature();

                for arg in &args {
                    sig.params.push(AbiParam::new(arg.ty.into()));
                }

                sig.returns.push(AbiParam::new(expression.ty.into()));

                let callee = self
                    .module
                    .declare_function(name, Linkage::Import, &sig)
                    .unwrap();

                let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

                let args = args
                    .into_iter()
                    .map(|arg| self.translate_expr(arg))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(local_callee, &args);

                self.builder.inst_results(call)[0]
            }
        }
    }

    fn variable(&mut self) -> Variable {
        let var = Variable::new(self.var_index);

        self.var_index += 1;

        var
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
