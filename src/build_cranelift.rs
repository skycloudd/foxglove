use crate::typed_ast::{self, *};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::codegen::verify_function;
use cranelift::prelude::*;
use rustc_hash::FxHashMap;

pub fn build_cranelift(typed_ast: TypedAst) {
    let sig = Signature::new(isa::CallConv::SystemV);

    let mut fn_builder_ctx = FunctionBuilderContext::new();

    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);

    let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

    let mut vars = FxHashMap::default();

    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);

    builder.switch_to_block(entry_block);

    builder.seal_block(entry_block);

    for statement in typed_ast.statements {
        build_statement(&mut builder, &mut vars, &mut 0, statement);
    }

    builder.ins().return_(&[]);

    builder.finalize();

    let flags = settings::Flags::new(settings::builder());

    let res = verify_function(&func, &flags);

    match res {
        Ok(_) => {
            println!("{}", func.display());
        }
        Err(e) => {
            panic!("cranelift verification error: {:?}", e);
        }
    }
}

fn build_statement<'src>(
    builder: &mut FunctionBuilder,
    vars: &mut FxHashMap<&'src str, Variable>,
    counter: &mut usize,
    statement: Statement<'src>,
) {
    match statement {
        Statement::Error => unreachable!(),
        Statement::Expr(expr) => {
            build_expr(builder, vars, expr);
        }
        Statement::Block(statements) => {
            for statement in statements {
                build_statement(builder, vars, counter, statement);
            }
        }
        Statement::Let { name, ty, value } => {
            let value = build_expr(builder, vars, value);

            let var = variable(counter);

            builder.declare_var(var, ty.into());

            builder.def_var(var, value);

            vars.insert(name, var);
        }
        Statement::Assign { name, value } => {
            let value = build_expr(builder, vars, value);

            let var = variable(counter);

            builder.def_var(var, value);

            vars.insert(name, var);
        }
        Statement::Print(expr) => todo!(),
        Statement::Loop(stmt) => todo!(),
        Statement::Continue => todo!(),
        Statement::Break => todo!(),
        Statement::Conditional {
            condition,
            then,
            otherwise,
        } => todo!(),
    }
}

fn build_expr(
    builder: &mut FunctionBuilder,
    vars: &mut FxHashMap<&str, Variable>,
    expression: Expr,
) -> Value {
    match expression.expr {
        ExprKind::Error => unreachable!(),
        ExprKind::Var(name) => {
            let var = vars.get(name).unwrap();

            builder.use_var(*var)
        }
        ExprKind::Literal(literal) => match literal {
            Literal::Int(v) => builder.ins().iconst(INT_TYPE, v as i64),
            Literal::Bool(v) => builder.ins().iconst(BOOL_TYPE, v as i64),
            Literal::Unit => builder.ins().iconst(UNIT_TYPE, 0),
        },
        ExprKind::Prefix { op, expr } => {
            let built_expr = build_expr(builder, vars, *expr.clone());

            match op {
                PrefixOp::Negate => match expr.ty {
                    typed_ast::Type::Int => builder.ins().ineg(built_expr),
                    typed_ast::Type::Bool => unreachable!(),
                    typed_ast::Type::Unit => unreachable!(),
                },
            }
        }
        ExprKind::Binary { op, lhs, rhs } => {
            let built_lhs = build_expr(builder, vars, *lhs.clone());
            let built_rhs = build_expr(builder, vars, *rhs.clone());

            match (lhs.ty, rhs.ty) {
                (typed_ast::Type::Int, typed_ast::Type::Int) => match op {
                    BinOp::Add => builder.ins().iadd(built_lhs, built_rhs),
                    BinOp::Subtract => builder.ins().isub(built_lhs, built_rhs),
                    BinOp::Multiply => builder.ins().imul(built_lhs, built_rhs),
                    BinOp::Divide => builder.ins().sdiv(built_lhs, built_rhs),
                    BinOp::Equals => builder.ins().icmp(IntCC::Equal, built_lhs, built_rhs),
                    BinOp::NotEquals => builder.ins().icmp(IntCC::NotEqual, built_lhs, built_rhs),
                    BinOp::LessThan => {
                        builder
                            .ins()
                            .icmp(IntCC::SignedLessThan, built_lhs, built_rhs)
                    }
                    BinOp::LessThanOrEqual => {
                        builder
                            .ins()
                            .icmp(IntCC::SignedLessThanOrEqual, built_lhs, built_rhs)
                    }
                    BinOp::GreaterThan => {
                        builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThan, built_lhs, built_rhs)
                    }
                    BinOp::GreaterThanOrEqual => {
                        builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, built_lhs, built_rhs)
                    }
                    BinOp::LogicalAnd => unreachable!(),
                    BinOp::LogicalOr => unreachable!(),
                },
                (typed_ast::Type::Bool, typed_ast::Type::Bool) => match op {
                    BinOp::Add => unreachable!(),
                    BinOp::Subtract => unreachable!(),
                    BinOp::Multiply => unreachable!(),
                    BinOp::Divide => unreachable!(),
                    BinOp::Equals => builder.ins().icmp(IntCC::Equal, built_lhs, built_rhs),
                    BinOp::NotEquals => builder.ins().icmp(IntCC::NotEqual, built_lhs, built_rhs),
                    BinOp::LessThan => {
                        builder
                            .ins()
                            .icmp(IntCC::UnsignedLessThan, built_lhs, built_rhs)
                    }
                    BinOp::LessThanOrEqual => {
                        builder
                            .ins()
                            .icmp(IntCC::UnsignedLessThanOrEqual, built_lhs, built_rhs)
                    }
                    BinOp::GreaterThan => {
                        builder
                            .ins()
                            .icmp(IntCC::UnsignedGreaterThan, built_lhs, built_rhs)
                    }
                    BinOp::GreaterThanOrEqual => {
                        builder
                            .ins()
                            .icmp(IntCC::UnsignedGreaterThanOrEqual, built_lhs, built_rhs)
                    }
                    BinOp::LogicalAnd => builder.ins().band(built_lhs, built_rhs),
                    BinOp::LogicalOr => builder.ins().bor(built_lhs, built_rhs),
                },
                _ => unreachable!(),
            }
        }
    }
}

fn variable(counter: &mut usize) -> Variable {
    let var = Variable::new(*counter);
    *counter += 1;
    var
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
