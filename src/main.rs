use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use chumsky::Parser as _;
use clap::{Parser, Subcommand};
use codegen::cranelift::Codegen;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};
use lexer::lexer;
use std::fs::read_to_string;
use std::path::PathBuf;
use typechecker::typecheck;
use typechecker::typed_ast::TypedAst;

mod codegen;
mod error;
mod lexer;
mod parser;
mod typechecker;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run {
        filename: PathBuf,
    },
    Build {
        filename: PathBuf,

        #[arg(short, long)]
        out: Option<PathBuf>,
        // TODO: add optimization level flag
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Run { filename } => {
            let input = read_to_string(filename).unwrap();

            match run(&input) {
                Ok((typed_ast, _)) => {
                    let jit_main = jit(typed_ast);

                    let exit_code = jit_main();

                    std::process::exit(exit_code);
                }
                Err(e) => {
                    print_errors(e, &input);

                    std::process::exit(1);
                }
            }
        }
        Command::Build { filename, out } => {
            let input = read_to_string(&filename).unwrap();

            match run(&input) {
                Ok((typed_ast, _)) => {
                    let obj = object(typed_ast);

                    let data = obj.emit().unwrap();

                    let obj_filename = filename.with_extension("o");

                    let executable_filename = match out {
                        Some(out) => out,
                        None => {
                            let executable_filename: PathBuf = filename
                                .with_extension(if cfg!(windows) { "exe" } else { "" })
                                .file_name()
                                .unwrap()
                                .into();

                            if executable_filename.exists() {
                                eprintln!("warning: default output file already exists, please specify an output file with -o");
                                std::process::exit(1);
                            } else {
                                executable_filename
                            }
                        }
                    };

                    std::fs::write(&obj_filename, data).unwrap();

                    let status = std::process::Command::new("cc")
                        .arg(&obj_filename)
                        .arg("-o")
                        .arg(executable_filename)
                        .status()
                        .unwrap();

                    std::fs::remove_file(obj_filename).unwrap();

                    if status.success() {
                        std::process::exit(0);
                    } else {
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    print_errors(e, &input);

                    std::process::exit(1);
                }
            }
        }
    }
}

fn run(input: &str) -> Result<Spanned<TypedAst>, Vec<error::Error>> {
    let (tokens, lex_errs) = lexer().parse(input).into_output_errors();

    let (ast, parse_errs) = tokens
        .as_ref()
        .map(|tokens| {
            parser::parser()
                .parse(tokens.spanned((input.len()..input.len()).into()))
                .into_output_errors()
        })
        .unwrap_or((None, vec![]));

    let (typed_ast, tc_errs) = ast
        .map(|ast| match typecheck(ast) {
            Ok(typed_ast) => (Some(typed_ast), vec![]),
            Err(tc_errs) => (None, tc_errs),
        })
        .unwrap_or((None, vec![]));

    let errs = Vec::new()
        .into_iter()
        .chain(
            lex_errs
                .into_iter()
                .map(|e| e.map_token(|t| t.to_string()))
                .map(Into::into),
        )
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|t| t.to_string()))
                .map(Into::into),
        )
        .chain(tc_errs)
        .collect::<Vec<_>>();

    if errs.is_empty() {
        Ok(typed_ast.unwrap())
    } else {
        Err(errs)
    }
}

fn object(typed_ast: TypedAst) -> ObjectProduct {
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });

    let mut settings = settings::builder();

    settings.enable("is_pic").unwrap();

    let isa = isa_builder.finish(settings::Flags::new(settings)).unwrap();

    let builder =
        ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names()).unwrap();

    let mut object_module = ObjectModule::new(builder);

    let mut codegen = Codegen::new(&mut object_module);

    let _ = codegen.compile(typed_ast);

    object_module.finish()
}

fn jit(typed_ast: TypedAst) -> extern "C" fn() -> i32 {
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });

    let isa = isa_builder
        .finish(settings::Flags::new(settings::builder()))
        .unwrap();

    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    let mut jit_module = JITModule::new(builder);

    let mut codegen = Codegen::new(&mut jit_module);

    let main_id = codegen.compile(typed_ast);

    jit_module.finalize_definitions().unwrap();

    let code = jit_module.get_finalized_function(main_id.unwrap());

    unsafe { std::mem::transmute(code) }
}

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

fn print_errors(errors: Vec<error::Error>, input: &str) {
    for e in errors {
        for (msg, spans, note) in e.make_report() {
            let mut report = Report::build(ReportKind::Error, (), spans.first().unwrap().start())
                .with_code(e.code())
                .with_message(msg);

            for ((msg, col), span) in spans {
                report = report.with_label(
                    Label::new(span.into_range())
                        .with_message(msg)
                        .with_color(col),
                );
            }

            if let Some(note) = note {
                report = report.with_note(note);
            }
            report.finish().eprint(Source::from(input)).unwrap();
        }
    }
}
