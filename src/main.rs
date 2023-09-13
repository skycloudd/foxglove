#![warn(clippy::disallowed_types)] // prevent accidental use of std hashmaps which are slower

use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use chumsky::Parser as _;
use clap::{Parser, Subcommand};
use std::fs::read_to_string;
use std::path::PathBuf;
use typed_ast::TypedAst;

mod ast;
mod build_cranelift;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod typecheck;
mod typed_ast;

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Run { filename } => {
            let input = read_to_string(filename).unwrap();

            match run(&input) {
                Ok(typed_ast) => match interpreter::interpret(typed_ast.0) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("runtime error: {}", e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    print_errors(e, &input);

                    std::process::exit(1);
                }
            }
        }
        Command::Build { filenames } => {
            for filename in filenames {
                let input = read_to_string(&filename).unwrap();

                match run(&input) {
                    Ok(typed_ast) => {
                        todo!("compile: {:?}", typed_ast);
                    }
                    Err(e) => {
                        print_errors(e, &input);

                        continue;
                    }
                }
            }
        }
    }
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Run { filename: PathBuf },
    Build { filenames: Vec<PathBuf> },
}

fn run(input: &str) -> Result<Spanned<TypedAst>, Vec<error::Error>> {
    let (tokens, lex_errs) = lexer::lexer().parse(input).into_output_errors();

    // dbg!(&tokens);

    let (ast, parse_errs) = if let Some(tokens) = &tokens {
        parser::parser()
            .parse(tokens.spanned((input.len()..input.len()).into()))
            .into_output_errors()
    } else {
        (None, vec![])
    };

    // dbg!(&ast);

    let (typed_ast, tc_errs) = if let Some(ast) = ast {
        match typecheck::typecheck(ast) {
            Ok(typed_ast) => (Some(typed_ast), vec![]),
            Err(tc_errs) => (None, tc_errs),
        }
    } else {
        (None, vec![])
    };

    // dbg!(&typed_ast);

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
