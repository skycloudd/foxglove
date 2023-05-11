use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use chumsky::Parser;
use std::fs::read_to_string;

mod ast;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod typecheck;
mod typed_ast;

fn main() {
    let filename = std::env::args().nth(1).expect("expected filename");

    match run(&filename) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn run(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let input = read_to_string(filename)?;

    let (tokens, lex_errs) = lexer::lexer().parse(&input).into_output_errors();

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
            Err(tc_errs) => (None, vec![tc_errs]),
        }
    } else {
        (None, vec![])
    };

    dbg!(&typed_ast);

    if let Some(typed_ast) = typed_ast {
        interpreter::interpret(typed_ast)?;
    }

    Vec::new()
        .into_iter()
        .chain(lex_errs.into_iter().map(|e| e.map_token(|t| t.to_string())))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|t| t.to_string())),
        )
        .for_each(|e| {
            eprintln!(
                "{} `{}`:\t{}",
                e.span(),
                &input[e.span().start()..e.span().end()],
                e
            );
        });

    for err in tc_errs {
        eprintln!("{}", err);
    }

    Ok(())
}

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);
