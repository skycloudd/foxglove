//! # foxglove compiler
//!
//! Compiler for the foxglove programming language.
//!
//! ```fox
//! fn main ||> {
//!    print(34 + 35);
//! }
//! ```

use crate::error::Report as ErrorReport;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::Parser as _;
use std::path::PathBuf;

mod ast;
mod error;
mod hir;
mod lexer;
mod lower;
mod parser;
mod typecheck;
mod typed_hir;

/// The main entry point for the compiler.
/// It goes through every stage sequentially
/// If a stage fails, it should go straight to the error reporting stage
pub fn run(config: Config) -> Result<(), Box<dyn std::error::Error>> {
    let input = std::fs::read_to_string(config.filename)?;

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(input.as_str());

    if config.debug_tokens {
        dbg!(&tokens);
    }

    let (ast, parse_errs) = if lex_errs.is_empty() {
        if let Some(tokens) = tokens {
            parser::parse(tokens, input.chars().count())
        } else {
            (None, Vec::new())
        }
    } else {
        (None, Vec::new())
    };

    if config.debug_ast {
        if let Some(ast) = &ast {
            eprintln!("ast: {}", ast.0.to_pretty(80)?);
        }
    }

    let hir = if parse_errs.is_empty() {
        ast.map(|ast| lower::lower(&ast))
    } else {
        None
    };

    if config.debug_hir {
        dbg!(&hir);
    }

    let (typed_hir, typecheck_errs) = if let Some(hir) = hir {
        match typecheck::typecheck(&hir) {
            Ok(typed_hir) => (Some(typed_hir), Vec::new()),
            Err(e) => (None, vec![e]),
        }
    } else {
        (None, Vec::new())
    };

    if config.debug_thir {
        dbg!(&typed_hir);
    }

    Vec::new()
        .into_iter()
        .chain(
            lex_errs
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string()))
                .map(Into::into),
        )
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string()))
                .map(Into::into),
        )
        .chain(typecheck_errs)
        .for_each(|e: ErrorReport| {
            let mut report = Report::build(ReportKind::Error, (), e.offset())
                .with_code(e.code().0.clone())
                .with_message(e.msg());

            report.add_labels(
                e.errors()
                    .iter()
                    .map(|e| Label::new(e.span()).with_message(e.msg())),
            );

            if let Some(note) = e.note() {
                report.set_note(note);
            }

            if let Some(help) = e.help_message() {
                report.set_help(help);
            }

            report
                .finish()
                .eprint(Source::from(&input))
                .unwrap_or_else(|e| eprintln!("{}", e.fg(Color::Yellow)));
        });

    Err("compilation failed".into())
}

pub type Span = core::ops::Range<usize>;

pub type Spanned<T> = (T, Span);

/// The configuration for the compiler. It is used to pass
/// information from the command line to the compiler.
/// It contains the filename of the file to compile.
pub struct Config {
    filename: PathBuf,
    debug_tokens: bool,
    debug_ast: bool,
    debug_hir: bool,
    debug_thir: bool,
}

impl Config {
    /// Creates a new `Config` from a filename.
    #[must_use]
    pub fn new(
        filename: PathBuf,
        debug_tokens: bool,
        debug_ast: bool,
        debug_hir: bool,
        debug_thir: bool,
    ) -> Self {
        Self {
            filename,
            debug_tokens,
            debug_ast,
            debug_hir,
            debug_thir,
        }
    }
}
