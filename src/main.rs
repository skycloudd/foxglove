use ariadne::{Color, Fmt};
use clap::Parser;
use foxglove::{run, Config};
use std::path::PathBuf;

fn main() {
    let args = Args::parse();

    let config = args.into();

    match run(config) {
        Ok(exit_code) => std::process::exit(exit_code),
        Err(e) => {
            eprintln!("{}: {}", "Error".fg(Color::Red), e);
            std::process::exit(1);
        }
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[command(arg_required_else_help(true))]
/// Compiler for the foxglove programming language
struct Args {
    filenames: Vec<PathBuf>,

    #[clap(long = "debug-tokens")]
    debug_tokens: bool,

    #[clap(long = "debug-ast")]
    debug_ast: bool,

    #[clap(long = "debug-hir")]
    debug_hir: bool,

    #[clap(long = "debug-thir")]
    debug_thir: bool,
}

impl From<Args> for Config {
    fn from(args: Args) -> Self {
        Self::new(
            args.filenames,
            args.debug_tokens,
            args.debug_ast,
            args.debug_hir,
            args.debug_thir,
        )
    }
}
