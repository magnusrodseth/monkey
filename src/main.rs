use clap::{arg, command, Parser};
use file::File;
use repl::Repl;
use std::path::Path;

mod ast;
mod builtins;
mod environment;
mod evaluator;
mod file;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Read program from file
    #[arg(short, long)]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(file) = args.file {
        File::run(Path::new(&file));
        return;
    }

    Repl::start();
}
