use repl::Repl;

mod ast;
mod builtin;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    Repl::start();
}
