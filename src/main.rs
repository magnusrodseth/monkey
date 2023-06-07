use repl::Repl;

mod ast;
mod lexer;
mod parser;
mod repl;

fn main() {
    Repl::start();
}
