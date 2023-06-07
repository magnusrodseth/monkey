use repl::Repl;

mod lexer;
mod repl;
mod ast;

fn main() {
    Repl::start();
}
