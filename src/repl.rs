use std::io::Write;

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub struct Repl {}

impl Repl {
    pub fn start() {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();

        loop {
            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .expect("Error reading line");

            let mut lexer = Lexer::new(input);

            loop {
                let token = lexer.next_token();
                if token == Token::EOF || token == Token::Illegal {
                    break;
                }
                println!("{:?}", token);
            }
        }
    }
}
