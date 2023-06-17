use std::{cell::RefCell, path::Path, rc::Rc};

use crate::{environment::Environment, evaluator::Evaluator, lexer::Lexer, parser::Parser};

pub struct File {}

impl File {
    pub fn run(path: &Path) {
        match path.exists() {
            false => {
                println!("{} does not exist", path.display());
                return;
            }
            true => {
                let environment = Rc::new(RefCell::new(Environment::new()));
                let mut evaluator = Evaluator::new(environment);

                let contents = std::fs::read_to_string(path).unwrap();
                let lexer = Lexer::new(contents);
                let mut parser = Parser::new(lexer);
                let program = parser.parse();

                match program {
                    Err(_) => {
                        println!("\t>> Errors:");
                        for error in parser.errors() {
                            println!("\t{}", error);
                        }
                    }
                    Ok(program) => {
                        let evaluated = evaluator.evaluate(program);

                        if let Some(evaluated) = evaluated {
                            println!("{}", evaluated);
                        }
                    }
                }

                return;
            }
        }
    }
}
