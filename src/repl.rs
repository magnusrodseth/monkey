use std::{cell::RefCell, env, io::Write, rc::Rc};

use crate::{
    environment::Environment,
    evaluator::Evaluator,
    lexer::Lexer,
    parser::{Parser, ParserError},
};

const PROMPT: &str = ">> ";

const MONKEY_ART: &str = r#"
                        .="=.
                      _/.-.-.\_     _
                     ( ( o o ) )    ))
                      |/  "  \|    //
      .-------.        \'---'/    //
     _|~~ ~~  |_       /`"""`\\  ((
   =(_|_______|_)=    / /_,_\ \\  \\
     |:::::::::|      \_\\_'__/ \  ))
     |:::::::[]|       /`  /`~\  |//
     |o=======.|      /   /    \  /
     `"""""""""`  ,--`,--'\/\    /
                   '-- "--'  '--'
"#;

pub struct Repl {}

impl Repl {
    fn print_errors(errors: Vec<ParserError>) {
        println!("{}", MONKEY_ART);

        println!("Woops! We ran into some monkey business here!");
        println!("\t>> Parser errors:");
        for error in errors {
            println!("\t{}", error);
        }
    }

    pub fn start() {
        // Parsist the environment
        let environment = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::new(environment);

        loop {
            print!("{}", PROMPT);
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .expect("Error reading line");

            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();

            if parser.errors().len() > 0 {
                Repl::print_errors(parser.errors());
                continue;
            }

            match program {
                Err(_) => {
                    Repl::print_errors(parser.errors());
                }
                Ok(program) => {
                    let evaluated = evaluator.evaluate(program);

                    if let Some(evaluated) = evaluated {
                        println!("{}\n", evaluated);
                    }
                }
            }
        }
    }
}
