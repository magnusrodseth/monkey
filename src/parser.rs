use std::fmt::Error;

use crate::{
    ast::{Identifier, LetStatement, Program, Statement},
    lexer::{Lexer, Token},
};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::EOF,
            peek_token: Token::EOF,
        };

        // Read two tokens, such that both `current_token` and `peek_token` are set
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse(&mut self) -> Result<Program, Error> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.current_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => return Err(error),
            }

            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, Error> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => todo!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, Error> {
        let statement_token = self.current_token.clone();

        // Peek the value of the upcoming identifier. If this fails, then the let statement is invalid.
        let identifier_token = if let Token::Identifier(value) = self.peek_token.clone() {
            value
        } else {
            return Err(Error);
        };

        if !self.peek_and_expect(Token::Identifier(identifier_token)) {
            return Err(Error);
        }

        let identifier = Identifier {
            token: self.current_token.clone(),
            value: if let Token::Identifier(value) = self.current_token.clone() {
                value
            } else {
                return Err(Error);
            },
        };

        if !self.peek_and_expect(Token::Assign) {
            return Err(Error);
        }

        // TODO: Skip expressions until we find a semicolon
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(LetStatement {
            token: statement_token,
            name: identifier,
            // TODO: Parse expressions
            value: None,
        }))
    }

    fn peek_and_expect(&mut self, expected: Token) -> bool {
        if self.peek_token_is(expected) {
            self.next_token();
            return true;
        }
        false
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::LetStatement;

    macro_rules! assert_let_statement_eq {
        ($let_statement:expr, $expected_identifier:expr) => {
            assert_eq!($let_statement.token(), Token::Let);

            // Cast the statement to any and then downcast to LetStatement
            if let Some(let_statement) = $let_statement.as_any().downcast_ref::<LetStatement>() {
                assert_eq!(let_statement.name.value, $expected_identifier.to_string());
                assert_eq!(
                    let_statement.name.token,
                    Token::Identifier($expected_identifier.to_string())
                );
            } else {
                panic!("Expected let statement, got something else");
            }
        };
    }

    #[test]
    fn let_statement() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let expected_identifiers = vec!["x", "y", "foobar"];

        match program {
            Err(_) => {
                panic!("Expected program, got error");
            }
            Ok(program) => {
                assert_eq!(program.statements.len(), 3);

                for (i, expected_identifier) in expected_identifiers.iter().enumerate() {
                    let statement = &program.statements[i];
                    assert_let_statement_eq!(statement, expected_identifier);
                }
            }
        }
    }
}
