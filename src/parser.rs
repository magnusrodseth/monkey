use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement,
        Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, PartialEq, Eq)]
enum Precedence {
    Lowest,
    /// ==
    Equals,
    /// > or <
    LessThanOrGreaterThan,
    /// +
    Sum,
    /// *
    Product,
    /// -X or !X
    Prefix,
    /// myFunction(X)
    Call,
}

#[derive(Debug, Clone)]
struct ParserError {
    message: String,
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.message)
    }
}

impl ParserError {
    fn new(message: String) -> Self {
        Self { message }
    }
}

type PrefixParseFn = dyn Fn(&Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = dyn Fn(Box<dyn Expression>) -> Option<Box<dyn Expression>>;

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,

    prefix_parse_fns: HashMap<Token, Box<PrefixParseFn>>,
    infix_parse_fns: HashMap<Token, Box<InfixParseFn>>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.add_prefix(
            Token::Identifier("".to_string()),
            Box::new(|parser| parser.parse_identifier()),
        );

        // Read two tokens, such that both `current_token` and `peek_token` are set
        parser.next_token();
        parser.next_token();

        parser
    }

    fn errors(&self) -> Vec<ParserError> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(ParserError::new(format!(
            "Expected next token to be {:?}, got {:?}",
            token, self.peek_token
        )));
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse(&mut self) -> Result<Program, ParserError> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => return Err(error),
            }

            self.next_token();
        }

        Ok(program)
    }

    fn parse_identifier(&self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier {
            token: self.current_token.clone(),
            value: if let Token::Identifier(value) = self.current_token.clone() {
                value
            } else {
                return None;
            },
        }))
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, ParserError> {
        let token = self.current_token.clone();

        // Peek the value of the upcoming identifier. If this fails, then the let statement is invalid.
        let identifier_token = if let Token::Identifier(value) = self.peek_token.clone() {
            value
        } else {
            return Err(ParserError::new("Expected identifier".to_string()));
        };

        if !self.peek_and_expect(Token::Identifier(identifier_token)) {
            return Err(ParserError::new("Expected identifier".to_string()));
        }

        let identifier = Identifier {
            token: self.current_token.clone(),
            value: if let Token::Identifier(value) = self.current_token.clone() {
                value
            } else {
                return Err(ParserError::new("Expected identifier".to_string()));
            },
        };

        if !self.peek_and_expect(Token::Assign) {
            return Err(ParserError::new(
                format!("Expected token to be `Assign`, got {}", self.peek_token).into(),
            ));
        }

        // TODO: Skip expressions until we find a semicolon
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(LetStatement {
            token,
            name: identifier,
            // TODO: Parse expressions
            value: None,
        }))
    }

    fn peek_and_expect(&mut self, token: Token) -> bool {
        if self.peek_token_is(&token) {
            self.next_token();
            return true;
        }
        self.peek_error(token);
        false
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        self.peek_token == *token
    }

    fn parse_return_statement(&mut self) -> Result<Box<dyn Statement>, ParserError> {
        let token = self.current_token.clone();

        self.next_token();

        // TODO: Skip expressions until we find a semicolon
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(ReturnStatement {
            token,
            // TODO: Pars expresion
            return_value: None,
        }))
    }

    fn add_prefix(&mut self, token: Token, function: Box<PrefixParseFn>) {
        self.prefix_parse_fns.insert(token, function);
    }

    fn add_infix(&mut self, token: Token, function: Box<InfixParseFn>) {
        self.infix_parse_fns.insert(token, function);
    }

    fn parse_expression_statement(&mut self) -> Result<Box<dyn Statement>, ParserError> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(ExpressionStatement { token, expression }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.current_token);

        if prefix.is_none() {
            return None;
        }

        let function = prefix.expect("Prefix function has already been checked")(self);

        return function;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExpressionStatement, LetStatement, ReturnStatement};

    /// Casts an expression into a specific type, panicking if the cast fails.
    macro_rules! cast_into {
        ($expr:expr, $expected_type:ty) => {
            if let Some(expr) = $expr.as_any().downcast_ref::<$expected_type>() {
                expr
            } else {
                panic!(
                    "Expected {}, got something else",
                    stringify!($expected_type)
                );
            }
        };
    }

    macro_rules! assert_let_statement_eq {
        ($let_statement:expr, $expected_identifier:expr) => {
            assert_eq!($let_statement.token(), Token::Let);

            // Cast the statement to any and then downcast
            let let_statement = cast_into!($let_statement, LetStatement);
            assert_eq!(let_statement.name.value, $expected_identifier.to_string());
            assert_eq!(
                let_statement.name.token,
                Token::Identifier($expected_identifier.to_string())
            );
        };
    }

    macro_rules! assert_return_statement_eq {
        ($return_statement:expr) => {
            assert_eq!($return_statement.token(), Token::Return);

            // Cast the statement to any and then downcast
            let return_statement = cast_into!($return_statement, ReturnStatement);
            assert_eq!(return_statement.token, Token::Return);
        };
    }

    macro_rules! handle_parser_errors {
        ($errors:expr) => {
            for error in $errors {
                println!("{}", error);
            }
            panic!("Expected program, got error");
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
                handle_parser_errors!(parser.errors());
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

    #[test]
    fn return_statement() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        match program {
            Err(_) => {
                handle_parser_errors!(parser.errors());
            }
            Ok(program) => {
                assert_eq!(program.statements.len(), 3);

                for statement in program.statements {
                    assert_return_statement_eq!(statement);
                }
            }
        }
    }

    #[test]
    fn identifier_as_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        match program {
            Err(_) => {
                handle_parser_errors!(parser.errors());
            }
            Ok(program) => {
                assert_eq!(program.statements.len(), 1);

                if let Some(statement) = program.statements.first() {
                    let expression = cast_into!(statement, ExpressionStatement);
                    if let Some(expr) = &expression.expression {
                        let identifier = cast_into!(expr, Identifier);
                        assert_eq!(identifier.value, "foobar");
                        assert_eq!(identifier.token, Token::Identifier("foobar".into()));
                    }
                } else {
                    panic!("Expected statement, got something else");
                }
            }
        }
    }
}
