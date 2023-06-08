use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
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

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
        };

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
                Err(error) => self.errors.push(error),
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

    fn parse_expression_statement(&mut self) -> Result<Box<dyn Statement>, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest);

        match expression {
            Some(expression) => {
                if self.peek_token_is(&Token::Semicolon) {
                    self.next_token();
                }

                Ok(Box::new(ExpressionStatement {
                    token: self.current_token.clone(),
                    expression: Some(expression),
                }))
            }
            None => Err(ParserError::new(
                format!("Expected expression, got {}", self.current_token).into(),
            )),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let left = match self.current_token {
            Token::Integer(_) => self.parse_integer_literal(),
            Token::Identifier(_) => self.parse_identifier(),
            Token::Bang | Token::Minus => self.parse_prefix(),
            _ => {
                self.errors.push(ParserError::new(
                    format!("No prefix parse function for {}", self.current_token).into(),
                ));
                None
            }
        };

        left
    }

    fn parse_integer_literal(&self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();

        let value = if let Token::Integer(value) = self.current_token.clone() {
            value
        } else {
            return None;
        };

        Some(Box::new(IntegerLiteral { token, value }))
    }

    fn parse_prefix(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        match right {
            Some(right) => Some(Box::new(PrefixExpression {
                token: self.current_token.clone(),
                right,
            })),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ExpressionStatement, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        ReturnStatement,
    };

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

    macro_rules! print_parser_errors {
        ($errors:expr) => {
            for error in $errors {
                println!("{}", error);
            }
        };
    }

    macro_rules! assert_integer_literal_eq {
        ($integer_literal:expr, $expected_value:expr) => {
            assert_eq!($integer_literal.value, $expected_value);
            assert_eq!($integer_literal.token, Token::Integer($expected_value));
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
        print_parser_errors!(parser.errors());

        match program {
            Err(_) => {
                panic!("Parser error");
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
        print_parser_errors!(parser.errors());

        match program {
            Err(_) => {
                panic!("Parser error");
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
        print_parser_errors!(parser.errors());

        match program {
            Err(_) => {
                panic!("Parser error");
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

    #[test]
    fn integer_literal_as_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        print_parser_errors!(parser.errors());

        match program {
            Err(_) => {
                panic!("Parser error");
            }
            Ok(program) => {
                assert_eq!(program.statements.len(), 1);

                if let Some(statement) = program.statements.first() {
                    let expression = cast_into!(statement, ExpressionStatement);
                    if let Some(expr) = &expression.expression {
                        let integer = cast_into!(expr, IntegerLiteral);
                        assert_integer_literal_eq!(integer, 5);
                    }
                } else {
                    panic!("Expected statement, got something else");
                }
            }
        }
    }

    #[test]
    fn prefix_expressions() {
        struct PrefixExpressionTest<'a> {
            input: &'a str,
            integer_value: i64,
        }

        let tests = vec![
            PrefixExpressionTest {
                input: "!5;",
                integer_value: 5,
            },
            PrefixExpressionTest {
                input: "-15;",
                integer_value: 15,
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            print_parser_errors!(parser.errors());

            match program {
                Err(_) => {
                    panic!("Parser error");
                }
                Ok(program) => {
                    assert_eq!(program.statements.len(), 1);

                    if let Some(statement) = program.statements.first() {
                        let expression = cast_into!(statement, ExpressionStatement);
                        if let Some(expression) = &expression.expression {
                            let prefix = cast_into!(expression, PrefixExpression);
                            let integer = cast_into!(prefix.right, IntegerLiteral);
                            assert_integer_literal_eq!(integer, test.integer_value);
                        }
                    } else {
                        panic!("Expected statement, got something else");
                    }
                }
            }
        }
    }

    #[test]
    fn infex_expressions() {
        struct InfixExpressionTest<'a> {
            input: &'a str,
            left_value: i64,
            operator: &'a str,
            right_value: i64,
        }

        let tests = vec![
            InfixExpressionTest {
                input: "5 + 5;",
                left_value: 5,
                operator: "+",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 - 5;",
                left_value: 5,
                operator: "-",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 * 5;",
                left_value: 5,
                operator: "*",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 / 5;",
                left_value: 5,
                operator: "/",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 > 5;",
                left_value: 5,
                operator: ">",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 < 5;",
                left_value: 5,
                operator: "<",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 == 5;",
                left_value: 5,
                operator: "==",
                right_value: 5,
            },
            InfixExpressionTest {
                input: "5 != 5;",
                left_value: 5,
                operator: "!=",
                right_value: 5,
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            print_parser_errors!(parser.errors());

            match program {
                Err(_) => {
                    panic!("Parser error");
                }
                Ok(program) => {
                    assert_eq!(program.statements.len(), 1);

                    if let Some(statement) = program.statements.first() {
                        let expression = cast_into!(statement, ExpressionStatement);
                        if let Some(expression) = &expression.expression {
                            let infix = cast_into!(expression, InfixExpression);
                            let left = cast_into!(infix.left, IntegerLiteral);
                            assert_integer_literal_eq!(left, test.left_value);
                            let right = cast_into!(infix.right, IntegerLiteral);
                            assert_integer_literal_eq!(right, test.right_value);

                            assert_eq!(infix.operator, test.operator);

                            assert_eq!(right.value, test.right_value);
                            assert_eq!(right.token, Token::Integer(test.right_value));
                        }
                    } else {
                        panic!("Expected statement, got something else");
                    }
                }
            }
        }
    }
}
