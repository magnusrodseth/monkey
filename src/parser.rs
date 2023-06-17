use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{
        BlockStatement, Expression, Identifier, Infix, Literal, Precedence, Prefix, Program,
        Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, Clone)]
pub struct ParserError {
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

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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

    fn token_to_precedence(token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LessThan
            | Token::GreaterThan
            | Token::LessThanOrEqual
            | Token::GreaterThanOrEqual => Precedence::LessThanOrGreaterThan,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LeftParenthesis => Precedence::Call,
            Token::LeftBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    pub fn errors(&self) -> Vec<ParserError> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(ParserError::new(format!(
            "Expected next token to be {:?}, got {:?}",
            token.formatted(),
            self.peek_token.formatted()
        )));
    }

    fn peek_precedence(&self) -> Precedence {
        Self::token_to_precedence(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        Self::token_to_precedence(&self.current_token)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut program = Program::new();

        while !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        Ok(program)
    }

    fn parse_identifier(&self) -> Option<Identifier> {
        match &self.current_token {
            Token::Identifier(value) => Some(Identifier(value.clone())),
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match self.parse_identifier() {
            Some(identifier) => Some(Expression::Identifier(identifier)),
            None => None,
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Empty => Ok(Statement::Empty),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        // Peek the value of the upcoming identifier. If this fails, then the let statement is invalid.
        let identifier_token = if let Token::Identifier(value) = self.peek_token.clone() {
            value
        } else {
            return Err(ParserError::new("Expected identifier".to_string()));
        };

        if !self.peek_and_expect(Token::Identifier(identifier_token)) {
            return Err(ParserError::new("Expected identifier".to_string()));
        }

        let name = match self.parse_identifier() {
            Some(name) => name,
            None => return Err(ParserError::new("Expected identifier".to_string())),
        };

        if !self.peek_and_expect(Token::Assign) {
            return Err(ParserError::new(format!(
                "Expected {:?} token, got {:?}",
                Token::Assign.formatted(),
                self.peek_token.formatted()
            )));
        }

        self.next_token();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(value) => value,
            None => return Err(ParserError::new("Expected expression".to_string())),
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let {
            identifier: name,
            value,
        })
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

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let return_expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return Err(ParserError::new("Expected expression".to_string())),
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(return_expression))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest);

        match expression {
            Some(expression) => {
                if self.peek_token_is(&Token::Semicolon) {
                    self.next_token();
                }

                Ok(Statement::Expression(expression))
            }
            None => Err(ParserError::new(
                format!(
                    "Expected expression, got {}",
                    self.current_token.formatted()
                )
                .into(),
            )),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        dbg!(&self.current_token);
        dbg!(&self.peek_token);

        // Parse prefix
        let mut left = match self.current_token {
            Token::Identifier(_) => self.parse_identifier_expression(),
            Token::Integer(_) => self.parse_integer_literal(),
            Token::String(_) => self.parse_string_literal(),
            Token::Boolean(_) => self.parse_boolean(),
            Token::LeftBracket => self.parse_array_literal(),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix(),
            Token::LeftParenthesis => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::LeftBrace => self.parse_hash_literal(),
            _ => {
                self.errors.push(ParserError::new(
                    format!(
                        "No prefix parse function for {}",
                        self.current_token.formatted()
                    )
                    .into(),
                ));
                None
            }
        };

        // Parse infix
        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual => {
                    self.next_token();
                    left = self.parse_infix(left.expect("Expected left expression"));
                }
                Token::LeftParenthesis => {
                    self.next_token();
                    left = self.parse_call_expression(left.expect("Expected left expression"));
                }
                Token::LeftBracket => {
                    self.next_token();
                    left = self.parse_index_expression(left.expect("Expected left expression"));
                }
                _ => return left,
            }
        }

        left
    }

    fn parse_integer_literal(&self) -> Option<Expression> {
        match &self.current_token {
            Token::Integer(value) => Some(Expression::Literal(Literal::Integer(value.clone()))),
            _ => None,
        }
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        let operator = match self.current_token {
            Token::Bang => Prefix::Not,
            Token::Minus => Prefix::Minus,
            Token::Plus => Prefix::Plus,
            _ => return None,
        };

        let token = self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        match right {
            Some(right) => Some(Expression::Prefix {
                operator,
                right: Box::new(right),
            }),
            None => None,
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
        let operator = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Divide,
            Token::Asterisk => Infix::Multiply,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::LessThan => Infix::LessThan,
            Token::LessThanOrEqual => Infix::LessThanOrEqual,
            Token::GreaterThan => Infix::GreaterThan,
            Token::GreaterThanOrEqual => Infix::GreaterThanOrEqual,
            _ => return None,
        };

        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);

        match right {
            Some(right) => Some(Expression::Infix {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }),
            None => None,
        }
    }

    fn parse_boolean(&self) -> Option<Expression> {
        match self.current_token {
            Token::Boolean(value) => Some(Expression::Literal(Literal::Boolean(value))),
            _ => None,
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.peek_and_expect(Token::RightParenthesis) {
            return None;
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.peek_and_expect(Token::LeftParenthesis) {
            return None;
        }

        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(condition) => condition,
            None => return None,
        };

        if !self.peek_and_expect(Token::RightParenthesis) {
            return None;
        }

        if !self.peek_and_expect(Token::LeftBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();
        let mut alternative = None;

        if self.peek_token_is(&Token::Else) {
            self.next_token();

            if !self.peek_and_expect(Token::LeftBrace) {
                return None;
            }

            alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        BlockStatement { statements }
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.peek_and_expect(Token::LeftParenthesis) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.peek_and_expect(Token::LeftBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::RightParenthesis) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        match self.parse_identifier() {
            Some(identifier) => identifiers.push(identifier),
            None => return Vec::new(),
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_identifier() {
                Some(identifier) => identifiers.push(identifier),
                None => return Vec::new(),
            }
        }

        if !self.peek_and_expect(Token::RightParenthesis) {
            return Vec::new();
        }

        identifiers
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let arguments = match self.parse_expression_list(Token::RightParenthesis) {
            Some(arguments) => arguments,
            None => return None,
        };

        Some(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_string_literal(&self) -> Option<Expression> {
        match self.current_token {
            Token::String(ref value) => Some(Expression::Literal(Literal::String(value.clone()))),
            _ => None,
        }
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let elements = match self.parse_expression_list(Token::RightBracket) {
            Some(elements) => elements,
            None => return None,
        };

        Some(Expression::Literal(Literal::Array(elements)))
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut elements = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(elements);
        }

        self.next_token();

        match self.parse_expression(Precedence::Lowest) {
            Some(element) => elements.push(element),
            None => return None,
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_expression(Precedence::Lowest) {
                Some(element) => elements.push(element),
                None => return None,
            }
        }

        if !self.peek_and_expect(end) {
            return None;
        }

        Some(elements)
    }

    fn parse_index_expression(&mut self, expect: Expression) -> Option<Expression> {
        self.next_token();

        let index = match self.parse_expression(Precedence::Lowest) {
            Some(index) => index,
            None => return None,
        };

        if !self.peek_and_expect(Token::RightBracket) {
            return None;
        }

        Some(Expression::Index {
            left: Box::new(expect),
            index: Box::new(index),
        })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut pairs: Vec<(Expression, Expression)> = Vec::new();

        while !self.peek_token_is(&Token::RightBrace) {
            self.next_token();

            let key = match self.parse_expression(Precedence::Lowest) {
                Some(key) => key,
                None => return None,
            };

            if !self.peek_and_expect(Token::Colon) {
                return None;
            }

            self.next_token();

            let value = match self.parse_expression(Precedence::Lowest) {
                Some(value) => value,
                None => return None,
            };

            pairs.push((key, value));

            if !self.peek_token_is(&Token::RightBrace) && !self.peek_and_expect(Token::Comma) {
                return None;
            }
        }

        if !self.peek_and_expect(Token::RightBrace) {
            return None;
        }

        Some(Expression::Literal(Literal::Hash(pairs)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! print_parser_errors {
        ($errors:expr) => {
            for error in $errors {
                println!("{}", error);
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

        let expected = vec![
            Statement::Let {
                identifier: Identifier("x".into()),
                value: Expression::Literal(Literal::Integer(5)),
            },
            Statement::Let {
                identifier: Identifier("y".into()),
                value: Expression::Literal(Literal::Integer(10)),
            },
            Statement::Let {
                identifier: Identifier("foobar".into()),
                value: Expression::Literal(Literal::Integer(838383)),
            },
        ];

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        print_parser_errors!(parser.errors());

        match program {
            Err(_) => {
                panic!("Parser error");
            }
            Ok(program) => {
                assert_eq!(program.statements.len(), 3);

                for (i, expected_identifier) in expected.iter().enumerate() {
                    let statement = &program.statements[i];
                    assert_eq!(statement, expected_identifier);
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

        let expected = vec![
            Statement::Return(Expression::Literal(Literal::Integer(5))),
            Statement::Return(Expression::Literal(Literal::Integer(10))),
            Statement::Return(Expression::Literal(Literal::Integer(993322))),
        ];

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

                for (i, expected_return) in expected.iter().enumerate() {
                    let statement = &program.statements[i];
                    assert_eq!(statement, expected_return);
                }
            }
        }
    }

    #[test]
    fn identifier_as_expression() {
        let input = "foobar;";
        let expected = Statement::Expression(Expression::Identifier(Identifier("foobar".into())));

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
                let statement = &program.statements[0];
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn integer_literal_as_expression() {
        let input = "5;";
        let expected = Statement::Expression(Expression::Literal(Literal::Integer(5)));

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
                let statement = &program.statements[0];
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn prefix_expressions() {
        struct PrefixExpressionTest<'a> {
            input: &'a str,
            expected: Statement,
        }

        let tests = vec![
            PrefixExpressionTest {
                input: "!5;",
                expected: Statement::Expression(Expression::Prefix {
                    operator: Prefix::Not,
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            PrefixExpressionTest {
                input: "-15;",
                expected: Statement::Expression(Expression::Prefix {
                    operator: Prefix::Minus,
                    right: Box::new(Expression::Literal(Literal::Integer(15))),
                }),
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
                    let statement = &program.statements[0];
                    assert_eq!(*statement, test.expected);
                }
            }
        }
    }

    #[test]
    fn infix_expressions() {
        struct InfixExpressionTest<'a> {
            input: &'a str,
            expected: Statement,
        }

        let tests = vec![
            InfixExpressionTest {
                input: "5 + 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::Plus,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 - 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::Minus,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 * 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::Multiply,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 / 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::Divide,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 > 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::GreaterThan,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 < 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::LessThan,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 == 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::Equal,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
            },
            InfixExpressionTest {
                input: "5 != 5;",
                expected: Statement::Expression(Expression::Infix {
                    operator: Infix::NotEqual,
                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                }),
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
                    let statement = &program.statements[0];
                    assert_eq!(*statement, test.expected);
                }
            }
        }
    }

    #[test]
    fn operator_precedence() {
        struct OperatorPrecedenceTest<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let expected = vec![
            OperatorPrecedenceTest {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            OperatorPrecedenceTest {
                input: "!-a",
                expected: "(!(-a))",
            },
            OperatorPrecedenceTest {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            OperatorPrecedenceTest {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            OperatorPrecedenceTest {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            OperatorPrecedenceTest {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            OperatorPrecedenceTest {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            OperatorPrecedenceTest {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            OperatorPrecedenceTest {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            OperatorPrecedenceTest {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            OperatorPrecedenceTest {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            OperatorPrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            OperatorPrecedenceTest {
                input: "true",
                expected: "true",
            },
            OperatorPrecedenceTest {
                input: "false",
                expected: "false",
            },
            OperatorPrecedenceTest {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            OperatorPrecedenceTest {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
            OperatorPrecedenceTest {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            OperatorPrecedenceTest {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            OperatorPrecedenceTest {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            OperatorPrecedenceTest {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            OperatorPrecedenceTest {
                input: "!(true == true)",
                expected: "(!(true == true))",
            },
            OperatorPrecedenceTest {
                input: "a + add(b * c) + d",
                expected: "((a + add((b * c))) + d)",
            },
            OperatorPrecedenceTest {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            },
            OperatorPrecedenceTest {
                input: "add(a + b + c * d / f + g)",
                expected: "add((((a + b) + ((c * d) / f)) + g))",
            },
            OperatorPrecedenceTest {
                input: "a * [1, 2, 3, 4][b * c] * d",
                expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            },
            OperatorPrecedenceTest {
                input: "add(a * b[2], b[1], 2 * [1, 2][1])",
                expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            },
        ];

        for test in expected {
            let lexer = Lexer::new(test.input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            print_parser_errors!(parser.errors());

            match program {
                Err(_) => {
                    panic!("Parser error");
                }
                Ok(program) => {
                    assert_eq!(program.to_string(), test.expected);
                }
            }
        }
    }

    #[test]
    fn simple_if() {
        let input = "if (x < y) { x }";
        let expected = Statement::Expression(Expression::If {
            condition: Box::new(Expression::Infix {
                left: Box::new(Expression::Identifier(Identifier("x".to_string()))),
                operator: Infix::LessThan,
                right: Box::new(Expression::Identifier(Identifier("y".to_string()))),
            }),
            consequence: BlockStatement {
                statements: vec![Statement::Expression(Expression::Identifier(Identifier(
                    "x".to_string(),
                )))],
            },
            alternative: None,
        });

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
                let statement = program.statements.first().unwrap();
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn if_else() {
        let input = "if (x < y) { x } else { y }";

        let expected = Statement::Expression(Expression::If {
            condition: Box::new(Expression::Infix {
                left: Box::new(Expression::Identifier(Identifier("x".to_string()))),
                operator: Infix::LessThan,
                right: Box::new(Expression::Identifier(Identifier("y".to_string()))),
            }),
            consequence: BlockStatement {
                statements: vec![Statement::Expression(Expression::Identifier(Identifier(
                    "x".to_string(),
                )))],
            },
            alternative: Some(BlockStatement {
                statements: vec![Statement::Expression(Expression::Identifier(Identifier(
                    "y".to_string(),
                )))],
            }),
        });

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

                let statement = program.statements.first().unwrap();
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";

        let expected = Statement::Expression(Expression::Function {
            parameters: vec![Identifier("x".to_string()), Identifier("y".to_string())],
            body: BlockStatement {
                statements: vec![Statement::Expression(Expression::Infix {
                    left: Box::new(Expression::Identifier(Identifier("x".to_string()))),
                    operator: Infix::Plus,
                    right: Box::new(Expression::Identifier(Identifier("y".to_string()))),
                })],
            },
        });

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
                let statement = program.statements.first().unwrap();
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn function_parameters() {
        struct Test<'a> {
            input: &'a str,
            parameters: Vec<Identifier>,
        }

        let tests = vec![
            Test {
                input: "fn() {};",
                parameters: vec![],
            },
            Test {
                input: "fn(x) {};",
                parameters: vec![Identifier("x".to_string())],
            },
            Test {
                input: "fn(x, y, z) {};",
                parameters: vec![
                    Identifier("x".to_string()),
                    Identifier("y".to_string()),
                    Identifier("z".to_string()),
                ],
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
                    let statement = program.statements.first().unwrap();

                    assert_eq!(
                        Statement::Expression(Expression::Function {
                            parameters: test.parameters,
                            body: BlockStatement { statements: vec![] },
                        }),
                        *statement
                    );
                }
            }
        }
    }

    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let expected = Statement::Expression(Expression::Call {
            function: Box::new(Expression::Identifier(Identifier("add".to_string()))),
            arguments: vec![
                Expression::Literal(Literal::Integer(1)),
                Expression::Infix {
                    left: Box::new(Expression::Literal(Literal::Integer(2))),
                    operator: Infix::Multiply,
                    right: Box::new(Expression::Literal(Literal::Integer(3))),
                },
                Expression::Infix {
                    left: Box::new(Expression::Literal(Literal::Integer(4))),
                    operator: Infix::Plus,
                    right: Box::new(Expression::Literal(Literal::Integer(5))),
                },
            ],
        });

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
                let statement = program.statements.first().unwrap();
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn string_literal() {
        let input = "\"hello world\";";

        let expected = Statement::Expression(Expression::Literal(Literal::String(
            "hello world".to_string(),
        )));

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
                let statement = program.statements.first().unwrap();
                assert_eq!(statement, &expected);
            }
        }
    }

    #[test]
    fn parse_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

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
                let statement = program.statements.first().unwrap();

                assert_eq!(
                    Statement::Expression(Expression::Literal(Literal::Array(vec![
                        Expression::Literal(Literal::Integer(1)),
                        Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(2))),
                            operator: Infix::Multiply,
                            right: Box::new(Expression::Literal(Literal::Integer(2))),
                        },
                        Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(3))),
                            operator: Infix::Plus,
                            right: Box::new(Expression::Literal(Literal::Integer(3))),
                        },
                    ]))),
                    *statement
                );
            }
        }
    }

    #[test]
    fn parse_index_operator() {
        let input = "myArray[1 + 1]";

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
                let statement = program.statements.first().unwrap();

                assert_eq!(
                    Statement::Expression(Expression::Index {
                        left: Box::new(Expression::Identifier(Identifier("myArray".to_string()))),
                        index: Box::new(Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(1))),
                            operator: Infix::Plus,
                            right: Box::new(Expression::Literal(Literal::Integer(1))),
                        }),
                    }),
                    *statement
                );
            }
        }
    }

    #[test]
    fn hash() {
        struct Test {
            input: &'static str,
            expected: Vec<(Expression, Expression)>,
        }

        let tests = vec![
            Test {
                input: r#"{"one": 1, "two": 2, "three": 3}"#,
                expected: vec![
                    (
                        Expression::Literal(Literal::String("one".to_string())),
                        Expression::Literal(Literal::Integer(1)),
                    ),
                    (
                        Expression::Literal(Literal::String("two".to_string())),
                        Expression::Literal(Literal::Integer(2)),
                    ),
                    (
                        Expression::Literal(Literal::String("three".to_string())),
                        Expression::Literal(Literal::Integer(3)),
                    ),
                ],
            },
            Test {
                input: r#"{}"#,
                expected: vec![],
            },
            Test {
                input: r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#,
                expected: vec![
                    (
                        Expression::Literal(Literal::String("one".to_string())),
                        Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(0))),
                            operator: Infix::Plus,
                            right: Box::new(Expression::Literal(Literal::Integer(1))),
                        },
                    ),
                    (
                        Expression::Literal(Literal::String("two".to_string())),
                        Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(10))),
                            operator: Infix::Minus,
                            right: Box::new(Expression::Literal(Literal::Integer(8))),
                        },
                    ),
                    (
                        Expression::Literal(Literal::String("three".to_string())),
                        Expression::Infix {
                            left: Box::new(Expression::Literal(Literal::Integer(15))),
                            operator: Infix::Divide,
                            right: Box::new(Expression::Literal(Literal::Integer(5))),
                        },
                    ),
                ],
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
                    let statement = program.statements.first().unwrap();

                    assert_eq!(
                        Statement::Expression(Expression::Literal(Literal::Hash(test.expected))),
                        *statement
                    );
                }
            }
        }
    }
}
