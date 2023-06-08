use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    ast::{
        BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
        Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        Program, ReturnStatement, Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
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
    precedences: HashMap<Token, Precedence>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
            precedences: Self::precedences(),
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
            token.formatted(),
            self.peek_token.formatted()
        )));
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedences.get(&self.peek_token) {
            precedence.clone()
        } else {
            Precedence::Lowest
        }
    }

    fn current_precedence(&self) -> Precedence {
        if let Some(precedence) = self.precedences.get(&self.current_token) {
            precedence.clone()
        } else {
            Precedence::Lowest
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse(&mut self) -> Result<Program, ParserError> {
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

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(LetStatement {
            token,
            name: identifier,
            value,
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

        let return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Ok(Box::new(ReturnStatement {
            token,
            return_value,
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
                format!(
                    "Expected expression, got {}",
                    self.current_token.formatted()
                )
                .into(),
            )),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        // Parse prefix
        let mut left = match self.current_token {
            Token::Integer(_) => self.parse_integer_literal(),
            Token::Identifier(_) => self.parse_identifier(),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix(),
            Token::True | Token::False => self.parse_boolean(),
            Token::LeftParenthesis => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
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
            match self.precedences.get(&self.peek_token) {
                Some(precedence) => match precedence {
                    Precedence::Call => {
                        self.next_token();
                        left = self.parse_call_expression(left.expect("Expected left expression"));
                    }
                    _ => {
                        self.next_token();
                        left = self.parse_infix(left.expect("Expected left expression"));
                    }
                },
                _ => return left,
            }
        }

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
        let token = self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        match right {
            Some(right) => Some(Box::new(PrefixExpression { token, right })),
            None => None,
        }
    }

    fn parse_infix(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);

        match right {
            Some(right) => Some(Box::new(InfixExpression { token, left, right })),
            None => None,
        }
    }

    fn precedences() -> HashMap<Token, Precedence> {
        let mut precedences = HashMap::new();

        precedences.insert(Token::Equal, Precedence::Equals);
        precedences.insert(Token::NotEqual, Precedence::Equals);

        precedences.insert(Token::LessThan, Precedence::LessThanOrGreaterThan);
        precedences.insert(Token::GreaterThan, Precedence::LessThanOrGreaterThan);

        precedences.insert(Token::Plus, Precedence::Sum);
        precedences.insert(Token::Minus, Precedence::Sum);

        precedences.insert(Token::Slash, Precedence::Product);
        precedences.insert(Token::Asterisk, Precedence::Product);

        precedences.insert(Token::LeftParenthesis, Precedence::Call);

        precedences
    }

    fn parse_boolean(&self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(Token::True),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.peek_and_expect(Token::RightParenthesis) {
            return None;
        }

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();

        if !self.peek_and_expect(Token::LeftParenthesis) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.peek_and_expect(Token::RightParenthesis) {
            return None;
        }

        if !self.peek_and_expect(Token::LeftBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        if self.peek_token_is(&Token::Else) {
            self.next_token();

            if !self.peek_and_expect(Token::LeftBrace) {
                return None;
            }

            let alternative = self.parse_block_statement();

            return Some(Box::new(IfExpression {
                token,
                condition: condition.expect("Expected condition"),
                consequence,
                alternative: Some(alternative),
            }));
        }

        Some(Box::new(IfExpression {
            token,
            condition: condition.expect("Expected condition"),
            consequence,
            alternative: None,
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.current_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(Token::RightBrace) && !self.current_token_is(Token::EOF) {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        BlockStatement { token, statements }
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();

        if !self.peek_and_expect(Token::LeftParenthesis) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.peek_and_expect(Token::LeftBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(Box::new(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::RightParenthesis) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let identifier = Identifier {
            token: self.current_token.clone(),
            value: if let Token::Identifier(value) = self.current_token.clone() {
                value
            } else {
                return identifiers;
            },
        };
        identifiers.push(identifier);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            let identifier = Identifier {
                token: self.current_token.clone(),
                value: if let Token::Identifier(value) = self.current_token.clone() {
                    value
                } else {
                    return identifiers;
                },
            };
            identifiers.push(identifier);
        }

        if !self.peek_and_expect(Token::RightParenthesis) {
            return Vec::new();
        }

        identifiers
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Option<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let arguments = self.parse_call_arguments();

        Some(Box::new(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Vec<Box<dyn Expression>> {
        let mut arguments = Vec::new();

        if self.peek_token_is(&Token::RightParenthesis) {
            self.next_token();
            return arguments;
        }

        self.next_token();

        let expression = self
            .parse_expression(Precedence::Lowest)
            .expect("Expected expression");
        arguments.push(expression);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();

            let expression = self
                .parse_expression(Precedence::Lowest)
                .expect("Expected expression");
            arguments.push(expression);
        }

        if !self.peek_and_expect(Token::RightParenthesis) {
            return Vec::new();
        }

        arguments
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        BlockStatement, CallExpression, ExpressionStatement, FunctionLiteral, IfExpression,
        InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, ReturnStatement,
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

    macro_rules! assert_identifier_eq {
        ($identifier:expr, $expected_value:expr) => {
            assert_eq!($identifier.value, $expected_value);
            assert_eq!($identifier.token, Token::Identifier($expected_value));
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
                        assert_identifier_eq!(identifier, "foobar".to_string());
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
    fn infix_expressions() {
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

                            assert_eq!(infix.token.formatted(), test.operator);

                            assert_integer_literal_eq!(left, test.left_value);
                        }
                    } else {
                        panic!("Expected statement, got something else");
                    }
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

    macro_rules! assert_if_else_eq {
        ($expression:expr, $condition:expr, $consequence:expr, $alternative:expr, $has_alternative:expr) => {
            let if_expression = cast_into!($expression, IfExpression);
            let condition = cast_into!(if_expression.condition, InfixExpression);
            assert_eq!(condition.to_string(), $condition);

            let consequence = cast_into!(if_expression.consequence, BlockStatement);
            assert_eq!(consequence.statements.len(), 1);

            let consequence_expression =
                cast_into!(consequence.statements.first().unwrap(), ExpressionStatement);
            let consequence_identifier = cast_into!(
                consequence_expression.expression.as_ref().unwrap(),
                Identifier
            );
            assert_identifier_eq!(consequence_identifier, $consequence);

            if $has_alternative {
                assert!(if_expression.alternative.is_some());

                let alternative =
                    cast_into!(if_expression.alternative.as_ref().unwrap(), BlockStatement);

                assert_eq!(alternative.statements.len(), 1);
                let alternative_statement = alternative.statements.first();
                if let Some(alternative_statement) = alternative_statement {
                    let alternative = cast_into!(alternative_statement, ExpressionStatement);
                    if let Some(expression) = &alternative.expression {
                        let alternative_identifier = cast_into!(expression, Identifier);
                        assert_identifier_eq!(alternative_identifier, "y".to_string());
                    } else {
                        panic!("Expected statement, got something else");
                    }
                } else {
                    panic!("Expected statement, got something else");
                }
            } else {
                assert!(if_expression.alternative.is_none());
            }
        };
    }

    #[test]
    fn simple_if() {
        let input = "if (x < y) { x }";

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
                let expression = cast_into!(statement, ExpressionStatement);

                if let Some(if_expression) = &expression.expression {
                    assert_if_else_eq!(
                        if_expression,
                        "(x < y)".to_string(),
                        "x".to_string(),
                        "".to_string(),
                        false
                    );
                } else {
                    panic!("Expected expression, got something else");
                }
            }
        }
    }

    #[test]
    fn if_else() {
        let input = "if (x < y) { x } else { y }";

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
                let expression = cast_into!(statement, ExpressionStatement);

                if let Some(if_expression) = &expression.expression {
                    assert_if_else_eq!(
                        if_expression,
                        "(x < y)".to_string(),
                        "x".to_string(),
                        "y".to_string(),
                        true
                    );
                } else {
                    panic!("Expected expression, got something else");
                }
            }
        }
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";

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
                let expression = cast_into!(statement, ExpressionStatement);
                let function = cast_into!(expression.expression.as_ref().unwrap(), FunctionLiteral);
                assert_eq!(function.parameters.len(), 2);

                let first_parameter = cast_into!(function.parameters.first().unwrap(), Identifier);
                assert_identifier_eq!(first_parameter, "x".to_string());

                let second_parameter = cast_into!(function.parameters.last().unwrap(), Identifier);
                assert_identifier_eq!(second_parameter, "y".to_string());

                let body = cast_into!(function.body, BlockStatement);
                assert_eq!(body.statements.len(), 1);

                let body_statement = body.statements.first().unwrap();
                let body_expression = cast_into!(body_statement, ExpressionStatement);
                let body_infix_expression = cast_into!(
                    body_expression.expression.as_ref().unwrap(),
                    InfixExpression
                );
                let left = cast_into!(body_infix_expression.left, Identifier);
                let right = cast_into!(body_infix_expression.right, Identifier);

                assert_eq!(body_infix_expression.token.formatted(), "+".to_string());
                assert_identifier_eq!(left, "x".to_string());
                assert_identifier_eq!(right, "y".to_string());
            }
        }
    }

    #[test]
    fn function_parameters() {
        struct Test<'a> {
            input: &'a str,
            parameters: Vec<&'a str>,
        }

        let tests = vec![
            Test {
                input: "fn() {};",
                parameters: vec![],
            },
            Test {
                input: "fn(x) {};",
                parameters: vec!["x"],
            },
            Test {
                input: "fn(x, y, z) {};",
                parameters: vec!["x", "y", "z"],
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
                    let expression = cast_into!(statement, ExpressionStatement);
                    let function =
                        cast_into!(expression.expression.as_ref().unwrap(), FunctionLiteral);
                    assert_eq!(function.parameters.len(), test.parameters.len());

                    for parameter in function.parameters.iter() {
                        let identifier = cast_into!(parameter, Identifier);
                        assert_identifier_eq!(identifier, parameter.to_string());
                    }
                }
            }
        }
    }

    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

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
                let expression = cast_into!(statement, ExpressionStatement);
                let call_expression =
                    cast_into!(expression.expression.as_ref().unwrap(), CallExpression);
                let identifier = cast_into!(call_expression.function, Identifier);
                assert_identifier_eq!(identifier, "add".to_string());
                assert_eq!(call_expression.arguments.len(), 3);

                let first_argument = call_expression.arguments.first().unwrap();
                let first_argument_literal = cast_into!(first_argument, IntegerLiteral);
                assert_integer_literal_eq!(first_argument_literal, 1);

                let second_argument = call_expression.arguments.get(1).unwrap();
                let second_argument_infix = cast_into!(second_argument, InfixExpression);
                let second_argument_infix_left =
                    cast_into!(second_argument_infix.left, IntegerLiteral);
                let second_argument_infix_right =
                    cast_into!(second_argument_infix.right, IntegerLiteral);
                assert_eq!(second_argument_infix.token.formatted(), "*".to_string());
                assert_integer_literal_eq!(second_argument_infix_left, 2);
                assert_integer_literal_eq!(second_argument_infix_right, 3);

                let third_argument = call_expression.arguments.last().unwrap();
                let third_argument_infix = cast_into!(third_argument, InfixExpression);
                let third_argument_infix_left =
                    cast_into!(third_argument_infix.left, IntegerLiteral);
                let third_argument_infix_right =
                    cast_into!(third_argument_infix.right, IntegerLiteral);
                assert_eq!(third_argument_infix.token.formatted(), "+".to_string());
                assert_integer_literal_eq!(third_argument_infix_left, 4);
                assert_integer_literal_eq!(third_argument_infix_right, 5);
            }
        }
    }
}
