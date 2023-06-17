use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockStatement, Expression, Identifier, Infix, Literal, Prefix, Program, Statement},
    environment::Environment,
    object::Object,
};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new(environment: Rc<RefCell<Environment>>) -> Self {
        Self { environment }
    }

    pub fn evaluate(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program.statements {
            if statement == Statement::Empty {
                continue;
            }

            match self.evaluate_statement(statement) {
                Some(Object::Return(object)) => return Some(*object),
                Some(Object::Error(error)) => return Some(Object::Error(error)),
                object => result = object,
            }
        }

        result
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.evaluate_expression(expression),
            Statement::Return(expression) => self.evaluate_return_statement(expression),
            Statement::Let { identifier, value } => self.evaluate_let_statement(identifier, value),
            _ => None,
        }
    }

    fn evaluate_block_statement(&mut self, block: BlockStatement) -> Option<Object> {
        let mut result = None;

        for statement in block.statements {
            match self.evaluate_statement(statement) {
                Some(Object::Return(object)) => return Some(Object::Return(object)),
                Some(Object::Error(error)) => return Some(Object::Error(error)),
                object => result = object,
            }
        }

        result
    }

    fn evaluate_expression(&mut self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.evaluate_literal(literal)),
            Expression::Prefix { operator, right } => {
                let right = self.evaluate_expression(*right)?;
                Some(self.evaluate_prefix_expression(operator, right))
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate_expression(*left)?;
                let right = self.evaluate_expression(*right)?;
                Some(self.evaluate_infix_expression(operator, left, right))
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.evaluate_if_expression(condition, consequence, alternative),
            Expression::Identifier(identifier) => Some(self.evaluate_identifier(identifier)),
            Expression::Function { parameters, body } => {
                Some(self.evaluate_function(parameters, body))
            }
            Expression::Call {
                function,
                arguments,
            } => Some(self.evaluate_call_expression(function, arguments)),
        }
    }

    fn evaluate_literal(&self, literal: Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(value),
            Literal::Boolean(value) => self.native_boolean_to_boolean_object(value),
            Literal::String(value) => Object::String(value),
        }
    }

    /// We need to convert Rust's native boolean type to our own Boolean object.
    /// We use global constants for TRUE and FALSE, in order to only allocate them once.
    /// This way, we don't have the create a new Boolean object every time we evaluate a boolean expression.
    fn native_boolean_to_boolean_object(&self, input: bool) -> Object {
        if input {
            TRUE
        } else {
            FALSE
        }
    }

    fn evaluate_prefix_expression(&self, operator: Prefix, right: Object) -> Object {
        match operator {
            Prefix::Not => self.evaluate_not_operator_expression(right),
            Prefix::Minus => self.evaluate_minus_operator_expression(right),
            _ => self.error(format!(
                "unknown operator: {}{}",
                operator,
                right.to_string()
            )),
        }
    }

    fn evaluate_not_operator_expression(&self, right: Object) -> Object {
        match right {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        }
    }

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Null | Object::Boolean(false) => false,
            _ => true,
        }
    }

    fn error(&self, message: String) -> Object {
        Object::Error(message)
    }

    fn evaluate_minus_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(value) => Object::Integer(-value),
            _ => self.error(format!("unknown operator: -{}", right.to_string())),
        }
    }

    fn evaluate_infix_expression(&self, operator: Infix, left: Object, right: Object) -> Object {
        match left {
            Object::Integer(left_value) => {
                if let Object::Integer(right_value) = right {
                    self.evaluate_integer_infix_expression(operator, left_value, right_value)
                } else {
                    self.error(format!("type mismatch: {} {} {}", left, operator, right,))
                }
            }

            Object::Boolean(left_value) => {
                if let Object::Boolean(right_value) = right {
                    self.evaluate_boolean_infix_expression(operator, left_value, right_value)
                } else {
                    self.error(format!("type mismatch: {} {} {}", left, operator, right,))
                }
            }

            Object::String(ref left_value) => {
                if let Object::String(right_value) = right {
                    self.evaluate_string_infix_expression(operator, left_value, &right_value)
                } else {
                    self.error(format!("type mismatch: {} {} {}", left, operator, right,))
                }
            }

            _ => self.error(format!("unknown operator: {} {} {}", left, operator, right,)),
        }
    }

    fn evaluate_integer_infix_expression(&self, operator: Infix, left: i64, right: i64) -> Object {
        match operator {
            Infix::Plus => Object::Integer(left + right),
            Infix::Minus => Object::Integer(left - right),
            Infix::Multiply => Object::Integer(left * right),
            Infix::Divide => Object::Integer(left / right),
            Infix::LessThan => self.native_boolean_to_boolean_object(left < right),
            Infix::GreaterThan => self.native_boolean_to_boolean_object(left > right),
            Infix::Equal => self.native_boolean_to_boolean_object(left == right),
            Infix::NotEqual => self.native_boolean_to_boolean_object(left != right),
            Infix::LessThanOrEqual => self.native_boolean_to_boolean_object(left <= right),
            Infix::GreaterThanOrEqual => self.native_boolean_to_boolean_object(left >= right),
        }
    }

    fn evaluate_boolean_infix_expression(
        &self,
        operator: Infix,
        left: bool,
        right: bool,
    ) -> Object {
        match operator {
            Infix::Equal => self.native_boolean_to_boolean_object(left == right),
            Infix::NotEqual => self.native_boolean_to_boolean_object(left != right),
            Infix::LessThan => self.native_boolean_to_boolean_object(left < right),
            Infix::GreaterThan => self.native_boolean_to_boolean_object(left > right),
            Infix::LessThanOrEqual => self.native_boolean_to_boolean_object(left <= right),
            Infix::GreaterThanOrEqual => self.native_boolean_to_boolean_object(left >= right),
            _ => self.error(format!("unknown operator: {} {} {}", left, operator, right)),
        }
    }

    fn evaluate_if_expression(
        &mut self,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Option<Object> {
        let condition = self.evaluate_expression(*condition)?;

        if self.is_truthy(condition) {
            self.evaluate_block_statement(consequence)
        } else {
            match alternative {
                Some(alternative) => self.evaluate_block_statement(alternative),
                None => None,
            }
        }
    }

    fn evaluate_return_statement(&mut self, expression: Expression) -> Option<Object> {
        let value = self.evaluate_expression(expression)?;

        if self.is_error(&value) {
            return Some(value);
        }

        Some(Object::Return(Box::new(value)))
    }

    fn is_error(&self, value: &Object) -> bool {
        match value {
            Object::Error(_) => true,
            _ => false,
        }
    }

    fn evaluate_let_statement(
        &mut self,
        identifier: Identifier,
        expression: Expression,
    ) -> Option<Object> {
        let value = self.evaluate_expression(expression)?;

        if self.is_error(&value) {
            return Some(value);
        }

        let Identifier(identifier) = identifier;
        self.environment.borrow_mut().set(identifier, value.clone());
        None
    }

    fn evaluate_identifier(&self, identifier: Identifier) -> Object {
        let Identifier(identifier) = identifier;
        match self.environment.borrow_mut().get(identifier.clone()) {
            Some(value) => value.clone(),
            None => Object::Error(format!("identifier not found: {}", identifier)),
        }
    }

    fn evaluate_function(&self, parameters: Vec<Identifier>, body: BlockStatement) -> Object {
        Object::Function {
            parameters,
            body,
            environment: Rc::clone(&self.environment),
        }
    }

    fn evaluate_call_expression(
        &mut self,
        function: Box<Expression>,
        arguments: Vec<Expression>,
    ) -> Object {
        let arguments = arguments
            .iter()
            .map(|arg| {
                self.evaluate_expression(arg.clone())
                    .unwrap_or(Object::Null)
            })
            .collect::<Vec<_>>();

        let (parameters, body, environment) = match self.evaluate_expression(*function) {
            Some(Object::Function {
                parameters,
                body,
                environment,
            }) => (parameters, body, environment),
            Some(object) => return self.error(format!("not a function: {}", object)),
            None => return NULL,
        };

        if parameters.len() != arguments.len() {
            return self.error(format!(
                "wrong number of arguments: want={}, got={}",
                parameters.len(),
                arguments.len()
            ));
        }

        let current_environment = Rc::clone(&self.environment);
        let mut function_environment = Environment::new_enclosed(environment);

        for (identifier, object) in parameters.iter().zip(arguments.iter()) {
            let Identifier(identifier) = identifier;
            function_environment.set(identifier.clone(), object.clone());
        }

        self.environment = Rc::new(RefCell::new(function_environment));

        let body = self.evaluate_block_statement(body);

        self.environment = current_environment;

        match body {
            Some(object) => object,
            _ => Object::Null,
        }
    }

    fn evaluate_string_infix_expression(
        &self,
        operator: Infix,
        left: &String,
        right: &String,
    ) -> Object {
        match operator {
            Infix::Plus => Object::String(format!("{}{}", left, right)),
            _ => self.error(format!("unknown operator: {} {} {}", left, operator, right)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{environment::Environment, lexer::Lexer, parser::Parser};

    use super::*;

    macro_rules! print_parser_errors {
        ($errors:expr) => {
            for error in $errors {
                println!("{}", error);
            }
        };
    }

    macro_rules! assert_integer_object {
        ($object:expr, $expected:expr) => {
            match $object {
                Some(Object::Integer(value)) => assert_eq!(value, $expected),
                _ => panic!("Object is not Integer. got={:?}", $object),
            }
        };
    }

    macro_rules! assert_boolean_object {
        ($object:expr, $expected:expr) => {
            match $object {
                Some(Object::Boolean(value)) => assert_eq!(value, $expected),
                _ => panic!("Object is not Boolean. got={:?}", $object),
            }
        };
    }

    macro_rules! assert_string_object {
        ($object:expr, $expected:expr) => {
            match $object {
                Some(Object::String(value)) => assert_eq!(value, $expected),
                _ => panic!("Object is not String. got={:?}", $object),
            }
        };
    }

    fn evaluate(input: String) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Environment::new())));

        print_parser_errors!(parser.errors());

        return match program {
            Err(_) => None,
            Ok(program) => evaluator.evaluate(program),
        };
    }

    #[test]
    fn evaluate_integer() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            // Simple integer
            Test {
                input: String::from("5"),
                expected: 5,
            },
            // Prefix operator
            Test {
                input: String::from("-5"),
                expected: -5,
            },
            Test {
                input: String::from("-10"),
                expected: -10,
            },
            Test {
                input: String::from("-50"),
                expected: -50,
            },
            // Infix operator
            Test {
                input: String::from("5 + 5 + 5 + 5 - 10"),
                expected: 10,
            },
            Test {
                input: String::from("2 * 2 * 2 * 2 * 2"),
                expected: 32,
            },
            Test {
                input: String::from("-50 + 100 + -50"),
                expected: 0,
            },
            Test {
                input: String::from("5 * 2 + 10"),
                expected: 20,
            },
            Test {
                input: String::from("5 + 2 * 10"),
                expected: 25,
            },
            Test {
                input: String::from("20 + 2 * -10"),
                expected: 0,
            },
            Test {
                input: String::from("50 / 2 * 2 + 10"),
                expected: 60,
            },
            Test {
                input: String::from("2 * (5 + 10)"),
                expected: 30,
            },
            Test {
                input: String::from("3 * 3 * 3 + 10"),
                expected: 37,
            },
            Test {
                input: String::from("3 * (3 * 3) + 10"),
                expected: 37,
            },
            Test {
                input: String::from("(5 + 10 * 2 + 15 / 3) * 2 + -10"),
                expected: 50,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_integer_object!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_boolean() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = vec![
            // Simple boolean
            Test {
                input: String::from("true"),
                expected: true,
            },
            Test {
                input: String::from("false"),
                expected: false,
            },
            // Infix operator
            Test {
                input: String::from("1 < 2"),
                expected: true,
            },
            Test {
                input: String::from("1 > 2"),
                expected: false,
            },
            Test {
                input: String::from("1 < 1"),
                expected: false,
            },
            Test {
                input: String::from("1 > 1"),
                expected: false,
            },
            Test {
                input: String::from("1 == 1"),
                expected: true,
            },
            Test {
                input: String::from("1 != 1"),
                expected: false,
            },
            Test {
                input: String::from("1 == 2"),
                expected: false,
            },
            Test {
                input: String::from("1 != 2"),
                expected: true,
            },
            Test {
                input: String::from("true == true"),
                expected: true,
            },
            Test {
                input: String::from("false == false"),
                expected: true,
            },
            Test {
                input: String::from("true == false"),
                expected: false,
            },
            Test {
                input: String::from("true != false"),
                expected: true,
            },
            Test {
                input: String::from("false != true"),
                expected: true,
            },
            Test {
                input: String::from("(1 < 2) == true"),
                expected: true,
            },
            Test {
                input: String::from("(1 < 2) == false"),
                expected: false,
            },
            Test {
                input: String::from("(1 > 2) == true"),
                expected: false,
            },
            Test {
                input: String::from("(1 > 2) == false"),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_boolean_object!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_not_operator() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: String::from("!true"),
                expected: false,
            },
            Test {
                input: String::from("!false"),
                expected: true,
            },
            Test {
                input: String::from("!5"),
                expected: false,
            },
            Test {
                input: String::from("!!true"),
                expected: true,
            },
            Test {
                input: String::from("!!false"),
                expected: false,
            },
            Test {
                input: String::from("!!5"),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_boolean_object!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_if_else() {
        struct Test {
            input: String,
            expected: Option<Object>,
        }

        let tests = vec![
            Test {
                input: String::from("if (true) { 10 }"),
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: String::from("if (false) { 10 }"),
                expected: None,
            },
            Test {
                input: String::from("if (1) { 10 }"),
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: String::from("if (1 < 2) { 10 }"),
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: String::from("if (1 > 2) { 10 }"),
                expected: None,
            },
            Test {
                input: String::from("if (1 > 2) { 10 } else { 20 }"),
                expected: Some(Object::Integer(20)),
            },
            Test {
                input: String::from("if (1 < 2) { 10 } else { 20 }"),
                expected: Some(Object::Integer(10)),
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_eq!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_return() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: String::from("return 10;"),
                expected: 10,
            },
            Test {
                input: String::from("return 10; 9;"),
                expected: 10,
            },
            Test {
                input: String::from("return 2 * 5; 9;"),
                expected: 10,
            },
            Test {
                input: String::from("9; return 2 * 5; 9;"),
                expected: 10,
            },
            Test {
                input: String::from(
                    r#"
                    if (10 > 1) {
                        if (10 > 1) {
                            return 10;
                        }

                        return 1;
                    }
                    "#,
                ),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_eq!(evaluated, Some(Object::Integer(test.expected)));
        }
    }

    #[test]
    fn error_handling() {
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![
            Test {
                input: String::from("5 + true;"),
                expected: String::from("type mismatch: 5 + true"),
            },
            Test {
                input: String::from("5 + true; 5;"),
                expected: String::from("type mismatch: 5 + true"),
            },
            Test {
                input: String::from("-true"),
                expected: String::from("unknown operator: -true"),
            },
            Test {
                input: String::from("true + false;"),
                expected: String::from("unknown operator: true + false"),
            },
            Test {
                input: String::from("5; true + false; 5"),
                expected: String::from("unknown operator: true + false"),
            },
            Test {
                input: String::from("if (10 > 1) { true + false; }"),
                expected: String::from("unknown operator: true + false"),
            },
            Test {
                input: String::from(
                    r#"
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }

                        return 1;
                    }
                    "#,
                ),
                expected: String::from("unknown operator: true + false"),
            },
            Test {
                input: String::from("foobar"),
                expected: String::from("identifier not found: foobar"),
            },
            Test {
                input: String::from(r#""Hello" - "World""#),
                expected: String::from("unknown operator: Hello - World"),
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_eq!(evaluated, Some(Object::Error(test.expected)));
        }
    }

    #[test]
    fn evaluate_let_statement() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: String::from("let a = 5; a;"),
                expected: 5,
            },
            Test {
                input: String::from("let a = 5 * 5; a;"),
                expected: 25,
            },
            Test {
                input: String::from("let a = 5; let b = a; b;"),
                expected: 5,
            },
            Test {
                input: String::from("let a = 5; let b = a; let c = a + b + 5; c;"),
                expected: 15,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_integer_object!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_function() {
        let input = String::from("fn(x) { x + 2; };");
        let evaluated = evaluate(input);

        match evaluated {
            Some(Object::Function {
                parameters,
                body,
                environment: _,
            }) => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters[0].to_string(), "x");
                assert_eq!(body.to_string(), "(x + 2)");
            }
            _ => panic!("object is not Function. got={:?}", evaluated),
        }
    }

    #[test]
    fn evaluate_function_application() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: String::from("let identity = fn(x) { x; }; identity(5);"),
                expected: 5,
            },
            Test {
                input: String::from("let identity = fn(x) { return x; }; identity(5);"),
                expected: 5,
            },
            Test {
                input: String::from("let double = fn(x) { x * 2; }; double(5);"),
                expected: 10,
            },
            Test {
                input: String::from("let add = fn(x, y) { x + y; }; add(5, 5);"),
                expected: 10,
            },
            Test {
                input: String::from("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"),
                expected: 20,
            },
            Test {
                input: String::from("fn(x) { x; }(5)"),
                expected: 5,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_integer_object!(evaluated, test.expected);
        }
    }

    #[test]
    fn evaluate_closures() {
        let input = String::from(
            r#"
            let newAdder = fn(x) {
                fn(y) { x + y };
            };

            let addTwo = newAdder(2);
            addTwo(2);
            "#,
        );

        assert_integer_object!(evaluate(input.clone()), 4);
    }

    #[test]
    fn evaluate_string() {
        let input = String::from(r#""Hello World!""#);
        assert_string_object!(evaluate(input.clone()), "Hello World!");
    }

    #[test]
    fn evaluate_string_concatenation() {
        let input = String::from(r#""Hello" + " " + "World!""#);
        assert_string_object!(evaluate(input.clone()), "Hello World!");
    }
}
