use crate::{
    ast::{Expression, Literal, Program, Statement},
    object::Object,
};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate(&mut self, program: Program) -> Option<Object> {
        let mut result = None;

        for statement in program.statements {
            if statement == Statement::Empty {
                continue;
            }

            match self.evaluate_statement(statement) {
                object => result = object,
            }
        }

        result
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Expression(expression) => self.evaluate_expression(expression),
            _ => None,
        }
    }

    fn evaluate_expression(&self, expression: Expression) -> Option<Object> {
        match expression {
            Expression::Literal(literal) => Some(self.evaluate_literal(literal)),
            _ => None,
        }
    }

    fn evaluate_literal(&self, literal: Literal) -> Object {
        match literal {
            Literal::Integer(value) => Object::Integer(value),
            Literal::Boolean(value) => self.native_boolean_to_boolean_object(value),
            _ => NULL,
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
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

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

    fn evaluate(input: String) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator::new();
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

        let tests = vec![Test {
            input: String::from("5"),
            expected: 5,
        }];

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
            Test {
                input: String::from("true"),
                expected: true,
            },
            Test {
                input: String::from("false"),
                expected: false,
            },
        ];

        for test in tests {
            let evaluated = evaluate(test.input);
            assert_boolean_object!(evaluated, test.expected);
        }
    }
}
