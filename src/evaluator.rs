use crate::{
    ast::{Expression, Infix, Literal, Prefix, Program, Statement},
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

    fn evaluate_prefix_expression(&self, operator: Prefix, right: Object) -> Object {
        match operator {
            Prefix::Not => self.evaluate_not_operator_expression(right),
            Prefix::Minus => self.evaluate_minus_operator_expression(right),
            _ => NULL,
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
            NULL => false,
            TRUE => true,
            FALSE => false,
            _ => true,
        }
    }

    fn evaluate_minus_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(value) => Object::Integer(-value),
            _ => NULL,
        }
    }

    fn evaluate_infix_expression(&self, operator: Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.evaluate_integer_infix_expression(operator, left, right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => {
                self.evaluate_boolean_infix_expression(operator, left, right)
            }
            _ => NULL,
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
            _ => NULL,
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
}
