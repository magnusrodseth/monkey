use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Divide => write!(f, "/"),
            Infix::Multiply => write!(f, "*"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::GreaterThanOrEqual => write!(f, ">="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::LessThanOrEqual => write!(f, "<="),
            Infix::LessThan => write!(f, "<"),
        }
    }
}

#[derive(PartialEq, Clone, Eq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    Prefix {
        operator: Prefix,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Infix,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    For {
        let_statement: Box<Statement>,
        condition: Box<Expression>,
        update: Box<Expression>,
        body: BlockStatement,
    },
    Assign {
        name: Identifier,
        value: Box<Expression>,
    },
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(Identifier(value)) => write!(f, "{}", value),
            Expression::Literal(literal) => match literal {
                Literal::Integer(integer) => write!(f, "{}", integer),
                Literal::Boolean(boolean) => write!(f, "{}", boolean),
                Literal::String(string) => write!(f, "{}", string),
                Literal::Array(array) => {
                    let mut string = String::new();

                    string.push_str("[");

                    for (index, element) in array.iter().enumerate() {
                        string.push_str(&element.to_string());

                        if index != array.len() - 1 {
                            string.push_str(", ");
                        }
                    }

                    string.push_str("]");

                    write!(f, "{}", string)
                }
                Literal::Hash(map) => {
                    let mut string = String::new();

                    string.push_str("{");

                    for (index, (key, value)) in map.iter().enumerate() {
                        string.push_str(&key.to_string());
                        string.push_str(": ");
                        string.push_str(&value.to_string());

                        if index != map.len() - 1 {
                            string.push_str(", ");
                        }
                    }

                    string.push_str("}");

                    write!(f, "{}", string)
                }
            },
            Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let mut string = format!("if {} {}", condition, consequence);

                if let Some(alternative) = alternative {
                    string.push_str(&format!(" else {}", alternative));
                }

                write!(f, "{}", string)
            }
            Expression::Function { parameters, body } => {
                let mut string = String::new();

                string.push_str("fn(");

                for (index, parameter) in parameters.iter().enumerate() {
                    string.push_str(&parameter.0);

                    if index != parameters.len() - 1 {
                        string.push_str(", ");
                    }
                }

                string.push_str(") { ");
                string.push_str(&body.to_string());
                string.push_str(" }");

                write!(f, "{}", string)
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let mut string = String::new();

                string.push_str(&function.to_string());
                string.push_str("(");

                for (index, argument) in arguments.iter().enumerate() {
                    string.push_str(&argument.to_string());

                    if index != arguments.len() - 1 {
                        string.push_str(", ");
                    }
                }

                string.push_str(")");

                write!(f, "{}", string)
            }
            Expression::Index { left, index } => {
                write!(f, "({}[{}])", left.to_string(), index.to_string())
            }
            Expression::For {
                let_statement,
                condition,
                update,
                body,
            } => {
                write!(
                    f,
                    "for ({}; {}; {}) {}",
                    let_statement.to_string(),
                    condition.to_string(),
                    update.to_string(),
                    body.to_string()
                )
            }
            Expression::Assign { name, value } => {
                write!(f, "{} = {}", name.0, value.to_string())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Hash(Vec<(Expression, Expression)>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let {
        identifier: Identifier,
        value: Expression,
    },
    Return(Expression),
    Expression(Expression),
    Empty,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { identifier, value } => {
                write!(f, "let {} = {};", identifier.0, value.to_string())
            }
            Statement::Return(expression) => write!(f, "return {};", expression.to_string()),
            Statement::Expression(expression) => write!(f, "{}", expression.to_string()),
            Statement::Empty => write!(f, ""),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut block = String::new();

        for statement in self.statements.iter() {
            block.push_str(&statement.to_string());
        }

        write!(f, "{}", block)
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessThanOrGreaterThan,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn to_string(&self) -> String {
        let mut program = String::new();

        for statement in self.statements.iter() {
            program.push_str(&statement.to_string());
        }

        program
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement_to_string() {
        let program = Program {
            statements: vec![Statement::Let {
                identifier: Identifier("myVar".to_string()),
                value: Expression::Identifier(Identifier("anotherVar".to_string())),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn infix_expression_to_string() {
        let program = Program {
            statements: vec![Statement::Expression(Expression::Infix {
                left: Box::new(Expression::Literal(Literal::Integer(5))),
                operator: Infix::Plus,
                right: Box::new(Expression::Literal(Literal::Integer(10))),
            })],
        };

        assert_eq!(program.to_string(), "(5 + 10)");
    }

    #[test]
    fn prefix_expression_to_string() {
        let program = Program {
            statements: vec![Statement::Expression(Expression::Prefix {
                operator: Prefix::Minus,
                right: Box::new(Expression::Literal(Literal::Integer(5))),
            })],
        };

        assert_eq!(program.to_string(), "(-5)");
    }

    #[test]
    fn integer_literal_to_string() {
        let program = Program {
            statements: vec![Statement::Expression(Expression::Literal(
                Literal::Integer(5),
            ))],
        };

        assert_eq!(program.to_string(), "5");
    }

    #[test]
    fn identifier_to_string() {
        let program = Program {
            statements: vec![Statement::Expression(Expression::Identifier(Identifier(
                "x".into(),
            )))],
        };

        assert_eq!(program.to_string(), "x");
    }

    #[test]
    fn return_statement_to_string() {
        let program = Program {
            statements: vec![Statement::Return(Expression::Identifier(Identifier(
                "x".into(),
            )))],
        };

        assert_eq!(program.to_string(), "return x;");
    }
}
