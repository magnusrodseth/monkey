use std::any::Any;

use crate::token::Token;

pub trait Node: Any {
    fn token(&self) -> Token;
    fn to_string(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn to_string(&self) -> String {
        let mut output = String::new();

        for statement in self.statements.iter() {
            // If statement is block statement, add curly braces
            if statement.token() == Token::LeftBrace {
                output.push_str("{");
                output.push_str(&statement.to_string());
                output.push_str("}");
            } else {
                output.push_str(&statement.to_string());
            }
        }

        output
    }
}

impl Node for Program {
    fn token(&self) -> Token {
        if self.statements.len() > 0 {
            self.statements.get(0).expect("No statements found").token()
        } else {
            Token::EOF
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = String::new();

        for statement in self.statements.iter() {
            output.push_str(&statement.to_string());
        }

        output
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

impl Node for Identifier {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {}

impl Node for LetStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token.formatted(),
            self.name.to_string(),
            self.value.as_ref().expect("No value found").to_string()
        )
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {}

impl Node for ReturnStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        if let Some(return_value) = &self.return_value {
            format!("{} {};", self.token.formatted(), return_value.to_string())
        } else {
            format!("{};", self.token)
        }
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {}

impl Node for ExpressionStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        if let Some(expression) = &self.expression {
            expression.to_string()
        } else {
            "".into()
        }
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

impl Node for IntegerLiteral {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub right: Box<dyn Expression>,
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

impl Node for PrefixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        format!(
            "({}{})",
            self.token.formatted(),
            self.right.as_ref().to_string()
        )
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

impl Node for InfixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.as_ref().to_string(),
            self.token.formatted(),
            self.right.as_ref().to_string()
        )
    }
}

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

impl Node for Boolean {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

impl Node for IfExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = format!(
            "if {} {}",
            self.condition.to_string(),
            self.consequence.to_string()
        );

        if let Some(alternative) = &self.alternative {
            output.push_str(&format!("else {}", alternative.to_string()));
        }

        output
    }
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Statement for BlockStatement {}

impl Node for BlockStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = String::new();

        for statement in &self.statements {
            output.push_str(&statement.to_string());
        }

        output
    }
}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

impl Node for FunctionLiteral {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        let mut output = format!("{}(", self.token.formatted());

        for (i, parameter) in self.parameters.iter().enumerate() {
            output.push_str(&parameter.to_string());

            if i != self.parameters.len() - 1 {
                output.push_str(", ");
            }
        }

        output.push_str(&format!(") {}", self.body.to_string()));

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement_to_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::Let,
                name: Identifier {
                    token: Token::Identifier("myVar".into()),
                    value: "myVar".into(),
                },
                value: Some(Box::new(Identifier {
                    token: Token::Identifier("anotherVar".into()),
                    value: "anotherVar".into(),
                })),
            })],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn infix_expression_to_string() {
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token::Identifier("x".into()),
                expression: Some(Box::new(InfixExpression {
                    token: Token::Plus,
                    left: Box::new(IntegerLiteral {
                        token: Token::Integer(5),
                        value: 5,
                    }),
                    right: Box::new(IntegerLiteral {
                        token: Token::Integer(10),
                        value: 10,
                    }),
                })),
            })],
        };

        assert_eq!(program.to_string(), "(5 + 10)");
    }

    #[test]
    fn prefix_expression_to_string() {
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token::Identifier("x".into()),
                expression: Some(Box::new(PrefixExpression {
                    token: Token::Minus,
                    right: Box::new(IntegerLiteral {
                        token: Token::Integer(5),
                        value: 5,
                    }),
                })),
            })],
        };

        assert_eq!(program.to_string(), "(-5)");
    }

    #[test]
    fn integer_literal_to_string() {
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token::Identifier("x".into()),
                expression: Some(Box::new(IntegerLiteral {
                    token: Token::Integer(5),
                    value: 5,
                })),
            })],
        };

        assert_eq!(program.to_string(), "5");
    }

    #[test]
    fn identifier_to_string() {
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token::Identifier("x".into()),
                expression: Some(Box::new(Identifier {
                    token: Token::Identifier("x".into()),
                    value: "x".into(),
                })),
            })],
        };

        assert_eq!(program.to_string(), "x");
    }

    #[test]
    fn return_statement_to_string() {
        let program = Program {
            statements: vec![Box::new(ReturnStatement {
                token: Token::Return,
                return_value: Some(Box::new(Identifier {
                    token: Token::Identifier("x".into()),
                    value: "x".into(),
                })),
            })],
        };

        assert_eq!(program.to_string(), "return x;");
    }
}
