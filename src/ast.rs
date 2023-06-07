use std::any::Any;

use crate::token::Token;

pub trait Node: Any {
    fn token(&self) -> Token;
    fn to_string(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
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

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {
        todo!()
    }
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
    // TODO: Remove option after implementing expressions
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {
        todo!()
    }
}

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
            self.token.to_string().to_lowercase(),
            self.name.to_string(),
            self.value.as_ref().expect("No value found").to_string()
        )
    }
}

pub struct ReturnStatement {
    pub token: Token,
    // TODO: Remove option after implementing expressions
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {
        todo!()
    }
}

impl Node for ReturnStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn to_string(&self) -> String {
        if let Some(return_value) = &self.return_value {
            format!("{} {};", self.token, return_value.to_string())
        } else {
            format!("{};", self.token)
        }
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    // TODO: Remove option after implementing expressions
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {
        todo!()
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ast_to_string() {
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
}
