use std::any::Any;

use crate::token::Token;

pub trait Node: Any {
    fn token(&self) -> Token;
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
}
