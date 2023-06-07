use crate::lexer::Token;

trait Node {
    fn token(&self) -> Token;
}

trait Statement: Node {
    fn statement_node(&self);
}

trait Expression: Node {
    fn expression_node(&self);
}

struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token(&self) -> Token {
        if self.statements.len() > 0 {
            self.statements.get(0).expect("No statements found").token()
        } else {
            Token::EOF
        }
    }
}

struct Identifier {
    token: Token,
    value: String,
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
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
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
}
