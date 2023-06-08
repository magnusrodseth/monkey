use crate::{ast::Program, object::Object};

struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate(&mut self, program: Program) -> Option<Object> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;
}
