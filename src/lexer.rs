#[derive(Debug, PartialEq)]
enum Token {
    // Illegal and end of file
    Illegal,
    EOF,
    // Identifiers and literals
    Identifier(String),
    Integer(i64),

    // Operators
    Plus,
    Assign,

    // Delimiters
    Comma,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,

    // Keywords
    Function,
    Let,
}

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    current_char: char,
}

const EOF: char = '\0';

impl Lexer {
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = EOF;
        } else {
            self.current_char = self
                .input
                .chars()
                .nth(self.read_position)
                .expect("Error reading character");
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            current_char: EOF,
        };
        lexer.read_char();
        lexer
    }

    fn next_token(&mut self) -> Token {
        while self.current_char.is_whitespace() {
            self.read_char();
        }

        let mut read_next_char = true;

        let token = match self.current_char {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            EOF => Token::EOF,
            _ => {
                read_next_char = false;
                if self.is_letter() {
                    let identifier = self.read_identifier();
                    self.lookup_identifier(identifier)
                } else if self.current_char.is_digit(10) {
                    let integer = self.read_integer();
                    Token::Integer(integer)
                } else {
                    Token::Illegal
                }
            }
        };

        if read_next_char {
            self.read_char();
        }

        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.current_char.is_alphabetic() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn lookup_identifier(&self, identifier: String) -> Token {
        match identifier.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Identifier(identifier),
        }
    }

    fn is_letter(&self) -> bool {
        self.current_char >= 'a' && self.current_char <= 'z'
            || self.current_char >= 'A' && self.current_char <= 'Z'
            || self.current_char == '_'
    }

    fn read_integer(&mut self) -> i64 {
        let position = self.position;
        while self.current_char.is_digit(10) {
            self.read_char();
        }

        self.input[position..self.position]
            .parse::<i64>()
            .expect("Error parsing integer")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operators_delimiters() {
        let input = "=+(){},;";

        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LeftParenthesis,
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn identifiers_keywords_numbers() {
        let input = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        ";

        let expected = vec![
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LeftParenthesis,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LeftParenthesis,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RightParenthesis,
            Token::Semicolon,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
