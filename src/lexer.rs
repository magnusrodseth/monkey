use crate::token::Token;

pub struct Lexer {
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

    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            current_char: EOF,
        };
        lexer.read_char();
        lexer
    }

    fn handle_peek(
        &mut self,
        expected_char: char,
        token_if_true: Token,
        token_if_false: Token,
    ) -> Token {
        if self.peek_char() == expected_char {
            self.read_char();
            token_if_true
        } else {
            token_if_false
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut read_next_char = true;

        let token = match self.current_char {
            '=' => self.handle_peek('=', Token::Equal, Token::Assign),
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '!' => self.handle_peek('=', Token::NotEqual, Token::Bang),
            '<' => self.handle_peek('=', Token::LessThanOrEqual, Token::LessThan),
            '>' => self.handle_peek('=', Token::GreaterThanOrEqual, Token::GreaterThan),
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            EOF => Token::EOF,
            '"' => {
                let string = self.read_string();
                Token::String(string)
            }
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
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identifier(identifier),
        }
    }

    fn is_letter(&self) -> bool {
        self.current_char.is_alphabetic() || self.current_char == '_'
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

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            EOF
        } else {
            self.input
                .chars()
                .nth(self.read_position)
                .expect("Error reading character")
        }
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        loop {
            self.read_char();
            if self.current_char == '"' || self.current_char == EOF {
                break;
            }
        }

        self.input[position..self.position].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_lexer_eq {
        ($input:expr, $expected:expr) => {
            let mut lexer = Lexer::new($input.to_string());

            for expected_token in $expected {
                let token = lexer.next_token();
                assert_eq!(token, expected_token);
            }
        };
    }

    #[test]
    fn operators_delimiters() {
        let input = "
            =+(){},;
            !-/*5;
            5 < 10 > 5;
        ";

        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LeftParenthesis,
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
            Token::EOF,
        ];

        assert_lexer_eq!(input, expected);
    }

    #[test]
    fn identifiers_keywords_integers_strings() {
        let input = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            \"foobar\"
            \"foo bar\"

            [1, 2];
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
            Token::String(String::from("foobar")),
            Token::String(String::from("foo bar")),
            Token::LeftBracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::RightBracket,
            Token::Semicolon,
        ];

        assert_lexer_eq!(input, expected);
    }

    #[test]
    fn if_else() {
        let input = "
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
        ";

        let expected = vec![
            Token::If,
            Token::LeftParenthesis,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::RightParenthesis,
            Token::LeftBrace,
            Token::Return,
            Token::Boolean(true),
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::Boolean(false),
            Token::Semicolon,
            Token::RightBrace,
        ];

        assert_lexer_eq!(input, expected);
    }

    #[test]
    fn comparisons() {
        let input = "
            10 == 10;
            10 != 9;
        ";

        let expected = vec![
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
        ];

        assert_lexer_eq!(input, expected);
    }

    #[test]
    fn hash() {
        let input = "{ \"foo\": \"bar\" }";

        let expected = vec![
            Token::LeftBrace,
            Token::String(String::from("foo")),
            Token::Colon,
            Token::String(String::from("bar")),
            Token::RightBrace,
        ];

        assert_lexer_eq!(input, expected);
    }
}
