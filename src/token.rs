use std::fmt::{format, Display};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    // Illegal and end of file
    Illegal,
    EOF,
    // Identifiers and literals
    Identifier(String),
    Integer(i64),

    // Operators
    Plus,
    Assign,
    Minus,
    Bang,
    Asterisk,
    Slash,
    GreaterThan,
    LessThan,
    Equal,
    NotEqual,

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
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn formatted(&self) -> String {
        match self {
            Token::Illegal => "Illegal".into(),
            Token::EOF => "EOF".into(),
            Token::Identifier(identifier) => format!("{}", identifier),
            Token::Integer(integer) => format!("{}", integer),
            Token::Plus => "+".into(),
            Token::Assign => "=".into(),
            Token::Minus => "-".into(),
            Token::Bang => "!".into(),
            Token::Asterisk => "*".into(),
            Token::Slash => "/".into(),
            Token::GreaterThan => ">".into(),
            Token::LessThan => "<".into(),
            Token::Equal => "==".into(),
            Token::NotEqual => "!=".into(),
            Token::Comma => ",".into(),
            Token::Semicolon => ";".into(),
            Token::LeftParenthesis => "(".into(),
            Token::RightParenthesis => ")".into(),
            Token::LeftBrace => "{".into(),
            Token::RightBrace => "}".into(),
            Token::Function => "fn".into(),
            Token::Let => "let".into(),
            Token::True => "true".into(),
            Token::False => "false".into(),
            Token::If => "if".into(),
            Token::Else => "else".into(),
            Token::Return => "return".into(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, "Illegal"),
            Token::EOF => write!(f, "EOF"),
            Token::Identifier(identifier) => write!(f, "{}", identifier),
            Token::Integer(integer) => write!(f, "{}", integer),
            Token::Plus => write!(f, "Plus"),
            Token::Assign => write!(f, "Assign"),
            Token::Minus => write!(f, "Minus"),
            Token::Bang => write!(f, "Bang"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Slash => write!(f, "Slash"),
            Token::GreaterThan => write!(f, "GreaterThan"),
            Token::LessThan => write!(f, "LessThan"),
            Token::Equal => write!(f, "Equal"),
            Token::NotEqual => write!(f, "NotEqual"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::LeftParenthesis => write!(f, "LeftParenthesis"),
            Token::RightParenthesis => write!(f, "RightParenthesis"),
            Token::LeftBrace => write!(f, "LeftBrace"),
            Token::RightBrace => write!(f, "RightBrace"),
            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
        }
    }
}
