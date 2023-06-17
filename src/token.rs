use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    // Illegal and end of file
    Illegal,
    Empty,
    EOF,

    // Identifiers and literals
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),

    // Statements
    Assign,
    If,
    Else,

    // Operators
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    GreaterThan,
    LessThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Keywords
    Function,
    Let,
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
            Token::Boolean(value) => format!("{}", value),
            Token::If => "if".into(),
            Token::Else => "else".into(),
            Token::Return => "return".into(),
            Token::Empty => "".into(),
            Token::LessThanOrEqual => "<=".into(),
            Token::GreaterThanOrEqual => ">=".into(),
            Token::String(string) => format!("{}", string),
            Token::LeftBracket => "[".into(),
            Token::RightBracket => "]".into(),
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
            Token::Boolean(value) => write!(f, "{}", value),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
            Token::Empty => write!(f, "Empty"),
            Token::LessThanOrEqual => write!(f, "LessThanOrEqual"),
            Token::GreaterThanOrEqual => write!(f, "GreaterThanOrEqual"),
            Token::String(string) => write!(f, "{}", string),
            Token::LeftBracket => write!(f, "LeftBracket"),
            Token::RightBracket => write!(f, "RightBracket"),
        }
    }
}
