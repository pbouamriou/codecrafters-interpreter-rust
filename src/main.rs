use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};

enum TokenType {
    LeftParent,
    RightParent,
    LeftBrace,
    RightBrace,
    EOF
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::LeftParent => write!(f, "LEFT_PAREN"),
            TokenType::RightParent => write!(f, "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

struct Token {
    token_type: TokenType,
    lexem: String,
}

impl Token {
    pub fn new_from_lexem(lexem: char) -> Option<Self>  {
        match lexem {
            ')' => Some(Token{ token_type: TokenType::RightParent, lexem: lexem.to_string()}),
            '(' => Some(Token{ token_type: TokenType::LeftParent, lexem: lexem.to_string()}),
            '}' => Some(Token{ token_type: TokenType::RightBrace, lexem: lexem.to_string()}),
            '{' => Some(Token{ token_type: TokenType::LeftBrace, lexem: lexem.to_string()}),
            _ => None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} null", self.token_type, self.lexem)
    }
}

fn tokenize(contents: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    for character in contents.chars() {
        if let Some(x) = Token::new_from_lexem(character) {
            tokens.push(x);
        }
    }
    tokens.push(Token{ token_type: TokenType::EOF, lexem: "".to_string()});
    tokens
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let tokens = tokenize(&file_contents);
            for token in tokens.iter() {
                println!("{}", token)
            }
            return;
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
