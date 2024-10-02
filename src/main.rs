use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::Termination;
use std::process::ExitCode;

enum TokenType {
    LeftParent,
    RightParent,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Divide,
    SemiColon,
    EOF
}

enum ApplicationErrorCode {
    UnexpectedCaracter = 65,
    WrongNumberOfParameters = 1,
    UnknownCommand = 2,
}
enum AppExitCode {
    Ok,
    Err(ApplicationErrorCode),
}


impl Termination for AppExitCode {
    fn report(self) -> ExitCode {
        match self {
            AppExitCode::Ok => ExitCode::SUCCESS,
            AppExitCode::Err(v) => ExitCode::from(v as u8),
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::LeftParent => write!(f, "LEFT_PAREN"),
            TokenType::RightParent => write!(f, "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType::Comma => write!(f, "COMMA"),
            TokenType::Dot => write!(f, "DOT"),
            TokenType::Minus => write!(f, "MINUS"),
            TokenType::Plus => write!(f, "PLUS"),
            TokenType::Star => write!(f, "STAR"),
            TokenType::Divide => write!(f, "DIV"),
            TokenType::SemiColon => write!(f, "SEMICOLON"),
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
            ',' => Some(Token{ token_type: TokenType::Comma, lexem: lexem.to_string()}),
            '.' => Some(Token{ token_type: TokenType::Dot, lexem: lexem.to_string()}),
            '-' => Some(Token{ token_type: TokenType::Minus, lexem: lexem.to_string()}),
            '+' => Some(Token{ token_type: TokenType::Plus, lexem: lexem.to_string()}),
            '*' => Some(Token{ token_type: TokenType::Star, lexem: lexem.to_string()}),
            '/' => Some(Token{ token_type: TokenType::Divide, lexem: lexem.to_string()}),
            ';' => Some(Token{ token_type: TokenType::SemiColon, lexem: lexem.to_string()}),
            _ => None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} null", self.token_type, self.lexem)
    }
}

fn tokenize(contents: &str) -> Result<Vec<Token>, Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut line_number = 1;
    let mut error = false;
    for character in contents.chars() {
        if character == '\n' {
            line_number += 1
        }
        if let Some(x) = Token::new_from_lexem(character) {
            tokens.push(x);
        }
        else if character != '\n' && character != '\r' {
            writeln!(io::stderr(), "[line {}] Error: Unexpected character: {}", line_number, character).unwrap();
            error = true
        }
    }
    tokens.push(Token{ token_type: TokenType::EOF, lexem: "".to_string()});
    if !error {
        Ok(tokens)
    } else {
        Err(tokens)
    }
}

fn main() -> AppExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return AppExitCode::Err(ApplicationErrorCode::WrongNumberOfParameters)
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

            match tokenize(&file_contents) {
                Ok(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    return AppExitCode::Ok;
                },
                Err(tokens) => {
                    for token in tokens.iter() {
                        println!("{}", token)
                    }
                    return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter);
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return AppExitCode::Err(ApplicationErrorCode::UnknownCommand);
        }
    }
}
