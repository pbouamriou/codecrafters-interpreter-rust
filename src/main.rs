use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::Termination;
use std::process::ExitCode;

#[derive(PartialEq, Clone, Copy)]
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
    EqualEqual,
    Equal,
    Bang,
    BangEqual,
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
            TokenType::Equal => write!(f, "EQUAL"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType::Bang => write!(f, "BANG"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

struct Token {
    token_type: TokenType,
    lexem: String,
}
pub trait Scanner {
    fn match_char(& mut self, character: char) -> bool;
    fn get_char(& mut self) -> Option<char>;
}

impl Token {
    pub fn new_from_scanner(scanner: &mut impl Scanner) -> Option<Self>  {
        if let Some(lexem) =  scanner.get_char() {
            writeln!(io::stderr(), "character {}", lexem).unwrap();
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
                '=' => {
                    if scanner.match_char('=') {
                        Some(Token{ token_type: TokenType::EqualEqual, lexem: "==".to_string()})
                    } else {
                        Some(Token{ token_type: TokenType::Equal, lexem: lexem.to_string()})
                    }
                }
                '!' => {
                    if scanner.match_char('=') {
                        Some(Token{ token_type: TokenType::BangEqual, lexem: "!=".to_string()})
                    } else {
                        Some(Token{ token_type: TokenType::Bang, lexem: lexem.to_string()})
                    }
                }
                _ => None
            }
        } else {
            Some(Token{ token_type: TokenType::EOF, lexem: "".to_string()})
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} null", self.token_type, self.lexem)
    }
}


struct LoxScanner<'a> {
    current: usize,
    source: &'a str
}

impl<'a> LoxScanner<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self { current: 0, source: contents }
    }

    pub fn tokenize(& mut self) -> Result<Vec<Token>, Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut line_number = 1;
        let mut error = false;
        loop {
            if let Some(character) = self.peek() {
                if character == '\n' {
                    line_number += 1
                }
                if let Some(x) = Token::new_from_scanner(self) {
                    tokens.push(x);
                }
                else if character != '\n' && character != '\r' {
                    writeln!(io::stderr(), "[line {}] Error: Unexpected character: {}", line_number, character).unwrap();
                    error = true
                }
            } else {
                tokens.push(Token{ token_type: TokenType::EOF, lexem: "".to_string() });
                break;
            }
        }
        if !error {
            Ok(tokens)
        } else {
            Err(tokens)
        }
    }

    fn peek(&self) -> Option<char> {
        if self.current >= self.source.len() {
            return None;
        }
        let item = self.source.as_bytes().get(self.current);
        match item {
            Some(x) => Some((*x) as char),
            None => None
        }
    }
}

impl<'a> Scanner for LoxScanner<'a> {
    fn get_char(& mut self) -> Option<char> {
        let x = self.peek();
        if let Some(_) = x {
            self.current += 1;
        }
        x
    }

    fn match_char(& mut self, character: char) -> bool {
        if let Some(x) = self.peek() {
            if x == character {
                self.current += 1;
                return true;
            }
        }
        false
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

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
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
