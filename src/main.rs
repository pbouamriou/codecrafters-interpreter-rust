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
    Slash,
    SemiColon,
    EqualEqual,
    Equal,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Comment,
    Space,
    Tab,
    NewLine,
    String,
    Number,
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
            TokenType::Slash => write!(f, "SLASH"),
            TokenType::SemiColon => write!(f, "SEMICOLON"),
            TokenType::Equal => write!(f, "EQUAL"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType::Bang => write!(f, "BANG"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL"),
            TokenType::Less => write!(f, "LESS"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL"),
            TokenType::Greater => write!(f, "GREATER"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenType::Comment => write!(f, "COMMENT"),
            TokenType::Space => write!(f, "|SPACE|"),
            TokenType::Tab => write!(f, "|TAB|"),
            TokenType::NewLine => write!(f, "|NEWLINE|"),
            TokenType::String => write!(f, "STRING"),
            TokenType::Number => write!(f, "NUMBER"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

pub enum MatchResult {
    Match(char),
    NotMatch(char),
}

pub enum ScannerError {
    UnexpectedCaracter,
    UnterminatedString,
}

struct Token {
    token_type: TokenType,
    lexem: String,
    evaluation: String,
    is_filtered: bool
}
pub trait Scanner {
    fn match_char(& mut self, character: char) -> Option<bool>;
    fn match_not_char(& mut self, character: char) -> Option<MatchResult>;
    fn match_with_fn(& mut self, match_fn: impl FnMut(char) -> bool) -> Option<MatchResult>;
    fn get_char(& mut self) -> Option<char>;
}

impl Token {

    pub fn new(token_type: TokenType, lexem: String ) -> Self {
        let is_filtered =  token_type == TokenType::Comment || token_type == TokenType::Space || token_type == TokenType::Tab || token_type == TokenType::NewLine;
        let mut evaluation = "null".to_string();
        match token_type {
            TokenType::String => {
                let x : Vec<&str> = lexem.split('"').collect();
                evaluation = x.get(1).unwrap().to_string();
            }
            TokenType::Number => {
                if let Ok(value) = lexem.parse::<u64>() {
                    evaluation = format!("{:.1}", value as f64);
                } else {
                    let value : f64 = lexem.parse().unwrap();
                    if value == ((value as u64) as f64) {
                        evaluation = format!("{:.1}", value as f64);
                    } else {
                        evaluation = format!("{}", value);
                    }
                }
            }
            _ => {}
        }
        Self {
            token_type,
            lexem,
            evaluation,
            is_filtered
        }
    }

    fn scan_string(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        let mut content = String::new();
        content.push(lexem);
        while scanner.match_not_char('"').is_some_and(|x| match x {
            MatchResult::Match(car) => {
                content.push(car);
                true
            },
            MatchResult::NotMatch(car) => {
                content.push(car);
                false
            }
        }) {
        }
        if scanner.get_char().is_some_and(|x| x == '"') {
            Ok(Token::new(TokenType::String, content))
        } else {
            Err(ScannerError::UnterminatedString)
        }
    }

    fn scan_number(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        let mut content = String::new();
        let mut has_dot = false;
        content.push(lexem);
        while scanner.match_with_fn(|car| {
            match car {
               '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true,
               '.' => { if !has_dot { 
                    has_dot = true; 
                    true
                } else { 
                    false
                }
            },
               _ => false
            }
        }).is_some_and(|x| match x {
            MatchResult::Match(car) => {
                content.push(car);
                true
            },
            MatchResult::NotMatch(_) => {
                false
            }
        }) {
        }
        Ok(Token::new(TokenType::Number, content))
    }

    pub fn new_from_scanner(scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        if let Some(lexem) =  scanner.get_char() {
            writeln!(io::stderr(), "character {}", lexem).unwrap();
            match lexem {
                ')' => Ok(Token::new(TokenType::RightParent, lexem.to_string())),
                '(' => Ok(Token::new(TokenType::LeftParent,lexem.to_string())),
                '}' => Ok(Token::new(TokenType::RightBrace, lexem.to_string())),
                '{' => Ok(Token::new(TokenType::LeftBrace, lexem.to_string())),
                ',' => Ok(Token::new(TokenType::Comma, lexem.to_string())),
                '.' => Ok(Token::new(TokenType::Dot, lexem.to_string())),
                '-' => Ok(Token::new(TokenType::Minus, lexem.to_string())),
                '+' => Ok(Token::new(TokenType::Plus, lexem.to_string())),
                '*' => Ok(Token::new(TokenType::Star, lexem.to_string())),
                '"' => Self::scan_string(lexem, scanner),
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Self::scan_number(lexem, scanner),
                '/' => {
                    if scanner.match_char('/').is_some_and(|x| x) {
                        while scanner.match_not_char('\n').is_some_and(|x| match x {
                            MatchResult::Match(_) => true,
                            MatchResult::NotMatch(_) => false,
                        }) {
                        }
                        Ok(Token::new(TokenType::Comment, "".to_string()))
                    } else {
                        Ok(Token::new(TokenType::Slash, lexem.to_string()))
                    }
                },
                ';' => Ok(Token::new(TokenType::SemiColon, lexem.to_string())),
                '=' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::EqualEqual, "==".to_string()))
                    } else {
                        Ok(Token::new(TokenType::Equal, lexem.to_string()))
                    }
                }
                '!' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::BangEqual, "!=".to_string()))
                    } else {
                        Ok(Token::new(TokenType::Bang, lexem.to_string()))
                    }
                }
                '<' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::LessEqual, "<=".to_string()))
                    } else {
                        Ok(Token::new(TokenType::Less, lexem.to_string()))
                    }
                }
                '>' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::GreaterEqual, ">=".to_string()))
                    } else {
                        Ok(Token::new(TokenType::Greater, lexem.to_string()))
                    }
                }
                '\t' => Ok(Token::new(TokenType::Tab, lexem.to_string())),
                ' ' => Ok(Token::new(TokenType::Space, lexem.to_string())),
                '\n' | '\r' => Ok(Token::new(TokenType::NewLine, lexem.to_string())),
                _ => Err(ScannerError::UnexpectedCaracter)
            }
        } else {
            Ok(Token::new(TokenType::EOF, "".to_string()))
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.token_type, self.lexem, self.evaluation)
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
                match Token::new_from_scanner(self) {
                    Ok(x) => {
                        if !x.is_filtered {
                            tokens.push(x);
                        }
                    },
                    Err(err) => {
                        match err {
                            ScannerError::UnexpectedCaracter => {
                                if character != '\n' && character != '\r' {
                                    writeln!(io::stderr(), "[line {}] Error: Unexpected character: {}", line_number, character).unwrap();
                                    error = true
                                }
                            },
                            ScannerError::UnterminatedString => {
                                    writeln!(io::stderr(), "[line {}] Error: Unterminated string.", line_number).unwrap();
                                    error = true
                            }
                        }
                    }
                }
            } else {
                tokens.push(Token::new(TokenType::EOF, "".to_string() ));
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

    fn match_char(& mut self, character: char) -> Option<bool> {
        if let Some(x) = self.peek() {
            if x == character {
                self.current += 1;
                return Some(true);
            } else {
                return Some(false);
            }
        }
        None
    }

    fn match_not_char(& mut self, character: char) -> Option<MatchResult> {
        if let Some(x) = self.peek() {
            if x != character {
                self.current += 1;
                return Some(MatchResult::Match(x));
            } else {
                return Some(MatchResult::NotMatch(x));
            }
        }
        None
    }


    fn match_with_fn(& mut self, mut match_fn: impl FnMut(char) -> bool) -> Option<MatchResult> {
        if let Some(x) = self.peek() {
            if match_fn(x) {
                self.current += 1;
                return Some(MatchResult::Match(x));
            } else {
                return Some(MatchResult::NotMatch(x));
            }
        }
        None

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
