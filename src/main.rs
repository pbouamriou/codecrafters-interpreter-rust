use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::Termination;
use std::process::ExitCode;
use std::rc::Rc;

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
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Function,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
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
            TokenType::Identifier => write!(f, "IDENTIFIER"),
            TokenType::And => write!(f, "AND"),
            TokenType::Class => write!(f, "CLASS"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::For => write!(f, "FOR"),
            TokenType::Function => write!(f, "FUN"),
            TokenType::If => write!(f, "IF"),
            TokenType::Nil => write!(f, "NIL"),
            TokenType::Or => write!(f, "OR"),
            TokenType::Print => write!(f, "PRINT"),
            TokenType::Return => write!(f, "RETURN"),
            TokenType::Super => write!(f, "SUPER"),
            TokenType::This => write!(f, "THIS"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::Var => write!(f, "VAR"),
            TokenType::While => write!(f, "WHILE"),
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
    is_filtered: bool,
    position: Position,
}


pub trait Scanner {
    fn match_char(& mut self, character: char) -> Option<bool>;
    fn match_not_char(& mut self, character: char) -> Option<MatchResult>;
    fn match_with_fn(& mut self, match_fn: impl FnMut(char) -> bool) -> Option<MatchResult>;
    fn get_char(& mut self) -> Option<char>;
    fn get_position(&self) -> Position;
}

impl Token {

    pub fn new(token_type: TokenType, lexem: String, position: Position ) -> Self {
        let is_filtered =  token_type == TokenType::Comment || token_type == TokenType::Space || token_type == TokenType::Tab || token_type == TokenType::NewLine;
        let mut evaluation = "".to_string();
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
            is_filtered,
            position
        }
    }

    pub fn parse<'a>(&'a self) -> &'a String {
        if self.evaluation.len() > 0 {
            &self.evaluation
        } else {
            &self.lexem
        }
    }
    
    pub fn evaluate<'a>(&'a self) -> &'a str {
        if self.evaluation.len() > 0 {
            &self.evaluation
        } else {
            &"null"
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
            Ok(Token::new(TokenType::String, content, scanner.get_position()))
        } else {
            Err(ScannerError::UnterminatedString)
        }
    }
    
    fn scan_identifier(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        let mut content = String::new();
        content.push(lexem);
        while scanner.match_with_fn(|car| {
            match car {
               'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
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
        let position = scanner.get_position();
        match content.as_str() {
            "and" => Ok(Token::new(TokenType::And, content, position)),
            "class" => Ok(Token::new(TokenType::Class, content, position)),
            "else" => Ok(Token::new(TokenType::Else, content, position)),
            "false" => Ok(Token::new(TokenType::False, content, position)),
            "for" => Ok(Token::new(TokenType::For, content, position)),
            "fun" => Ok(Token::new(TokenType::Function, content, position)),
            "if" => Ok(Token::new(TokenType::If, content, position)),
            "nil" => Ok(Token::new(TokenType::Nil, content, position)),
            "or" => Ok(Token::new(TokenType::Or, content, position)),
            "print" => Ok(Token::new(TokenType::Print, content, position)),
            "return" => Ok(Token::new(TokenType::Return, content, position)),
            "super" => Ok(Token::new(TokenType::Super, content, position)),
            "this" => Ok(Token::new(TokenType::This, content, position)),
            "true" => Ok(Token::new(TokenType::True, content, position)),
            "var" => Ok(Token::new(TokenType::Var, content, position)),
            "while" => Ok(Token::new(TokenType::While, content, position)),
            _ => Ok(Token::new(TokenType::Identifier, content, position))
        }
    }

    fn scan_number(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        let mut content = String::new();
        let mut has_dot = false;
        content.push(lexem);
        while scanner.match_with_fn(|car| {
            match car {
               '0'..='9' => true,
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
        Ok(Token::new(TokenType::Number, content, scanner.get_position()))
    }

    pub fn new_from_scanner(scanner: &mut impl Scanner) -> Result<Self, ScannerError>  {
        if let Some(lexem) =  scanner.get_char() {
            let position = scanner.get_position();
            match lexem {
                ')' => Ok(Token::new(TokenType::RightParent, lexem.to_string(), position)),
                '(' => Ok(Token::new(TokenType::LeftParent,lexem.to_string(), position)),
                '}' => Ok(Token::new(TokenType::RightBrace, lexem.to_string(), position)),
                '{' => Ok(Token::new(TokenType::LeftBrace, lexem.to_string(), position)),
                ',' => Ok(Token::new(TokenType::Comma, lexem.to_string(), position)),
                '.' => Ok(Token::new(TokenType::Dot, lexem.to_string(), position)),
                '-' => Ok(Token::new(TokenType::Minus, lexem.to_string(), position)),
                '+' => Ok(Token::new(TokenType::Plus, lexem.to_string(), position)),
                '*' => Ok(Token::new(TokenType::Star, lexem.to_string(), position)),
                '"' => Self::scan_string(lexem, scanner),
                'a'..='z' | 'A'..='Z' | '_' => Self::scan_identifier(lexem, scanner),
                '0'..='9' => Self::scan_number(lexem, scanner),
                '/' => {
                    if scanner.match_char('/').is_some_and(|x| x) {
                        while scanner.match_not_char('\n').is_some_and(|x| match x {
                            MatchResult::Match(_) => true,
                            MatchResult::NotMatch(_) => false,
                        }) {
                        }
                        Ok(Token::new(TokenType::Comment, "".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Slash, lexem.to_string(), position))
                    }
                },
                ';' => Ok(Token::new(TokenType::SemiColon, lexem.to_string(), position)),
                '=' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::EqualEqual, "==".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Equal, lexem.to_string(), position))
                    }
                }
                '!' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::BangEqual, "!=".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Bang, lexem.to_string(), position))
                    }
                }
                '<' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::LessEqual, "<=".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Less, lexem.to_string(), position))
                    }
                }
                '>' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(TokenType::GreaterEqual, ">=".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Greater, lexem.to_string(), position))
                    }
                }
                '\t' => Ok(Token::new(TokenType::Tab, lexem.to_string(), position)),
                ' ' => Ok(Token::new(TokenType::Space, lexem.to_string(), position)),
                '\n' | '\r' => Ok(Token::new(TokenType::NewLine, lexem.to_string(), position)),
                _ => Err(ScannerError::UnexpectedCaracter)
            }
        } else {
            Ok(Token::new(TokenType::EOF, "".to_string(), scanner.get_position()))
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.token_type, self.lexem, self.evaluate())
    }
}

#[derive(Clone)]
pub struct Position {
    line_number: u32,
    position : u32,
}


struct LoxScanner<'a> {
    current: usize,
    position: Position,
    source: &'a str
}

impl<'a> LoxScanner<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self { current: 0, source: contents, position: Position{line_number: 1, position: 1} }
    }

    pub fn tokenize(& mut self) -> Result<Vec<Rc<Token>>, Vec<Rc<Token>>> {
        let mut tokens: Vec<Rc<Token>> = Vec::new();
        let mut error = false;
        loop {
            if let Some(character) = self.peek() {
                match Token::new_from_scanner(self) {
                    Ok(x) => {
                        if !x.is_filtered {
                            tokens.push(Rc::new(x));
                        }
                    },
                    Err(err) => {
                        match err {
                            ScannerError::UnexpectedCaracter => {
                                if character != '\n' && character != '\r' {
                                    writeln!(io::stderr(), "[line {}] Error: Unexpected character: {}", self.get_position().line_number, character).unwrap();
                                    error = true
                                }
                            },
                            ScannerError::UnterminatedString => {
                                    writeln!(io::stderr(), "[line {}] Error: Unterminated string.", self.get_position().line_number).unwrap();
                                    error = true
                            }
                        }
                    }
                }
            } else {
                tokens.push(Rc::new(Token::new(TokenType::EOF, "".to_string(), self.get_position() )));
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
        if let Some(character) = x {
            self.current += 1;
            self.position.position += 1;
            if character == '\n' {
                self.position.line_number += 1;
                self.position.position = 1;
            }
        }
        x
    }

    fn get_position(& self) -> Position {
        self.position.clone()
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

pub trait Ast {
    fn print(&self);
}

struct BinaryExpr {
    left: Rc<Expr>,
    operator: Rc<Token>,
    right: Rc<Expr>,
}

struct UnaryExpr {
    operator: Rc<Token>,
    right: Box<Expr>,
}

struct GroupExpr {
    expr: Box<Expr>,
}

enum Expr {
    Primary(Rc<Token>),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Group(GroupExpr),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Primary(token) => {
                write!(f, "{}", *token.parse())
            }
            Expr::Group(expr) => {
                write!(f, "(group {})", expr.expr)
            }
            Expr::Unary(expr) => {
                write!(f, "({} {})", expr.operator.parse(), expr.right)
            }

            Expr::Binary(expr) => {
                write!(f, "({} {} {})", expr.operator.parse(), expr.left, expr.right)
            }
        }
    }
}

struct LoxAst {
    root: Expr
}

impl Ast for LoxAst {
    fn print(&self) {
        println!("{}", self.root)
    }
}

struct ParserError {
    token: Rc<Token>,
    message: String,
}

struct LoxParser {
    current: usize,
    tokens: Vec<Rc<Token>>,
} 

impl LoxParser {
    pub fn new(tokens: Vec<Rc<Token>>) -> Self {
        Self {
            current: 0,
            tokens
        }
    }

    pub fn parse(&mut self) -> Result<impl Ast, ParserError> {
        match self.equality() {
            Ok(ast) => Ok(LoxAst{root: ast}),
            Err(x) => Err(x)
        }
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        match self.condition() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    match x.token_type {
                        TokenType::EqualEqual | TokenType::BangEqual => true,
                        _ => false
                    }
                }) {
                    match self.condition() {
                        Ok(right) => expr = Expr::Binary(BinaryExpr{left: Rc::new(expr), operator: token, right: Rc::new(right)}),
                        Err(err) => return Err(err)
                    }
                }
                return Ok(expr);
            }
            Err(err) => Err(err)
        }
    }

    fn condition(&mut self) -> Result<Expr, ParserError> {
        match self.term() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    match x.token_type {
                        TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => true,
                        _ => false
                    }
                }) {
                    match self.term() {
                        Ok(right) => expr = Expr::Binary(BinaryExpr{left: Rc::new(expr), operator: token, right: Rc::new(right)}),
                        Err(err) => return Err(err)
                    }
                }
                return Ok(expr);
            }
            Err(err) => Err(err)
        }
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        match self.factor() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    match x.token_type {
                        TokenType::Plus | TokenType::Minus => true,
                        _ => false
                    }
                }) {
                    match self.factor() {
                        Ok(right) => expr = Expr::Binary(BinaryExpr{left: Rc::new(expr), operator: token, right: Rc::new(right)}),
                        Err(err) => return Err(err)
                    }
                }
                return Ok(expr);
            }
            Err(err) => Err(err)
        }
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        match self.unary() {
            Ok(expr) => {
                let mut expr = expr;
                while let Some(token) = self.match_cond(|x| {
                    match x.token_type {
                        TokenType::Slash | TokenType::Star => true,
                        _ => false
                    }
                }) {
                    match self.unary() {
                        Ok(right) => expr = Expr::Binary(BinaryExpr{left: Rc::new(expr), operator: token, right: Rc::new(right)}),
                        Err(err) => return Err(err)
                    }
                }
                return Ok(expr);
            }
            Err(err) => Err(err)
        }
    }


    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.match_cond(|x| {
            match x.token_type {
                TokenType::Bang | TokenType::Minus => true,
                _ => false
            }
        }) {
            match self.unary() {
                Ok(expr) => Ok(Expr::Unary(UnaryExpr{operator: token, right: Box::new(expr)})),
                Err(x) => Err(x)
            }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.match_cond(|x| {
            match x.token_type {
                TokenType::False | TokenType::True | TokenType::Nil | TokenType::Number | TokenType::String => true,
                _ => false
            }
        }) {
            Ok(Expr::Primary(token))
        } else {
            self.group()
        }
    }

    fn group(&mut self) -> Result<Expr, ParserError> {
        if let Some(_) = self.match_cond(|x| {
            match x.token_type {
                TokenType::LeftParent => true,
                _ => false
            }
        }) {
            match self.equality() {
                Ok(expr) => {
                    if let Some(_) = self.match_cond(|x| {
                        match x.token_type {
                            TokenType::RightParent => true,
                            _ => false
                        }
                    }) {
                        Ok(Expr::Group(GroupExpr{expr: Box::new(expr)}))
                    } else {
                        Err(ParserError{token: self.peek().unwrap(), message: "Expect expression.".to_string()})
                    }
                }
                Err(x) => Err(x)
            }
        }
        else {
            Err(ParserError{token: self.peek().unwrap(), message: "Expect expression.".to_string()})
        }
    }


    fn match_cond(&mut self, cond: impl Fn(Rc<Token>) -> bool) -> Option<Rc<Token>> {
        if let Some(token) = self.peek() {
            if cond(token.clone()) {
                self.current += 1;
                return Some(token);
            }
        }
        None
    }

    fn peek(&self) -> Option<Rc<Token>> {
        if self.current >= self.tokens.len() {
            return None;
        }
        let item = self.tokens.get(self.current);
        match item {
            Some(x) => Some(x.clone()),
            None => None
        }
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
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = LoxScanner::new(&file_contents);

            match scanner.tokenize() {
                Ok(tokens) => {
                    let mut parser = LoxParser::new(tokens);
                    match parser.parse() {
                        Err(err) => {
                            writeln!(io::stderr(), "[line {}] Error at '{}': {}", err.token.position.line_number, err.token.lexem, err.message).unwrap();
                            return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
                        }
                        Ok(ast) => ast.print(),
                    }
                    return AppExitCode::Ok
                }
                Err(_) => return AppExitCode::Err(ApplicationErrorCode::UnexpectedCaracter)
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return AppExitCode::Err(ApplicationErrorCode::UnknownCommand);
        }
    }
}
