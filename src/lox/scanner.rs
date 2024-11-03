use super::private::{
    Environment, EvaluationError, MatchResult, Position, Scanner, Token, TokenType,
};
use super::EvaluationResult;

use core::{fmt, str};
use std::rc::Rc;

pub enum ScannerError {
    UnexpectedCaracter,
    UnterminatedString,
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
            TokenType::Eof => write!(f, "EOF"),
        }
    }
}

impl Token {
    fn new(token_type: TokenType, lexem: String, position: Position) -> Self {
        let is_filtered = token_type == TokenType::Comment
            || token_type == TokenType::Space
            || token_type == TokenType::Tab
            || token_type == TokenType::NewLine;
        let mut evaluation = "".to_string();
        match token_type {
            TokenType::String => {
                let x: Vec<&str> = lexem.split('"').collect();
                evaluation = x.get(1).unwrap().to_string();
            }
            TokenType::Number => {
                if let Ok(value) = lexem.parse::<u64>() {
                    evaluation = format!("{:.1}", value as f64);
                } else {
                    let value: f64 = lexem.parse().unwrap();
                    if value == ((value as u64) as f64) {
                        evaluation = format!("{:.1}", value);
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
            position,
        }
    }

    pub fn parse(&self) -> &String {
        if !self.evaluation.is_empty() {
            &self.evaluation
        } else {
            &self.lexem
        }
    }

    pub fn evaluate_str(&self) -> &str {
        (if !self.evaluation.is_empty() {
            &self.evaluation
        } else {
            "null"
        }) as _
    }

    pub fn raw_evaluate(&self) -> &str {
        &self.evaluation
    }

    pub fn evaluate(&self, environment: &mut Environment) -> EvaluationResult {
        match self.token_type {
            TokenType::False => EvaluationResult::Boolean(false),
            TokenType::True => EvaluationResult::Boolean(true),
            TokenType::Nil => EvaluationResult::Nil,
            TokenType::Number => {
                EvaluationResult::Number(self.raw_evaluate().parse::<f64>().unwrap())
            }
            TokenType::String => EvaluationResult::Str(self.raw_evaluate().to_string()),
            TokenType::Identifier => {
                if let Some(result) = environment.value(self.get_lexem()) {
                    result.clone()
                } else {
                    EvaluationResult::Error(EvaluationError {
                        token: Rc::new(self.clone()),
                        message: "Undefined variable".to_string(),
                    })
                }
            }
            _ => EvaluationResult::Error(EvaluationError {
                token: Rc::new(self.clone()),
                message: "Can't evaluate token".to_string(),
            }),
        }
    }

    fn scan_string(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError> {
        let mut content = Vec::<u8>::new();
        content.push(lexem as u8);
        while scanner.match_not_char('"').is_some_and(|x| match x {
            MatchResult::Match(car) => {
                content.push(car as u8);
                true
            }
            MatchResult::NotMatch(car) => {
                content.push(car as u8);
                false
            }
        }) {}
        if scanner.get_char().is_some_and(|x| x == '"') {
            Ok(Token::new(
                TokenType::String,
                String::from_utf8(content).unwrap(),
                scanner.get_position(),
            ))
        } else {
            Err(ScannerError::UnterminatedString)
        }
    }

    fn scan_identifier(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError> {
        let mut content = String::new();
        content.push(lexem);
        while scanner
            .match_with_fn(|car| matches!(car, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
            .is_some_and(|x| match x {
                MatchResult::Match(car) => {
                    content.push(car);
                    true
                }
                MatchResult::NotMatch(_) => false,
            })
        {}
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
            _ => Ok(Token::new(TokenType::Identifier, content, position)),
        }
    }

    fn scan_number(lexem: char, scanner: &mut impl Scanner) -> Result<Self, ScannerError> {
        let mut content = String::new();
        let mut has_dot = false;
        content.push(lexem);
        while scanner
            .match_with_fn(|car| match car {
                '0'..='9' => true,
                '.' => {
                    if !has_dot {
                        has_dot = true;
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            })
            .is_some_and(|x| match x {
                MatchResult::Match(car) => {
                    content.push(car);
                    true
                }
                MatchResult::NotMatch(_) => false,
            })
        {}
        Ok(Token::new(
            TokenType::Number,
            content,
            scanner.get_position(),
        ))
    }

    pub fn new_from_scanner(scanner: &mut impl Scanner) -> Result<Self, ScannerError> {
        if let Some(lexem) = scanner.get_char() {
            let position = scanner.get_position();
            match lexem {
                ')' => Ok(Token::new(
                    TokenType::RightParent,
                    lexem.to_string(),
                    position,
                )),
                '(' => Ok(Token::new(
                    TokenType::LeftParent,
                    lexem.to_string(),
                    position,
                )),
                '}' => Ok(Token::new(
                    TokenType::RightBrace,
                    lexem.to_string(),
                    position,
                )),
                '{' => Ok(Token::new(
                    TokenType::LeftBrace,
                    lexem.to_string(),
                    position,
                )),
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
                        }) {}
                        Ok(Token::new(TokenType::Comment, "".to_string(), position))
                    } else {
                        Ok(Token::new(TokenType::Slash, lexem.to_string(), position))
                    }
                }
                ';' => Ok(Token::new(
                    TokenType::SemiColon,
                    lexem.to_string(),
                    position,
                )),
                '=' => {
                    if scanner.match_char('=').is_some_and(|x| x) {
                        Ok(Token::new(
                            TokenType::EqualEqual,
                            "==".to_string(),
                            position,
                        ))
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
                        Ok(Token::new(
                            TokenType::GreaterEqual,
                            ">=".to_string(),
                            position,
                        ))
                    } else {
                        Ok(Token::new(TokenType::Greater, lexem.to_string(), position))
                    }
                }
                '\t' => Ok(Token::new(TokenType::Tab, lexem.to_string(), position)),
                ' ' => Ok(Token::new(TokenType::Space, lexem.to_string(), position)),
                '\n' | '\r' => Ok(Token::new(TokenType::NewLine, lexem.to_string(), position)),
                _ => Err(ScannerError::UnexpectedCaracter),
            }
        } else {
            Ok(Token::new(
                TokenType::Eof,
                "".to_string(),
                scanner.get_position(),
            ))
        }
    }

    pub fn get_position(&self) -> &Position {
        &self.position
    }

    pub fn get_lexem(&self) -> &String {
        &self.lexem
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token_type,
            self.lexem,
            self.evaluate_str()
        )
    }
}

pub struct LoxScanner<'a> {
    current: usize,
    position: Position,
    source: &'a str,
}

impl<'a> LoxScanner<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self {
            current: 0,
            source: contents,
            position: Position {
                line_number: 1,
                position: 1,
            },
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Rc<Token>>, Vec<Rc<Token>>> {
        let mut tokens: Vec<Rc<Token>> = Vec::new();
        let mut error = false;
        loop {
            if let Some(character) = self.peek() {
                match Token::new_from_scanner(self) {
                    Ok(x) => {
                        if !x.is_filtered {
                            tokens.push(Rc::new(x));
                        }
                    }
                    Err(err) => match err {
                        ScannerError::UnexpectedCaracter => {
                            if character != '\n' && character != '\r' {
                                eprintln!(
                                    "[line {}] Error: Unexpected character: {}",
                                    self.get_position().line_number,
                                    character
                                );
                                error = true
                            }
                        }
                        ScannerError::UnterminatedString => {
                            eprintln!(
                                "[line {}] Error: Unterminated string.",
                                self.get_position().line_number
                            );
                            error = true
                        }
                    },
                }
            } else {
                tokens.push(Rc::new(Token::new(
                    TokenType::Eof,
                    "".to_string(),
                    self.get_position(),
                )));
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
        item.map(|x| (*x) as char)
    }
}

impl<'a> Scanner for LoxScanner<'a> {
    fn get_char(&mut self) -> Option<char> {
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

    fn get_position(&self) -> Position {
        self.position.clone()
    }

    fn match_char(&mut self, character: char) -> Option<bool> {
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

    fn match_not_char(&mut self, character: char) -> Option<MatchResult> {
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

    fn match_with_fn(&mut self, mut match_fn: impl FnMut(char) -> bool) -> Option<MatchResult> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unexpected_character_error() {
        let result = LoxScanner::new("^").tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_unterminated_string_error() {
        let result = LoxScanner::new("\"unterminated").tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn test_token_types() {
        let source = "( ) { } , . - + * / ; = == ! != < <= > >=";
        let result = LoxScanner::new(source).tokenize();
        assert!(result.is_ok());
        let tokens = result.unwrap();
        let token_types: Vec<TokenType> = tokens.iter().map(|token| token.token_type).collect();
        assert_eq!(
            token_types,
            vec![
                TokenType::LeftParent,
                TokenType::RightParent,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Minus,
                TokenType::Plus,
                TokenType::Star,
                TokenType::Slash,
                TokenType::SemiColon,
                TokenType::Equal,
                TokenType::EqualEqual,
                TokenType::Bang,
                TokenType::BangEqual,
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Eof
            ]
        );
    }
}
