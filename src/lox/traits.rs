use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Position {
    pub line_number: u32,
    pub position: u32,
}

pub enum MatchResult {
    Match(char),
    NotMatch(char),
}

pub trait Scanner {
    fn match_char(&mut self, character: char) -> Option<bool>;
    fn match_not_char(&mut self, character: char) -> Option<MatchResult>;
    fn match_with_fn(&mut self, match_fn: impl FnMut(char) -> bool) -> Option<MatchResult>;
    fn get_char(&mut self) -> Option<char>;
    fn get_position(&self) -> Position;
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenType {
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
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexem: String,
    pub evaluation: String,
    pub is_filtered: bool,
    pub position: Position,
}

#[derive(Clone)]
pub struct EvaluationError {
    pub token: Rc<Token>,
    pub message: String,
}

#[derive(Clone)]
pub enum EvaluationResult {
    Str(String),
    Number(f64),
    Boolean(bool),
    Nil,
    Error(EvaluationError),
}

impl fmt::Display for EvaluationResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvaluationResult::Boolean(x) => write!(f, "{}", x),
            EvaluationResult::Str(x) => write!(f, "{}", x),
            EvaluationResult::Number(x) => write!(f, "{}", x),
            EvaluationResult::Nil => write!(f, "nil"),
            EvaluationResult::Error(err) => write!(
                f,
                "[line {}]: {}",
                err.token.position.line_number, err.message
            ),
        }
    }
}

pub trait AstPrint {
    fn print(&self);
}
pub trait AstEvaluation {
    fn evaluate(&self) -> EvaluationResult;
}

pub struct Ast {
    pub printable: Rc<dyn AstPrint>,
    pub evaluable: Rc<dyn AstEvaluation>,
}
