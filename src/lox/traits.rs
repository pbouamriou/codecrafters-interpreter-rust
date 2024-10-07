use std::fmt;


#[derive(Clone)]
pub struct Position {
    pub line_number: u32,
    pub position : u32,
}

pub enum MatchResult {
    Match(char),
    NotMatch(char),
}

pub trait Scanner {
    fn match_char(& mut self, character: char) -> Option<bool>;
    fn match_not_char(& mut self, character: char) -> Option<MatchResult>;
    fn match_with_fn(& mut self, match_fn: impl FnMut(char) -> bool) -> Option<MatchResult>;
    fn get_char(& mut self) -> Option<char>;
    fn get_position(&self) -> Position;
}

pub enum EvaluationResult {
    Str(String),
    Number(f64),
    Boolean(bool),
    Nil,
    Error
}

impl fmt::Display for EvaluationResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvaluationResult::Boolean(x) => write!(f, "{}", x),
            EvaluationResult::Str(x) => write!(f, "{}", x),
            EvaluationResult::Number(x) => write!(f, "{}", x),
            EvaluationResult::Nil => write!(f, "nil"),
            EvaluationResult::Error => write!(f, "error"),
        }
    }
}

pub trait Ast {
    fn print(&self);
    fn evaluate(&self) -> EvaluationResult;
}