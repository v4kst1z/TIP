use crate::position::Position;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct LexicalError {
    pub error: LexicalErrorType,
    pub location: Position,
}

#[derive(Debug, PartialEq)]
pub enum LexicalErrorType {
    IndentationError,
    OtherError(String),
}

impl fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexicalErrorType::IndentationError => {
                write!(f, "unindent does not match any outer indentation level")
            }
            LexicalErrorType::OtherError(msg) => write!(f, "{}", msg),
        }
    }
}
