use thiserror::Error;

use crate::lexer::{Token, TokenType};

#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub struct LexerError {
    start_position: usize,
    line_number: usize,
    string_rep: String,
}

#[cfg(test)]
impl LexerError {
    pub(crate) fn test<T: ToString>(start_position: usize, line_nbr: usize, chars: T) -> Self {
        Self {
            start_position,
            line_number: line_nbr,
            string_rep: chars.to_string(),
        }
    }
}

impl LexerError {
    pub(crate) fn new(start_position: usize, line_nbr: usize, chars: &[char]) -> Self {
        let mut string_rep = String::with_capacity(chars.len());
        chars.iter().for_each(|f| string_rep.push(*f));
        Self {
            start_position,
            line_number: line_nbr,
            string_rep,
        }
    }

    pub(crate) fn empty() -> Self {
        Self {
            start_position: 1,
            line_number: 1,
            string_rep: String::new(),
        }
    }

    pub(crate) fn new_string(start_position: usize, line_nbr: usize, string_rep: String) -> Self {
        Self {
            start_position,
            line_number: line_nbr,
            string_rep,
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LexerError {{ start_position: {},\nstring_rep: {}}}",
            self.start_position, self.string_rep
        )
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ParserError {
    ExpectedExpression(&'static str),
    InvalidExpression(Token),
    InvalidIdentifier(Token),
    ExpectedTokenGotNone(TokenType),
    ExpectedToken(Token, TokenType),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ParserError::ExpectedExpression(got) => {
                    format!("ExpectedExpression {{ Got({}) }}", got)
                }
                Self::InvalidExpression(token) => {
                    format!("InvalidExpression {{ {} }}", token)
                }
                Self::InvalidIdentifier(token) => {
                    format!("InvalidIdentifier {{ {} }}", token)
                }
                Self::ExpectedTokenGotNone(token_type) => {
                    format!("ExpectedTokenGotNone {{ {} }}", token_type)
                }
                Self::ExpectedToken(token, token_type) => {
                    format!("ExpectedToken {{ {}, {} }}", token, token_type)
                }
            }
        )
    }
}
