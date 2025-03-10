use thiserror::Error;

use crate::lexer::Token;

#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub struct LexerError {
    start_position: usize,
    end_position: usize,
    string_rep: String,
}

impl LexerError {
    pub(crate) fn new(start_position: usize, end_position: usize, chars: &[char]) -> Self {
        let mut string_rep = String::with_capacity(chars.len());
        chars.iter().for_each(|f| string_rep.push(*f));
        Self {
            start_position,
            end_position,
            string_rep,
        }
    }

    pub(crate) fn new_s(
        start_position: usize,
        end_position: usize,
        cursor: &crate::peaker::Cursor<'_, char>,
    ) -> Self {
        LexerError::new(
            start_position,
            end_position,
            cursor
                .slice_x_y(start_position..end_position)
                .unwrap_or_else(|| panic!("Should always have this rage start_position: `{}`, end_position: `{}`, cursor_idx: `{}`, cursor_len: `{}`",
                    start_position,
                    end_position,
                    cursor.idx(),
                    cursor.len())),
        )
    }

    pub(crate) fn new_string(
        start_position: usize,
        end_position: usize,
        string_rep: String,
    ) -> Self {
        Self {
            start_position,
            end_position,
            string_rep,
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LexerError {{ start_position: {},\nend_position: {},\nstring_rep: {}}}",
            self.start_position, self.end_position, self.string_rep
        )
    }
}

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum ParserError {
    ExpectedExpression(String),
    InvalidExpression(Token),
    InvalidIdentifier(Token),
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
            }
        )
    }
}
