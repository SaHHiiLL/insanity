use thiserror::Error;

#[derive(Error, Debug, Clone, Eq, PartialEq)]
pub struct InsanityLexerError {
    start_position: usize,
    end_position: usize,
    string_rep: String,
}

impl InsanityLexerError {
    pub fn new(start_position: usize, end_position: usize, chars: &[char]) -> Self {
        let mut string_rep = String::with_capacity(chars.len());
        chars.iter().for_each(|f| string_rep.push(*f));
        Self {
            start_position,
            end_position,
            string_rep,
        }
    }

    pub fn new_string(start_position: usize, end_position: usize, string_rep: String) -> Self {
        Self {
            start_position,
            end_position,
            string_rep,
        }
    }
}

impl std::fmt::Display for InsanityLexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InsanityLexerError {{ start_position: {},\nend_position: {},\nstring_rep: {}}}",
            self.start_position, self.end_position, self.string_rep
        )
    }
}
