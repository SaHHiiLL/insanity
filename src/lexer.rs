/*
*
* project(<NAME>, <VERSION>, [DESCRIPTION])
*
* define_library(<LIB_NAME>, path=)
*
*/

use super::peaker::Peaker;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Identifier(String),
    Number(i64),
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Comma,
    StringLiteral(String),
    Equals,
    NotEquals,
    InValid,
}

struct Lexer<'a> {
    input: Peaker<'a, char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a [char]) -> Self {
        Self {
            input: Peaker::new(input),
        }
    }

    fn skip_spaces(&mut self) {
        loop {
            if let Some(x) = self.input.get() {
                if x.is_whitespace() {
                    self.input.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(x) = self.input.get() {
            if x.is_alphanumeric() {
                buffer.push(*x);
                self.input.next();
            } else {
                break;
            }
        }
        Some(buffer)
    }

    fn read_number(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(x) = self.input.get() {
            if x.is_ascii_digit() {
                buffer.push(*x);
                self.input.next();
            } else {
                break;
            }
        }
        Some(buffer)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_spaces();
        let res = match self.input.get()? {
            'a'..='z' | 'A'..='Z' => {
                let ident = self.read_identifier()?;
                // Return out the function, as `read_identifier` already moves the cursor to where it needs to be
                return Some(Token::Identifier(ident));
            }
            '0'..='9' => {
                let ident = self.read_number()?;
                // Return out the function, as `read_number` already moves the cursor to where it needs to be
                return Some(Token::Identifier(ident));
            }
            '"' => {
                todo!()
            }
            ')' => Some(Token::RightParen),
            '(' => Some(Token::LeftParen),
            '}' => Some(Token::RightBrace),
            '{' => Some(Token::LeftBrace),
            '=' => Some(Token::Equals),
            '!' => {
                if *self.input.peak()? == '=' {
                    self.input.next();
                    Some(Token::NotEquals)
                } else {
                    Some(Token::InValid)
                }
            }
            ',' => Some(Token::Comma),
            '\0' => None,
            _ => Some(Token::InValid),
        };
        self.input.next();
        res
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "(){()}=!=".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::LeftParen,
            Token::RightParen,
            Token::RightBrace,
            Token::Equals,
            Token::NotEquals,
        ];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }

    #[test]
    fn test_ident() {
        let input = "heloo () worlkd".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            Token::Identifier("heloo".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Identifier("worlkd".to_string()),
        ];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens, expected);
    }
}