/*
*
* project(<NAME>, <VERSION>, [DESCRIPTION])
*
* define_library(<LIB_NAME>, path=)
*
* let sj = <Variable>
*
*/

use super::peaker::{Cursor, MoveBackIterator};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Number(i64),
    StringLiteral(String),

    // Keywords
    Let,
    Return,
    Process,
    Function,

    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Comma,
    SemiColon,
    Assign,
    NotEquals,
    Minus,
    Add,
    Divide,
    Multiply,

    LessThen,
    GreaterThen,

    LessOrEqualThen,
    GreaterEqualThen,

    InValid,
}

pub struct Lexer<'a> {
    input: Cursor<'a, char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [char]) -> Self {
        Self {
            input: Cursor::new(input),
        }
    }

    fn skip_spaces(&mut self) {
        while let Some(x) = self.input.get() {
            if x.is_whitespace() {
                self.input.next();
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
        // On success move the cursor back
        self.input.next_prev().expect("Should Never fail");
        Some(buffer)
    }

    fn read_number(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while let Some(x) = self.input.get() {
            if x.is_numeric() {
                buffer.push(*x);
                self.input.next();
            } else {
                break;
            }
        }
        // On success move the cursor back
        self.input.next_prev().expect("Should Never fail");
        Some(buffer)
    }

    fn read_string(&mut self) -> Option<String> {
        let mut buffer = String::new();
        self.input.next();

        while let Some(x) = self.input.get() {
            if *x != '"' {
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
                match ident.as_str() {
                    "return" => Some(Token::Return),
                    "let" => Some(Token::Let),
                    "proc" => Some(Token::Process),
                    "fn" => Some(Token::Function),
                    _ => Some(Token::Identifier(ident)),
                }
                // Return out the function, as `read_identifier` already moves the cursor to where it needs to be
            }
            '0'..='9' => {
                let ident = self.read_number()?.parse().unwrap();
                // Return out the function, as `read_number` already moves the cursor to where it needs to be
                Some(Token::Number(ident))
            }
            ';' => Some(Token::SemiColon),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Add),
            '/' => Some(Token::Divide),
            '*' => Some(Token::Multiply),
            ')' => Some(Token::RightParen),
            '(' => Some(Token::LeftParen),
            '}' => Some(Token::RightBrace),
            '{' => Some(Token::LeftBrace),
            '=' => Some(Token::Assign),
            '<' => {
                if let Some(peak) = self.input.peak() {
                    if *peak == '=' {
                        Some(Token::GreaterEqualThen)
                    } else {
                        Some(Token::InValid)
                    }
                } else {
                    Some(Token::GreaterThen)
                }
            }
            '>' => {
                if let Some(peak) = self.input.peak() {
                    if *peak == '=' {
                        Some(Token::LessOrEqualThen)
                    } else {
                        Some(Token::InValid)
                    }
                } else {
                    Some(Token::LessThen)
                }
            }
            '!' => {
                if let Some(peak) = self.input.peak() {
                    if *peak == '=' {
                        self.input.next();
                        Some(Token::NotEquals)
                    } else {
                        Some(Token::InValid)
                    }
                } else {
                    Some(Token::InValid)
                }
            }
            '"' => {
                let string = self.read_string()?;
                Some(Token::StringLiteral(string))
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
    use super::Token::*;
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
            Token::Assign,
            Token::NotEquals,
        ];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }

    #[test]
    fn test_string() {
        let input = "\"Hello World\"".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[Token::StringLiteral("Hello World".to_string())];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }

    #[test]
    fn test_ident() {
        let input = "Hey world!= == ()".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            Token::Identifier("Hey".to_string()),
            Token::Identifier("world".to_string()),
            Token::NotEquals,
            Token::Assign,
            Token::Assign,
            Token::LeftParen,
            Token::RightParen,
        ];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }

    #[test]
    fn test_function() {
        let input = r##"
            fn foo() {
                let bar = "Hello World!";
                return -1337;
            }
        "##;

        let expected = vec![
            Function,
            Identifier("foo".to_string()),
            LeftParen,
            RightParen,
            LeftBrace,
            Let,
            Identifier("bar".to_string()),
            Assign,
            StringLiteral("Hello World!".to_string()),
            SemiColon,
            Return,
            Minus,
            Number(1337),
            SemiColon,
            RightBrace,
        ];
        let chars = &input.chars().collect::<Vec<char>>();
        let lexer = Lexer::new(chars);
        let lexer_token = lexer.collect::<Vec<Token>>();
        assert_eq!(lexer_token.len(), expected.len());
        assert_eq!(lexer_token, expected);
    }
}
