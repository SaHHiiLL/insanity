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
    True,
    False,
    ForLoop,
    WhileLoop,
    Break,
    Continue,
    If,
    Else,
    ElseIf,

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

    LessOrEqualThan,
    LessThan,
    GreaterOrEqualThan,
    GreaterThan,

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
        self.input.prev().expect("Should Never fail");
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
        self.input.prev().expect("Should Never fail");
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
                    "for" => Some(Token::ForLoop),
                    "while" => Some(Token::WhileLoop),
                    "let" => Some(Token::Let),
                    "proc" => Some(Token::Process),
                    "fn" => Some(Token::Function),
                    "true" => Some(Token::True),
                    "false" => Some(Token::False),
                    "break" => Some(Token::Break),
                    "continue" => Some(Token::Continue),
                    "if" => Some(Token::If),
                    "else" => Some(Token::Else),
                    "elif" => Some(Token::ElseIf),
                    _ => Some(Token::Identifier(ident)),
                }
            }
            '0'..='9' => {
                let ident = self.read_number()?.parse().unwrap();
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
                        Some(Token::LessOrEqualThan)
                    } else {
                        Some(Token::LessThan)
                    }
                } else {
                    Some(Token::LessThan)
                }
            }
            '>' => {
                if let Some(peak) = self.input.peak() {
                    if *peak == '=' {
                        Some(Token::GreaterOrEqualThan)
                    } else {
                        Some(Token::GreaterThan)
                    }
                } else {
                    Some(Token::GreaterThan)
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

    #[test]
    fn test_let_statement() {
        let input: Vec<char> = "let x = 42;".chars().collect();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::Let));
        assert_eq!(lexer.next(), Some(Token::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(Token::Assign));
        assert_eq!(lexer.next(), Some(Token::Number(42)));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_if_branch() {
        let input: Vec<char> = "if (x < 10) { return x + 1; }".chars().collect();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::If));
        assert_eq!(lexer.next(), Some(Token::LeftParen));
        assert_eq!(lexer.next(), Some(Token::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(Token::LessThan));
        assert_eq!(lexer.next(), Some(Token::Number(10)));
        assert_eq!(lexer.next(), Some(Token::RightParen));
        assert_eq!(lexer.next(), Some(Token::LeftBrace));
        assert_eq!(lexer.next(), Some(Token::Return));
        assert_eq!(lexer.next(), Some(Token::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(Token::Add));
        assert_eq!(lexer.next(), Some(Token::Number(1)));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
        assert_eq!(lexer.next(), Some(Token::RightBrace));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_invalid_token() {
        let input: Vec<char> = "@".chars().collect();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::InValid));
    }

    #[test]
    fn test_whitespace_handling() {
        let input: Vec<char> = "  let   x   =  5   ;   ".chars().collect();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::Let));
        assert_eq!(lexer.next(), Some(Token::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(Token::Assign));
        assert_eq!(lexer.next(), Some(Token::Number(5)));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
        assert_eq!(lexer.next(), None);
    }
}
