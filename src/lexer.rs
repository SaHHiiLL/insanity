use crate::error::LexerError;

use super::peeker::{Cursor, MoveBackIterator};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    inner: TokenType,
    line_nbr: usize,
    char_nbr: usize,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token {{ TokenType: {},\n\tline_nbr: {},\n\tchar_nbr: {} }}",
            self.inner, self.line_nbr, self.char_nbr
        )
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = match self {
            TokenType::Identifier(ident) => &format!("{}", ident),
            TokenType::Number(num) => &format!("{}", num),
            TokenType::StringLiteral(literal) => &format!("{}", literal),
            TokenType::Let => "let",
            TokenType::Return => "return",
            TokenType::Process => "Process",
            TokenType::Function => "Function",
            TokenType::True => "True",
            TokenType::False => "False",
            TokenType::ForLoop => "ForLoop",
            TokenType::WhileLoop => "WhileLoop",
            TokenType::Break => "Break",
            TokenType::Continue => "Continue",
            TokenType::If => "If",
            TokenType::Else => "Else",
            TokenType::ElseIf => "ElseIf",
            TokenType::RightParen => "RightParen",
            TokenType::LeftParen => "LeftParen",
            TokenType::RightBrace => "RightBrace",
            TokenType::LeftBrace => "LeftBrace",
            TokenType::Comma => "Comma",
            TokenType::SemiColon => "SemiColon",
            TokenType::Assign => "Assign",
            TokenType::NotEquals => "NotEquals",
            TokenType::Minus => "Minus",
            TokenType::Add => "Add",
            TokenType::Divide => "Divide",
            TokenType::Multiply => "Multiply",
            TokenType::LessOrEqualThan => "LessOrEqualThan",
            TokenType::LessThan => "LessThan",
            TokenType::GreaterOrEqualThan => "GreaterOrEqualThan",
            TokenType::GreaterThan => "GreaterThan",
            TokenType::InValid(error) => &format!("InValid({})", error),
        };
        write!(f, "{}", x)
    }
}

impl Token {
    pub fn token_type(&self) -> &TokenType {
        &self.inner
    }

    pub fn line_nbr(&self) -> &usize {
        &self.line_nbr
    }

    pub fn char_nbr(&self) -> &usize {
        &self.char_nbr
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
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

    // clone parenthesis
    RightParen,
    // Open parenthesis
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

    InValid(crate::error::LexerError),
}

#[derive(Debug)]
pub struct Lexer {
    input: Cursor<char>,
    curr_line: usize,
    curr_char: usize,
}

impl Lexer {
    pub fn new(input: &[char]) -> Self {
        let input = input.iter().map(|d| d.clone()).collect::<Vec<_>>();
        Self {
            input: Cursor::new(input),
            curr_line: 1,
            curr_char: 1,
        }
    }

    fn skip_spaces(&mut self) {
        while let Some(x) = self.input.get() {
            if x.is_whitespace() {
                if *x == '\n' {
                    self.curr_line += 1;
                    self.curr_char = 1;
                }
                self.read_next();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> Result<String, ()> {
        let mut buffer = String::new();
        while let Some(x) = self.input.get() {
            if x.is_alphanumeric() || *x == '_' || *x == '-' {
                buffer.push(*x);
                self.read_next();
            } else {
                break;
            }
        }
        // On success moves the cursor back
        self.input.prev().expect("Should Never fail");
        Ok(buffer)
    }

    fn read_number(&mut self) -> Result<String, ()> {
        let mut buffer = String::new();
        while let Some(x) = self.input.get() {
            if x.is_numeric() {
                buffer.push(*x);
                self.read_next();
            } else {
                break;
            }
        }

        // On success moves the cursor back
        self.input.prev().expect("Should Never fail");
        Ok(buffer)
    }

    /// Reads the next char and increments the line number and current char number incase of Some()
    fn read_next(&mut self) -> Option<char> {
        self.input.next().map(|c| {
            if c == '\n' {
                self.curr_line += 1;
                self.curr_char = 1;
            } else {
                self.curr_char += 1;
            }
            c
        })
    }

    fn read_string(&mut self) -> Result<String, ()> {
        let mut buffer = String::new();
        self.read_next();

        while let Some(x) = self.input.get() {
            if *x != '"' {
                buffer.push(*x);
                self.read_next();
            } else {
                break;
            }
        }

        Ok(buffer)
    }
}

impl From<String> for Lexer {
    fn from(value: String) -> Self {
        Self::new(&value.chars().collect::<Vec<char>>())
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_spaces();
        let char_nbr = self.curr_char;
        let start_idx = self.input.idx();
        let res = match self.input.get()? {
            'a'..='z' | 'A'..='Z' => {
                let ident = self.read_identifier();
                match ident {
                    Ok(ident) => match ident.as_str() {
                        "return" => Some(TokenType::Return),
                        "for" => Some(TokenType::ForLoop),
                        "while" => Some(TokenType::WhileLoop),
                        "let" => Some(TokenType::Let),
                        "proc" => Some(TokenType::Process),
                        "fn" => Some(TokenType::Function),
                        "true" => Some(TokenType::True),
                        "false" => Some(TokenType::False),
                        "break" => Some(TokenType::Break),
                        "continue" => Some(TokenType::Continue),
                        "if" => Some(TokenType::If),
                        "else" => Some(TokenType::Else),
                        "elif" => Some(TokenType::ElseIf),
                        _ => Some(TokenType::Identifier(ident)),
                    },
                    Err(_) => Some(TokenType::InValid(LexerError::new_string(
                        char_nbr,
                        self.curr_line,
                        self.input
                            .n_slice(start_idx)
                            .unwrap()
                            .iter()
                            .collect::<String>(),
                    ))),
                }
            }
            '0'..='9' => Some(
                self.read_number()
                    // should never panic because read_number only reads numbers
                    .map_or(
                        TokenType::InValid(LexerError::new_string(
                            char_nbr,
                            self.curr_line,
                            self.input
                                .n_slice(start_idx)
                                .unwrap()
                                .iter()
                                .collect::<String>(),
                        )),
                        |z| TokenType::Number(z.parse().unwrap()),
                    ),
            ),
            ';' => Some(TokenType::SemiColon),
            '-' => Some(TokenType::Minus),
            '+' => Some(TokenType::Add),
            '/' => Some(TokenType::Divide),
            '*' => Some(TokenType::Multiply),
            ')' => Some(TokenType::RightParen),
            '(' => Some(TokenType::LeftParen),
            '}' => Some(TokenType::RightBrace),
            '{' => Some(TokenType::LeftBrace),
            '=' => Some(TokenType::Assign),
            '<' => {
                if let Some(peak) = self.input.peek_ahead() {
                    if *peak == '=' {
                        Some(TokenType::LessOrEqualThan)
                    } else {
                        Some(TokenType::LessThan)
                    }
                } else {
                    Some(TokenType::LessThan)
                }
            }
            '>' => {
                if let Some(peak) = self.input.peek_ahead() {
                    if *peak == '=' {
                        Some(TokenType::GreaterOrEqualThan)
                    } else {
                        Some(TokenType::GreaterThan)
                    }
                } else {
                    Some(TokenType::GreaterThan)
                }
            }
            '!' => {
                if let Some(peak) = self.input.peek_ahead() {
                    if *peak == '=' {
                        self.read_next();
                        Some(TokenType::NotEquals)
                    } else {
                        Some(TokenType::InValid(LexerError::new_string(
                            char_nbr,
                            self.curr_line,
                            self.input.n_slice(start_idx).unwrap().iter().collect(),
                        )))
                    }
                } else {
                    Some(TokenType::InValid(LexerError::new_string(
                        char_nbr,
                        self.curr_line,
                        self.input.n_slice(start_idx).unwrap().iter().collect(),
                    )))
                }
            }
            '"' => Some(self.read_string().map_or(
                TokenType::InValid(LexerError::new_string(
                    char_nbr,
                    self.curr_line,
                    self.input.n_slice(start_idx).unwrap().iter().collect(),
                )),
                TokenType::StringLiteral,
            )),
            ',' => Some(TokenType::Comma),
            '\0' => None,
            _ => Some(TokenType::InValid(LexerError::new_string(
                char_nbr,
                self.curr_line,
                self.input
                    .n_slice(start_idx)
                    .unwrap()
                    .iter()
                    .collect::<String>(),
            ))),
        };
        self.read_next();
        Some(Token {
            inner: res?,
            line_nbr: self.curr_line,
            char_nbr,
        })
    }
}

mod tests {
    #![allow(unused_imports)]
    use std::clone;

    use crate::{
        error::LexerError,
        lexer::{Token, TokenType},
        Lexer,
    };

    #[test]
    fn test_lexer() {
        let input = "(){()}=!=".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::RightBrace,
            TokenType::Assign,
            TokenType::NotEquals,
        ];

        let lexed_tokens = lexer.map(|x| x.token_type().clone()).collect::<Vec<_>>();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }
    #[test]
    fn test_string() {
        let input = "\"Hello World\"".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[TokenType::StringLiteral("Hello World".to_string())];

        let lexed_tokens: Vec<TokenType> = lexer.map(|d| d.token_type().clone()).collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }
    #[test]
    fn test_ident() {
        let input = "Hey world!= == ()".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            TokenType::Identifier("Hey".to_string()),
            TokenType::Identifier("world".to_string()),
            TokenType::NotEquals,
            TokenType::Assign,
            TokenType::Assign,
            TokenType::LeftParen,
            TokenType::RightParen,
        ];

        let lexed_tokens: Vec<TokenType> = lexer.map(|d| d.token_type().clone()).collect();
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
            TokenType::Function,
            TokenType::Identifier("foo".to_string()),
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Let,
            TokenType::Identifier("bar".to_string()),
            TokenType::Assign,
            TokenType::StringLiteral("Hello World!".to_string()),
            TokenType::SemiColon,
            TokenType::Return,
            TokenType::Minus,
            TokenType::Number(1337),
            TokenType::SemiColon,
            TokenType::RightBrace,
        ];
        let chars = &input.chars().collect::<Vec<char>>();
        let lexer = Lexer::new(chars);
        let lexed_tokens: Vec<TokenType> = lexer.map(|d| d.token_type().clone()).collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }

    #[test]
    fn test_let_statement() {
        let input: Vec<char> = "let x = 42;".chars().collect();
        let mut lexer = Lexer::new(&input).map(|d| d.token_type().clone());
        assert_eq!(lexer.next(), Some(TokenType::Let));
        assert_eq!(lexer.next(), Some(TokenType::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(TokenType::Assign));
        assert_eq!(lexer.next(), Some(TokenType::Number(42)));
        assert_eq!(lexer.next(), Some(TokenType::SemiColon));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_if_branch() {
        let input: Vec<char> = "if (x < 10) { return x + 1; }".chars().collect();
        let mut lexer = Lexer::new(&input).map(|d| d.token_type().clone());
        assert_eq!(lexer.next(), Some(TokenType::If));
        assert_eq!(lexer.next(), Some(TokenType::LeftParen));
        assert_eq!(lexer.next(), Some(TokenType::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(TokenType::LessThan));
        assert_eq!(lexer.next(), Some(TokenType::Number(10)));
        assert_eq!(lexer.next(), Some(TokenType::RightParen));
        assert_eq!(lexer.next(), Some(TokenType::LeftBrace));
        assert_eq!(lexer.next(), Some(TokenType::Return));
        assert_eq!(lexer.next(), Some(TokenType::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(TokenType::Add));
        assert_eq!(lexer.next(), Some(TokenType::Number(1)));
        assert_eq!(lexer.next(), Some(TokenType::SemiColon));
        assert_eq!(lexer.next(), Some(TokenType::RightBrace));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_invalid_token() {
        let input: Vec<char> = "@".chars().collect();
        let lexer = Lexer::new(&input);
        let mut token_types = lexer
            .collect::<Vec<_>>()
            .into_iter()
            .map(|d| d.token_type().clone());
        let expected = Some(TokenType::InValid(LexerError::new_string(
            1,
            1,
            "@".to_string(),
        )));
        assert_eq!(token_types.next(), expected)
    }

    #[test]
    fn test_shoudl_pass() {
        let input: Vec<char> = "!= <><>".chars().collect();
        let lexer = Lexer::new(&input);
        let tokens = lexer.collect::<Vec<_>>();
        let expected = vec![
            Token {
                inner: TokenType::NotEquals,
                line_nbr: 1,
                char_nbr: 1,
            },
            Token {
                inner: TokenType::LessThan,
                line_nbr: 1,
                char_nbr: 4,
            },
            Token {
                inner: TokenType::GreaterThan,
                line_nbr: 1,
                char_nbr: 5,
            },
            Token {
                inner: TokenType::LessThan,
                line_nbr: 1,
                char_nbr: 6,
            },
            Token {
                inner: TokenType::GreaterThan,
                line_nbr: 1,
                char_nbr: 7,
            },
        ];
        assert_eq!(tokens, expected)
    }

    #[test]
    fn test_whitespace_handling() {
        let input: Vec<char> = "  let   x   =  5   ;   @@@@@".chars().collect();
        let lexer = Lexer::new(&input);
        let mut lexer = lexer
            .collect::<Vec<_>>()
            .into_iter()
            .map(|f| f.token_type().clone());

        assert_eq!(lexer.next(), Some(TokenType::Let));
        assert_eq!(lexer.next(), Some(TokenType::Identifier("x".to_string())));
        assert_eq!(lexer.next(), Some(TokenType::Assign));
        assert_eq!(lexer.next(), Some(TokenType::Number(5)));
        assert_eq!(lexer.next(), Some(TokenType::SemiColon));
        assert_eq!(
            lexer.next(),
            Some(TokenType::InValid(LexerError::test(27, 1, "@")))
        );
        assert_eq!(
            lexer.next(),
            Some(TokenType::InValid(LexerError::test(28, 1, "@")))
        );
        assert_eq!(
            lexer.next(),
            Some(TokenType::InValid(LexerError::test(29, 1, "@")))
        );
        assert_eq!(
            lexer.next(),
            Some(TokenType::InValid(LexerError::test(30, 1, "@")))
        );
        assert_eq!(
            lexer.next(),
            Some(TokenType::InValid(LexerError::test(31, 1, "@")))
        )
    }
}
