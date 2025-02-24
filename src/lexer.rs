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

    fn read_identifer(&mut self) -> Option<String> {
        let start_idx = self.input.idx();
        let mut end_idx = start_idx;

        while self.input.get()?.is_alphanumeric() {
            end_idx = self.input.idx();
            self.input.next()?;
        }
        Some(
            self.input
                .slice_x_y(start_idx, end_idx)?
                .iter()
                .cloned()
                .collect::<String>(),
        )
    }

    fn read_number(&mut self) -> Option<String> {
        let start_idx = self.input.idx();
        let mut end_idx = start_idx;

        while self.input.get()?.is_numeric() {
            end_idx = self.input.idx();
            self.input.next()?;
        }
        Some(
            self.input
                .slice_x_y(start_idx, end_idx)?
                .iter()
                .cloned()
                .collect::<String>(),
        )
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_spaces();
        let res = match self.input.next()? {
            'a'..='z' | 'A'..='Z' => {
                let ident = self.read_identifer()?.to_string();
                Some(Token::Identifier(dbg!(ident.to_string())))
            }
            '0'..='9' => {
                let ident = self.read_number()?.to_string();
                Some(Token::Identifier(dbg!(ident.to_string())))
            }
            ')' => Some(Token::RightParen),
            '(' => Some(Token::LeftParen),
            '}' => Some(Token::RightBrace),
            '{' => Some(Token::LeftBrace),
            '=' => Some(Token::Equals),
            ',' => Some(Token::Comma),
            '\0' => None,
            _ => None,
        };
        res
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "(){()}=".chars().collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let expected = &[
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::LeftParen,
            Token::RightParen,
            Token::RightBrace,
            Token::Equals,
        ];

        let lexed_tokens: Vec<Token> = lexer.collect();
        assert_eq!(lexed_tokens.len(), expected.len());
        assert_eq!(lexed_tokens, expected);
    }
}
