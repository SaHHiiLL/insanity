/*
*
* project(<NAME>, <VERSION>, [DESCRIPTION])
*
* define_library(<LIB_NAME>, path=)
*
*/

use std::time::Instant;

use iter_tools::Itertools;

use super::peaker::Peaker;

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

    // TODO: change this to a two pointer approach and return a string slice
    fn read_identifer(&mut self) -> Option<String> {
        let start_idx = self.input.idx();
        let mut end_idx = start_idx;

        while self.input.get()?.is_ascii_alphabetic() {
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

    fn read_identifer2(&mut self) -> Option<String> {
        let mut buffer = String::new();
        while self.input.get()?.is_ascii_alphabetic() {
            buffer.push(*self.input.get()?);
            self.input.next()?;
        }
        Some(buffer)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ident;
        let res = match self.input.next()? {
            'a'..='z' | 'A'..='Z' => {
                let i = Instant::now();
                ident = self.read_identifer()?;
                eprintln!("Time: {}", i.elapsed().as_nanos());
                Some(Token::Identifier(dbg!(ident)))
            }
            '0'..='9' => todo!("Number"),
            ')' => Some(Token::RightParen),
            '(' => Some(Token::LeftParen),
            '}' => Some(Token::RightBrace),
            '{' => Some(Token::LeftBrace),
            '=' => Some(Token::Equals),
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
        let input = "(kkkkkkkkkkkkkkkkkkkkkkkkjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhjhsd){(sa}}=".chars().collect::<Vec<char>>();
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
        // assert_eq!(lexed_tokens.len(), expected.len());
    }
}
