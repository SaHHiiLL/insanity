/*
*
* project(<NAME>, <VERSION>, [DESCRIPTION])
*
* define_library(<LIB_NAME>, path=)
*
*/

use super::peaker::Peaker;

enum Token<'a> {
    Identifier(&'a str),
    Number(i64),
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Comma,
    StringLiteral(&'a str),
    Equals,
    NotEquals,
}

struct Lexer<'a> {
    input: Peaker<'a, char>,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a [char]) -> Self {
        Self {
            input: Peaker::new(input),
            position: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.input.next()? {
            'a'..='z' | 'A'..='Z' => todo!("Identifier"),
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

#[cfg(test)]
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
    }
}
