use crate::ast::*;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            errors: vec![],
        }
    }

    fn parse(&mut self) -> Option<Program> {
        let res = Program::new();

        while let Some(token) = self.lexer.next() {
            match token {
                Token::Let => todo!(),
                Token::Return => todo!(),
                Token::Identifier(_ident) => todo!(),
                _ => todo!(),
            }
        }
        Some(res)
    }

    fn parse_let(&mut self) -> Result<LetStatement, ()> {
        let ident = self.lexer.next().expect("Expected Identifier got none");
        let assign = self.lexer.next().expect("Expected `=` got None");
        assert_eq!(
            Token::Assign,
            assign,
            "Expedted `=` got something else {:?}",
            assign
        );
    }
}
