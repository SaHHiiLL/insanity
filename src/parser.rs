use crate::ast::*;
use crate::error::ParserError;
use crate::lexer::{Lexer, TokenType};

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
            let token = token.token_type();
            let _ = match token {
                TokenType::Let => self.parse_let(),
                TokenType::Return => todo!(),
                TokenType::SemiColon => continue,
                TokenType::Identifier(_ident) => todo!(),
                _ => todo!(),
            };
        }
        Some(res)
    }

    fn parse_let(&mut self) -> Result<LetStatement, ParserError> {
        let ident = self.lexer.next().expect("Expected Identifier got none");
        let assign = self.lexer.next().expect("Expected `=` got None");
        assert_eq!(
            &TokenType::Assign,
            assign.token_type(),
            "Expedted `=` got something else {:?}",
            assign
        );
        let mut expression_tokens = vec![];
        for token in self.lexer.by_ref() {
            if *token.token_type() == TokenType::SemiColon {
                break;
            }
            expression_tokens.push(token);
        }
        let expression = Expression::try_from(expression_tokens)?;
        Ok(LetStatement::new(Identifier::try_from(ident)?, expression))
    }
}

#[cfg(test)]
mod test {
    use crate::Lexer;

    use super::Parser;

    #[test]
    fn test_let_statement() {
        let input = r#"
            let x = 5 * 5;
        "#
        .chars()
        .collect::<Vec<char>>();
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        parser.parse();
    }
}
