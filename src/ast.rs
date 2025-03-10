// https://github.com/ThePrimeagen/ts-rust-zig-deez/blob/3441b21d30a8d8488f2bec85cef4f9054891e9ea/rust_dr/src/ast/mod.rs

use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peaker::Cursor,
};

#[derive(Debug)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ProcessCall(ProcessStatement),
}

#[derive(Debug)]
pub(crate) struct ProcessStatement {
    expression: Expression,
}

#[derive(Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
    StringLiteral(Literal),
}

impl TryFrom<Vec<Token>> for Expression {
    type Error = ParserError;

    fn try_from(value: Vec<Token>) -> Result<Self, Self::Error> {
        let mut cursor = Cursor::new(&value);
        let x = cursor
            .next()
            .ok_or(ParserError::ExpectedExpression("None".to_string()))?;
        let token_type = x.token_type();
        match token_type {
            TokenType::Number(_) => todo!(),
            TokenType::Identifier(_) => todo!(),
            TokenType::StringLiteral(literal_value) => Ok(Expression::StringLiteral(
                Literal::from(literal_value.to_owned()),
            )),
            TokenType::True => todo!(),
            TokenType::False => todo!(),
            TokenType::Minus => todo!(),
            _ => return Err(ParserError::InvalidExpression(x.clone())),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub(crate) fn new() -> Self {
        Self { statements: vec![] }
    }
}

#[derive(Debug)]
pub(crate) struct LetStatement {
    ident: Identifier,
    value: Expression,
}

impl LetStatement {
    pub(crate) fn new(ident: Identifier, expression: Expression) -> Self {
        Self {
            ident,
            value: expression,
        }
    }
}

#[derive(Debug)]
pub(crate) struct ReturnStatement {
    value: Expression,
}

#[derive(Debug)]
pub(crate) struct Identifier {
    name: String,
}

impl TryFrom<Token> for Identifier {
    type Error = ParserError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token_type() {
            TokenType::Identifier(ident) => Ok(Self {
                name: ident.to_owned(),
            }),
            _ => Err(ParserError::InvalidIdentifier(value)),
        }
    }
}

impl Identifier {
    pub(crate) fn new(ident: String) -> Self {
        Self { name: ident }
    }
}

#[derive(Debug)]
pub(crate) struct Literal {
    value: String,
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self { value }
    }
}
