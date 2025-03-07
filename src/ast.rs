// https://github.com/ThePrimeagen/ts-rust-zig-deez/blob/3441b21d30a8d8488f2bec85cef4f9054891e9ea/rust_dr/src/ast/mod.rs

use crate::lexer::Token;

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
    Literal(Literal),
}

impl From<Vec<Token>> for Expression {
    fn from(_value: Vec<Token>) -> Self {
        todo!()
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
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Identifier(ident) => Ok(Self { name: ident }),
            _ => Err(()),
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
