// https://github.com/ThePrimeagen/ts-rust-zig-deez/blob/3441b21d30a8d8488f2bec85cef4f9054891e9ea/rust_dr/src/ast/mod.rs

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

#[derive(Debug)]
pub(crate) struct ReturnStatement {
    value: Expression,
}

#[derive(Debug)]
pub(crate) struct Identifier {
    name: String,
}

#[derive(Debug)]
pub(crate) struct Literal {
    value: String,
}
