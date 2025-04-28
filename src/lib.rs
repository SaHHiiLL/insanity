#![allow(dead_code)]
#![allow(warnings)]
mod ast;
mod error;
mod lexer;
mod peeker;

pub use self::lexer::Lexer;
