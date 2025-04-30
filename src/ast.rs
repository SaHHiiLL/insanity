use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peeker::{Cursor, MoveBackIterator},
};
use std::ops::Add;
use std::thread::current;

type BAstNode = Box<AstNode>;

#[derive(Debug, PartialEq)]
pub(crate) enum ArithmeticType {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug, PartialEq)]
pub(crate) enum LogicalType {
    Equals,
    NotEquals,
}

#[derive(Debug, PartialEq)]
pub(crate) enum AstNode {
    Assignment {
        ident: String,
        expression: BAstNode,
    },
    Identifier {
        ident: String,
    },
    Return {
        expression: BAstNode,
    },
    StringLiteral(String),
    Scope(Vec<BAstNode>),
    Number(i64),
    Float(f64),
    Arithmetic {
        lhs: BAstNode,
        rhs: BAstNode,
        arithmetic_type: ArithmeticType,
    },
    Logical {
        lhs: BAstNode,
        rhs: BAstNode,
        logical_type: LogicalType,
    },
    Boolean(bool),
    Paren(BAstNode),
    FunctionCall(Vec<BAstNode>),
    FunctionDefinition {
        name: String,
        args: Vec<String>,
        body: BAstNode,
    },
}

#[cfg(test)]
mod test {
    use crate::ast;
    use crate::parser::Parser::*;
    use crate::Lexer;

    #[test]
    fn test_long_program() {
        let input = r#"let x = 10;
            let y = 20;
            let z = x + y;
            let a = x - y;
        "#;

        let lexer = Lexer::from(input.to_string());
        let tokens = lexer.collect::<Vec<_>>();
        let ast = parse(tokens);
        let expected = vec![
            ast::AstNode::Assignment {
                ident: String::from("x"),
                expression: Box::new(ast::AstNode::Number(10)),
            },
            ast::AstNode::Assignment {
                ident: String::from("y"),
                expression: Box::new(ast::AstNode::Number(20)),
            },
            ast::AstNode::Assignment {
                ident: String::from("z"),
                expression: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Identifier {
                        ident: String::from("x"),
                    }),
                    rhs: Box::new(ast::AstNode::Identifier {
                        ident: String::from("y"),
                    }),
                    arithmetic_type: ast::ArithmeticType::Addition,
                }),
            },
            ast::AstNode::Assignment {
                ident: String::from("a"),
                expression: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Identifier {
                        ident: String::from("x"),
                    }),
                    rhs: Box::new(ast::AstNode::Identifier {
                        ident: String::from("y"),
                    }),
                    arithmetic_type: ast::ArithmeticType::Subtraction,
                }),
            },
        ];

        assert_eq!(ast, Ok(expected));
    }

    #[test]
    fn test_let_statement() {
        let input = String::from("let x = 10;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = parse(tokens);
        assert!(program.is_ok());
    }

    #[test]
    fn test_let_statement_arithmetic() {
        let input = String::from("let x = 10 + 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = parse(tokens);
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Number(20)),
                arithmetic_type: ast::ArithmeticType::Addition,
            }),
        }];
        assert_eq!(program, expected);

        let input = String::from("let x = 10 - 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Number(20)),
                arithmetic_type: ast::ArithmeticType::Subtraction,
            }),
        }];
        assert_eq!(program, expected);

        let input = String::from("let x = 10 * 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Number(20)),
                arithmetic_type: ast::ArithmeticType::Multiplication,
            }),
        }];
        assert_eq!(program, expected);

        let input = String::from("let x = 10 / 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Number(20)),
                arithmetic_type: ast::ArithmeticType::Division,
            }),
        }];
        assert_eq!(program, expected);

        let input = String::from("let x = 10 / 20 + 30;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let unexpected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Number(20)),
                    rhs: Box::new(ast::AstNode::Number(30)),
                    arithmetic_type: ast::ArithmeticType::Addition,
                }),
                arithmetic_type: ast::ArithmeticType::Division,
            }),
        }];
        assert_ne!(program, unexpected);
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Number(10)),
                    rhs: Box::new(ast::AstNode::Number(20)),
                    arithmetic_type: ast::ArithmeticType::Division,
                }),
                rhs: Box::new(ast::AstNode::Number(30)),
                arithmetic_type: ast::ArithmeticType::Addition,
            }),
        }];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_arithmetic_precedence() {
        let input = String::from("let x = 10 * 20 + 30;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Number(10)),
                    rhs: Box::new(ast::AstNode::Number(20)),
                    arithmetic_type: ast::ArithmeticType::Multiplication,
                }),
                rhs: Box::new(ast::AstNode::Number(30)),
                arithmetic_type: ast::ArithmeticType::Addition,
            }),
        }];
        assert_eq!(program, expected);
    }

    #[test]
    fn test_simple_arithmetic() {
        let input = String::from("let x = 10 + 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = parse(tokens);
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Number(20)),
                arithmetic_type: ast::ArithmeticType::Addition,
            }),
        }];
        assert_eq!(program, Ok(expected));
    }

    #[test]
    fn test_long_arithmetic_precedence() {
        let input = String::from("let x = 10 + 20 * 30;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = parse(tokens);
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Number(10)),
                rhs: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Number(20)),
                    rhs: Box::new(ast::AstNode::Number(30)),
                    arithmetic_type: ast::ArithmeticType::Multiplication,
                }),
                arithmetic_type: ast::ArithmeticType::Addition,
            }),
        }];
        assert_eq!(program, Ok(expected));
    }

    #[test]
    fn test_same_precedence() {
        let input = String::from("let x = 10 + 20 - 30;");

        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = parse(tokens);
        let expected = vec![ast::AstNode::Assignment {
            ident: String::from("x"),
            expression: Box::new(ast::AstNode::Arithmetic {
                lhs: Box::new(ast::AstNode::Arithmetic {
                    lhs: Box::new(ast::AstNode::Number(10)),
                    rhs: Box::new(ast::AstNode::Number(20)),
                    arithmetic_type: ast::ArithmeticType::Addition,
                }),
                rhs: Box::new(ast::AstNode::Number(30)),
                arithmetic_type: ast::ArithmeticType::Subtraction,
            }),
        }];
        assert_eq!(program, Ok(expected));
    }
}
