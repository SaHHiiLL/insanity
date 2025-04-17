use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peeker::Cursor,
};
use lazy_static::lazy_static;
use std::collections::HashMap;

type BAstNode = Box<AstNode>;

#[derive(Debug, PartialEq)]
pub(crate) enum ArithmeticType {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl ArithmeticType {
    fn weighted(&self) -> i64 {
        match self {
            ArithmeticType::Addition | ArithmeticType::Subtraction => 1,
            ArithmeticType::Multiplication | ArithmeticType::Division => 2,
        }
    }
}

impl PartialOrd for ArithmeticType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.weighted().cmp(&other.weighted()))
    }
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
    EmptyExpression,
}

type AstResult<T> = Result<T, ParserError>;

pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, Vec<ParserError>> {
    let mut errors: Vec<ParserError> = vec![];
    let mut ast: Vec<AstNode> = vec![];
    let mut token_cursor = Cursor::new(tokens);

    while token_cursor.get().is_some() {
        if token_cursor
            .next_if(|t| *t.token_type() == TokenType::Let)
            .is_some()
        {
            match parse_let(&mut token_cursor) {
                Ok(node) => {
                    ast.push(node);
                }
                Err(e) => {
                    errors.push(e);
                    break;
                }
            }
        }
    }
    Ok(ast)
}

fn parse_let(tokens: &mut Cursor<Token>) -> AstResult<AstNode> {
    if let Some(ident) = tokens.next_if(|t| matches!(t.token_type(), TokenType::Identifier(_))) {
        if tokens
            .next_if(|t| *t.token_type() == TokenType::Assign)
            .is_some()
        {
            // parse the expression at the end
            let expression = Box::new(parse_expression(tokens)?);
            Ok(AstNode::Assignment {
                ident: ident.token_type().to_string(),
                expression,
            })
        } else {
            panic!("Expected assign token after identifier");
        }
    } else {
        panic!("Expected identifier after let");
    }
}

lazy_static! {
    static ref PRECEDENCE: HashMap<TokenType, usize> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Add, 1);
        m.insert(TokenType::Minus, 1);
        m.insert(TokenType::Multiply, 2);
        m.insert(TokenType::Divide, 2);
        m
    };
}

fn handle_operators(tokens: &mut Cursor<Token>, lhs: AstNode) -> AstResult<AstNode> {
    if tokens
        .next_if(|t| *t.token_type() == TokenType::SemiColon)
        .is_some()
    {
        return Ok(lhs);
    }

    if tokens
        .next_if(|t| *t.token_type() == TokenType::Add)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        Ok(AstNode::Arithmetic {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arithmetic_type: ArithmeticType::Addition,
        })
    } else if tokens
        .next_if(|t| *t.token_type() == TokenType::Minus)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        return Ok(AstNode::Arithmetic {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arithmetic_type: ArithmeticType::Subtraction,
        });
    } else if tokens
        .next_if(|t| *t.token_type() == TokenType::Multiply)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        return Ok(AstNode::Arithmetic {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arithmetic_type: ArithmeticType::Multiplication,
        });
    } else if tokens
        .next_if(|t| *t.token_type() == TokenType::Divide)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        return Ok(AstNode::Arithmetic {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            arithmetic_type: ArithmeticType::Division,
        });
    } else if tokens
        .next_if(|t| *t.token_type() == TokenType::Equals)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        return Ok(AstNode::Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            logical_type: LogicalType::Equals,
        });
    } else if tokens
        .next_if(|t| *t.token_type() == TokenType::NotEquals)
        .is_some()
    {
        let rhs = parse_expression(tokens)?;
        return Ok(AstNode::Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            logical_type: LogicalType::NotEquals,
        });
    } else {
        panic!(
            "Expected operator after expression got {:?}",
            tokens.peek_ahead()
        );
    }
}

#[allow(dead_code)]
#[allow(unused_variables)]
#[allow(clippy::unnecessary_lazy_evaluations)]
fn opetator_climb(tokens: &mut Cursor<Token>, lhs: AstNode, min_pre: usize) -> AstResult<AstNode> {
    let lookahead = tokens
        .peek_ahead()
        .ok_or_else(|| ParserError::ExpectedExpression("expected expreesion got NONE"))?;

    match lookahead.token_type() {
        TokenType::Add | TokenType::Minus => {
            let lookahead_pre = PRECEDENCE
                .get(lookahead.token_type())
                .map(|p| *p > min_pre)
                .unwrap_or(false);

            while lookahead_pre {
                // can safely unwrap here
                let op = tokens.next().unwrap();
                let rhs = parse_expression(tokens)?;

                let lookahead = tokens.peek_ahead().ok_or_else(|| {
                    ParserError::ExpectedExpression("expected expreesion got NONE")
                })?;
            }
        }
        TokenType::Divide | TokenType::Multiply => {}
        _ => {
            panic!("Invalid operator: {:?}", lookahead);
        }
    }

    todo!()
}

fn parse_expression(tokens: &mut Cursor<Token>) -> AstResult<AstNode> {
    while let Some(token) = tokens.next() {
        match token.token_type() {
            TokenType::Number(num) => {
                if tokens
                    .next_if(|t| {
                        (*t.token_type() == TokenType::SemiColon)
                            || (*t.token_type() == TokenType::Comma)
                    })
                    .is_some()
                {
                    return Ok(AstNode::Number(*num));
                } else {
                    let lhs = AstNode::Number(*num);
                    return handle_operators(tokens, lhs);
                }
            }
            TokenType::StringLiteral(string) => {
                if tokens
                    .next_if(|t| *t.token_type() == TokenType::SemiColon)
                    .is_some()
                {
                    return Ok(AstNode::StringLiteral(string.clone()));
                } else if tokens
                    .next_if(|t| *t.token_type() == TokenType::Add)
                    .is_some()
                {
                    let rhs = parse_expression(tokens)?;
                    match rhs {
                        AstNode::StringLiteral(rhs_string) => {
                            return Ok(AstNode::StringLiteral(format!("{}{}", string, rhs_string)));
                        }
                        _ => {
                            panic!("Expected string literal after +")
                        }
                    }
                } else {
                    // "String A" "String B" -> "String AString B"
                    // "String A", "String B" -> "String A String B"
                }
            }
            TokenType::Identifier(ident) => {
                if tokens
                    .next_if(|t| *t.token_type() == TokenType::SemiColon)
                    .is_some()
                {
                    return Ok(AstNode::Identifier {
                        ident: ident.to_string(),
                    });
                } else {
                    let lhs = AstNode::Identifier {
                        ident: ident.to_string(),
                    };
                    return handle_operators(tokens, lhs);
                }
            }
            _ => todo!("Case not handled: {:?}", token),
        }
    }
    panic!("Expected semicolon")
}

#[cfg(test)]
mod test {
    use crate::ast;
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
        let ast = ast::parse(tokens);
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
        let program = ast::parse(dbg!(tokens));
        assert!(program.is_ok());
    }

    #[test]
    fn test_let_statement_arithmetic() {
        let input = String::from("let x = 10 + 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(ast::parse(tokens));
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
        let program = dbg!(ast::parse(tokens));
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
        let program = dbg!(ast::parse(tokens));
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
        let program = dbg!(ast::parse(tokens));
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
        let program = dbg!(ast::parse(tokens));
        assert!(program.is_ok());
        let program = program.unwrap();
        let expected = vec![ast::AstNode::Assignment {
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
        assert_eq!(program, expected);
    }

    #[test]
    fn test_arithmetic_precedence() {
        let input = String::from("let x = 10 * 20 + 30;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = dbg!(ast::parse(tokens));
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
}
