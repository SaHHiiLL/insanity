use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peeker::Cursor,
};

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

fn parse_expression(tokens: &mut Cursor<Token>) -> AstResult<AstNode> {
    while let Some(token) = tokens.next_if(|t| *t.token_type() != TokenType::SemiColon) {
        match token.token_type() {
            TokenType::Number(num) => {
                if tokens
                    .next_if(|t| *t.token_type() == TokenType::SemiColon)
                    .is_some()
                {
                    return Ok(AstNode::Number(*num));
                } else {
                    // We have something else to parse such as a binary expression or arithmetic
                    match tokens.next() {
                        Some(next_token) => match next_token.token_type() {
                            TokenType::Add => {
                                let rhs = parse_expression(tokens)?;
                                return Ok(AstNode::Arithmetic {
                                    lhs: Box::new(AstNode::Number(*num)),
                                    rhs: Box::new(rhs),
                                    arithmetic_type: ArithmeticType::Addition,
                                });
                            }
                            TokenType::Minus => {
                                let rhs = parse_expression(tokens)?;
                                return Ok(AstNode::Arithmetic {
                                    lhs: Box::new(AstNode::Number(*num)),
                                    rhs: Box::new(rhs),
                                    arithmetic_type: ArithmeticType::Subtraction,
                                });
                            }
                            TokenType::Multiply => {
                                let rhs = parse_expression(tokens)?;
                                return Ok(AstNode::Arithmetic {
                                    lhs: Box::new(AstNode::Number(*num)),
                                    rhs: Box::new(rhs),
                                    arithmetic_type: ArithmeticType::Multiplication,
                                });
                            }
                            TokenType::Divide => {
                                let rhs = parse_expression(tokens)?;
                                return Ok(AstNode::Arithmetic {
                                    lhs: Box::new(AstNode::Number(*num)),
                                    rhs: Box::new(rhs),
                                    arithmetic_type: ArithmeticType::Division,
                                });
                            }

                            _ => {
                                todo!("Case not handled: {:?}", next_token.token_type())
                            }
                        },
                        None => {
                            return Err(ParserError::ExpectedTokenGotNone(TokenType::SemiColon))
                        }
                    }
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
                            return Ok(AstNode::StringLiteral(format!(
                                "{}{}",
                                string, rhs_string
                            )));
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
                return Ok(AstNode::Identifier {
                    ident: ident.clone(),
                });
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
}
