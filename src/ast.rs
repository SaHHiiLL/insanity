use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peeker::Cursor,
};
use std::ops::Add;

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

fn parse_primary(tokens: &mut Cursor<Token>) -> AstResult<AstNode> {
    if let Some(token) = tokens.next() {
        match token.token_type() {
            TokenType::Number(num) => Ok(AstNode::Number(*num)),
            TokenType::StringLiteral(string) => Ok(AstNode::StringLiteral(string.clone())),
            TokenType::Identifier(ident) => Ok(AstNode::Identifier {
                ident: ident.to_string(),
            }),
            TokenType::LeftParen => {
                let expression = parse_expression(tokens)?;
                if tokens
                    .next_if(|t| *t.token_type() == TokenType::RightParen)
                    .is_some()
                {
                    Ok(expression)
                } else {
                    panic!("Expected closing paren");
                }
            }
            _ => panic!("Unexpected token: {:?}", token),
        }
    } else {
        panic!("Expected token");
    }
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

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Zero = 0,
    Low,
    High,
    Max,
}

impl Add for Precedence {
    type Output = Precedence;

    fn add(self, rhs: Self) -> Self::Output {
        if self == Precedence::Max || rhs == Precedence::Max {
            return Precedence::Max;
        }
        if self == Precedence::Zero {
            return rhs;
        }
        if rhs == Precedence::Zero {
            return self;
        }
        if self == Precedence::Low && rhs == Precedence::High {
            return Precedence::High;
        }
        if self == Precedence::High && rhs == Precedence::Low {
            return Precedence::High;
        }
        if self == Precedence::Low && rhs == Precedence::Low {
            return Precedence::Low;
        }
        if self == Precedence::High && rhs == Precedence::High {
            return Precedence::High;
        }
        if self == Precedence::Low && rhs == Precedence::Max {
            return Precedence::Max;
        }
        if self == Precedence::Max && rhs == Precedence::Low {
            return Precedence::Max;
        }
        if self == Precedence::High && rhs == Precedence::Max {
            return Precedence::Max;
        }
        if self == Precedence::Max && rhs == Precedence::High {
            return Precedence::Max;
        }
        panic!("Invalid precedence: {:?} + {:?}", self, rhs);
    }
}

impl From<usize> for Precedence {
    fn from(value: usize) -> Self {
        match value {
            0 => Precedence::Zero,
            1 => Precedence::Low,
            2 => Precedence::High,
            _ => Precedence::Max,
        }
    }
}

fn parse_expression1(
    tokens: &mut Cursor<Token>,
    mut lhs: AstNode,
    min_precedence: Precedence,
) -> AstResult<AstNode> {
    let mut lookahead = {
        tokens
            .next_if(|t| token_precedence(t.token_type()).is_some())
            .ok_or(ParserError::ExpectedExpression(
                "expected expression got NONE",
            ))?
    };

    while let Some(precedence) = token_precedence(lookahead.token_type()) {
        if precedence < min_precedence {
            break;
        }

        let operator = lookahead;
        let op_pre = token_precedence(operator.token_type())
            .expect("operator should always be an operator because of the fist while loop");

        let mut rhs = { parse_primary(tokens) }?;
        lookahead = tokens.next().unwrap();

        while let Some(precedence) = token_precedence(lookahead.token_type()) {
            if precedence < op_pre {
                break;
            }

            let rhs_pre = precedence
                + Precedence::from({
                    if let Some(pre) = token_precedence(lookahead.token_type()) {
                        if pre > op_pre {
                            1
                        } else {
                            0
                        }
                    } else {
                        0
                    }
                });
            rhs = parse_expression1(tokens, rhs, rhs_pre).unwrap();
            lookahead = tokens.next().unwrap();
        }

        lhs = match operator.token_type() {
            TokenType::Add => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Addition,
            },
            TokenType::Minus => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Subtraction,
            },
            TokenType::Multiply => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Multiplication,
            },
            TokenType::Divide => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Division,
            },
            _ => panic!("Invalid operator"),
        };
    }

    Ok(lhs)
}

/// returns a Some(_) varient if token type is a binary operator
fn token_precedence(token: &TokenType) -> Option<Precedence> {
    match token {
        TokenType::Add | TokenType::Minus => Some(Precedence::Low),
        TokenType::Divide | TokenType::Multiply => Some(Precedence::High),
        _ => None,
    }
}

#[deprecated]
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
                    return parse_expression1(tokens, lhs, Precedence::Zero);
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
