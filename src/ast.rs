use crate::{
    error::ParserError,
    lexer::{Token, TokenType},
    peeker::{Cursor, MoveBackIterator},
};
use std::ops::Add;
use std::thread::current;

type BAstNode = Box<AstNode>;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    Low,
    Medium,
    High,
}

impl Precedence {
    fn precedence(&self) -> usize {
        match self {
            Precedence::Low => 0,
            Precedence::Medium => 1,
            Precedence::High => 2,
        }
    }
}

impl Add for Precedence {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let self_precedence = self.precedence();
        let other_precedence = other.precedence();
        match self_precedence + other_precedence {
            0 => Precedence::Low,
            1 => Precedence::Medium,
            _ => Precedence::High,
        }
    }
}

impl From<i64> for Precedence {
    fn from(value: i64) -> Self {
        match value {
            0 => Precedence::Low,
            1 => Precedence::Medium,
            _ => Precedence::High,
        }
    }
}

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

type AstResult = Result<AstNode, ParserError>;

pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, Vec<ParserError>> {
    let mut cursor = Cursor::new(tokens);
    let mut ast: Vec<AstNode> = vec![];

    while let Some(token) = cursor.next() {
        match token.token_type() {
            TokenType::Let => {
                // Syntax: let <identifier> = <expression>;
                if let Some(ident) =
                    cursor.next_if(|t| matches!(t.token_type(), TokenType::Identifier(_)))
                {
                    if cursor
                        .next_if(|t| matches!(t.token_type(), TokenType::Assign))
                        .is_some()
                    {
                        let expression = parse_expression(&mut cursor, Precedence::Low);
                        cursor.next_if(|t| matches!(t.token_type(), TokenType::SemiColon));
                        // parse the expression;
                        ast.push(AstNode::Assignment {
                            ident: ident.token_type().to_string(),
                            expression: Box::new(expression.unwrap()),
                        })
                    }
                }
            }
            _ => todo!("Unexpected token: {:?}", token),
        }
    }
    Ok(ast)
}

fn parse_expression(tokens: &mut Cursor<Token>, min_precedence: Precedence) -> AstResult {
    let mut lhs = parse_primary(tokens)?;
    parse_expression1(tokens, lhs, min_precedence)
}

fn parse_expression1(
    tokens: &mut Cursor<Token>,
    mut lhs: AstNode,
    min_precedence: Precedence,
) -> AstResult {
    while let Some(token) = tokens.get() {
        if *token.token_type() == TokenType::SemiColon {
            break;
        }

        if token_precedence(token.token_type()).is_none() {
            panic!("Unexpected token: {:?}, expected an operator", token);
        }

        let op = tokens.next().unwrap();
        let lhs_precedence = token_precedence(op.token_type()).unwrap();

        if lhs_precedence < min_precedence {
            // if the precedence is less than the minimum precedence, break
            break;
        }

        // parse primary will also move the cursor
        let mut rhs = parse_primary(tokens)?;

        while let Some(token) = tokens.get() {
            if *token.token_type() == TokenType::SemiColon {
                break;
            }

            if token_precedence(token.token_type()).is_none() {
                panic!("Unexpected token: {:?}, expected an operator", token);
            }

            let precedence = token_precedence(token.token_type()).unwrap();

            if precedence > lhs_precedence {
                rhs = parse_expression1(tokens, rhs, precedence)?;
            } else {
                break;
            }
        }

        lhs = match op.token_type() {
            TokenType::Add => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Addition,
            },
            TokenType::Multiply => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Multiplication,
            },
            TokenType::Minus => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Subtraction,
            },
            TokenType::Divide => AstNode::Arithmetic {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                arithmetic_type: ArithmeticType::Division,
            },
            _ => panic!("Unexpected token: {:?}", op),
        };
    }

    Ok(lhs)
}

fn parse_primary(tokens: &mut Cursor<Token>) -> AstResult {
    let x = tokens.next().ok_or_else(|| {
        ParserError::ExpectedExpression("Expected expression got `None` when parse_primary")
    })?;
    match x.token_type() {
        TokenType::Number(n) => Ok(AstNode::Number(*n)),
        TokenType::StringLiteral(s) => Ok(AstNode::StringLiteral(s.to_string())),
        TokenType::Identifier(ident) => Ok(AstNode::Identifier {
            ident: ident.to_string(),
        }),
        _ => panic!("Unexpected token: {:?}", x),
    }
}

/// returns a Some(_) varient if token type is a binary operator
fn token_precedence(token: &TokenType) -> Option<Precedence> {
    match token {
        TokenType::Add | TokenType::Minus => Some(Precedence::Low),
        TokenType::Divide | TokenType::Multiply => Some(Precedence::Medium),
        _ => None,
    }
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

    #[test]
    fn test_simple_arithmetic() {
        let input = String::from("let x = 10 + 20;");
        let lexer = Lexer::from(input);
        let tokens = lexer.collect::<Vec<_>>();
        let program = ast::parse(tokens);
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
        let program = ast::parse(tokens);
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
        let program = ast::parse(tokens);
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
