pub mod Parser {
    use std::ops::Add;

    use crate::{
        ast::ArithmeticType,
        ast::AstNode,
        error::ParserError,
        lexer::{Token, TokenType},
        peeker::Cursor,
    };

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

    impl From<u64> for Precedence {
        fn from(value: u64) -> Self {
            match value {
                0 => Precedence::Low,
                1 => Precedence::Medium,
                _ => Precedence::High,
            }
        }
    }

    type AstResult = Result<AstNode, ParserError>;

    pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<AstNode>, Vec<ParserError>> {
        let mut cursor = Cursor::new(tokens);
        let mut ast: Vec<AstNode> = vec![];

        while let Some(token) = cursor.next() {
            match token.token_type() {
                TokenType::Return => {
                    // Syntax: return <expression>;
                    if let Some(expression) = parse_expression(&mut cursor, Precedence::Low).ok() {
                        ast.push(AstNode::Return {
                            expression: Box::new(expression),
                        });
                    } else {
                        panic!(
                            "{}:{}: Expected expression after `return` at ",
                            token.line_nbr(),
                            token.char_nbr(),
                        );
                    }
                }
                TokenType::Function => {
                    // Syntax: fn <ident>(args0..*) : <type> { <body> }
                    // TODO: Implement function parsing
                }
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
            TokenType::Identifier(ident) => {
                if tokens
                    .next_if(|t| *t.token_type() == TokenType::RightParen)
                    .is_some()
                {
                    // Possible function call
                    // have to get all the values
                    todo!("Function Arguments")
                } else {
                    Ok(AstNode::Identifier {
                        ident: ident.to_string(),
                    })
                }
            }
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
        use super::parse;
        use super::*;
        use crate::ast::ArithmeticType;
        use crate::ast::AstNode;
        use crate::lexer;
        use crate::lexer::Lexer;
        use crate::lexer::TokenType;
        #[test]
        fn test_function_call() {
            let input = String::from("let x = 1 + choose(1, 2);");
            let tokens = lexer::Lexer::new(input).collect();
            let ast = parse(tokens).unwrap();
            let expected = vec![AstNode::Assignment {
                ident: "x".to_string(),
                expression: Box::new(AstNode::Arithmetic {
                    lhs: Box::new(AstNode::Number(1)),
                    rhs: Box::new(AstNode::FunctionCall(vec![
                        Box::new(AstNode::Number(1)),
                        Box::new(AstNode::Number(2)),
                    ])),
                    arithmetic_type: ArithmeticType::Addition,
                }),
            }];
        }
    }
}
