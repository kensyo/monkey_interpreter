use core::fmt;

use crate::ast::{Expression, Ident, Int, PrefixOperator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

pub type PrefixParseFn = fn() -> Result<Expression, ParseError>;
pub type InfixParseFn = fn(Expression) -> Result<Expression, ParseError>;

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
}

pub struct Parser<'a> {
    l: &'a mut Lexer<'a>,

    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer<'a>) -> Self {
        let cur_token = l.next_token();
        let peek_token = l.next_token();

        Parser {
            l,
            cur_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    // <Program> -> { <Statement> } EOF
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        match self.cur_token {
            // <Program> -> { <Statement> } EOF の Director
            Token::Let
            | Token::EOF
            | Token::Return
            | Token::Ident(_)
            | Token::Int(_)
            | Token::Bang
            | Token::Minus => {
                // T({ <Statement> })
                let mut statements = vec![];
                // <Statement> の First
                while matches!(
                    self.cur_token,
                    Token::Let
                        | Token::Return
                        | Token::Ident(_)
                        | Token::Int(_)
                        | Token::Bang
                        | Token::Minus
                ) {
                    // T(<Statement>)
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }

                // T(EOF)
                self.parse_token(Token::EOF)?;

                Ok(Program::Program(statements))
            }

            _ => Err(ParseError::Symbol {
                symbol: "<program>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Statement> -> let [Ident] = <Expression> ; | return <Expression> ; | <Expression> ;
    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token {
            // <Statement> -> let [Ident] = <Expression> ; の Director
            Token::Let => {
                // T(let)
                self.parse_token(Token::Let)?;

                // T([ident])
                let ident = self.parse_ident_token()?;

                // T(=)
                self.parse_token(Token::Assign)?;

                // T(<Expression>)
                let expression = self.parse_expression()?;

                // T(;)
                self.parse_token(Token::Semicolon)?;

                Ok(Statement::Let(ident, expression))
            }

            // <Statement> -> return <Expression> ; の Director
            Token::Return => {
                // T(return)
                self.parse_token(Token::Return)?;

                // T(<Expression>)
                let expression = self.parse_expression()?;

                // T(;)
                self.parse_token(Token::Semicolon)?;

                Ok(Statement::Return(expression))
            }

            // <Statement> -> <Expression> ; の Director
            Token::Int(_) | Token::Ident(_) | Token::Bang | Token::Minus => {
                // T(<Expression>)
                let expression = self.parse_expression()?;

                // T(;)
                self.parse_token(Token::Semicolon)?;

                Ok(Statement::Expression(expression))
            }

            _ => Err(ParseError::Symbol {
                symbol: "<statement>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Expression> -> [Ident] | [Int] | <Prefix Operator> <Expression>
    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let expression = self.parse_expression_pratt(Precedence::Lowest)?;

        // pratt パーシングではその中で next_token を呼ばないのでここで呼んでおく
        self.next_token();
        Ok(expression)
    }

    pub fn parse_expression_pratt(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        // prefix
        let left_expression = match self.cur_token {
            Token::Ident(_) => self.parse_ident_pratt()?,
            Token::Int(_) => self.parse_int_pratt()?,
            Token::Bang => self.parse_prefix_expression_pratt()?,
            Token::Minus => self.parse_prefix_expression_pratt()?,

            _ => {
                return Err(ParseError::PrattPrefix {
                    current_token: self.cur_token.clone(),
                })
            }
        };

        Ok(left_expression)

        // // <Expression> -> [Ident] | [Int] の Director
        // match self.cur_token {
        //     // <Expression> -> [Ident] の Director
        //     Token::Ident(_) => {
        //         let ident = self.parse_ident_token()?;
        //         Ok(Expression::Ident(ident))
        //     }
        //     // <Expression> -> [Int] の Director
        //     Token::Int(_) => {
        //         let int = self.parse_int_token()?;
        //         Ok(Expression::Int(int))
        //     }
        //
        //     _ => Err(ParseError::Symbol {
        //         symbol: "<expression>".to_string(),
        //         current_token: self.cur_token.clone(),
        //     }),
        // }
    }

    pub fn parse_token(&mut self, expected_token: Token) -> Result<(), ParseError> {
        if self.cur_token.is_same_variant(&expected_token) {
            self.next_token();
            Ok(())
        } else {
            return Err(ParseError::Symbol {
                symbol: format!("{:?}", expected_token.clone()),
                current_token: self.cur_token.clone(),
            });
        }
    }

    pub fn parse_ident_token(&mut self) -> Result<Ident, ParseError> {
        if let Token::Ident(ref val) = self.cur_token {
            let str = val.clone();
            self.next_token();
            Ok(Ident(str))
        } else {
            return Err(ParseError::Symbol {
                symbol: "[ident]".to_string(),
                current_token: self.cur_token.clone(),
            });
        }
    }

    pub fn parse_int_token(&mut self) -> Result<Int, ParseError> {
        if let Token::Int(ref val) = self.cur_token {
            let num = val.clone();
            self.next_token();
            Ok(Int(num))
        } else {
            return Err(ParseError::Symbol {
                symbol: "[int]".to_string(),
                current_token: self.cur_token.clone(),
            });
        }
    }

    pub fn parse_ident_pratt(&mut self) -> Result<Expression, ParseError> {
        if let Token::Ident(ref val) = self.cur_token {
            let str = val.clone();
            // self.next_token();
            Ok(Expression::Ident(Ident(str)))
        } else {
            return Err(ParseError::Symbol {
                symbol: "[ident](pratt)".to_string(),
                current_token: self.cur_token.clone(),
            });
        }
    }

    pub fn parse_int_pratt(&mut self) -> Result<Expression, ParseError> {
        if let Token::Int(ref val) = self.cur_token {
            let num = val.clone();
            // self.next_token();
            Ok(Expression::Int(Int(num)))
        } else {
            return Err(ParseError::Symbol {
                symbol: "[int](pratt)".to_string(),
                current_token: self.cur_token.clone(),
            });
        }
    }

    pub fn parse_prefix_expression_pratt(&mut self) -> Result<Expression, ParseError> {
        // NOTE: ll1 解析と同じようにやっても良い気がする
        let prefix_operator = match self.cur_token {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            _ => unreachable!(),
        };

        self.next_token();

        Ok(Expression::PrefixExpression(
            prefix_operator,
            Box::new(self.parse_expression_pratt(Precedence::Prefix)?),
        ))
    }
}

#[derive(Debug)]
pub enum ParseError {
    Symbol {
        symbol: String,
        current_token: Token,
    },
    PrattPrefix {
        current_token: Token,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Symbol {
                symbol,
                current_token,
            } => {
                write!(
                    f,
                    "Error: An inconsistency was detected while executing the parsing function for the symbol {}. Encountered unexpected token: {:?}.",
                    symbol,
                    current_token
                )
            }
            ParseError::PrattPrefix { current_token } => {
                write!(
                    f,
                    "Error: No corresponding prefix parse function for token {:?}.",
                    current_token
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, Ident, Int, PrefixOperator, Statement};
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = x;
let y = y;
let foobar = foobar;
"#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

        // parse_program がエラーを返していたらパニックして終了
        let program = p.parse_program().unwrap();

        let actual_statements = match program {
            Program::Program(s_s) => {
                assert_eq!(s_s.len(), 3);
                s_s
            }
        };

        // Expression は一旦適当
        let expected_statements = vec![
            Statement::Let(
                Ident("x".to_string()),
                Expression::Ident(Ident("x".to_string())),
            ),
            Statement::Let(
                Ident("y".to_string()),
                Expression::Ident(Ident("y".to_string())),
            ),
            Statement::Let(
                Ident("foobar".to_string()),
                Expression::Ident(Ident("foobar".to_string())),
            ),
        ];

        assert_eq!(actual_statements, expected_statements);
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
"#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

        // parse_program がエラーを返していたらパニックして終了
        let program = p.parse_program().unwrap();

        let actual_statements = match program {
            Program::Program(s_s) => {
                assert_eq!(s_s.len(), 3);
                s_s
            }
        };

        // Expression は一旦適当
        let expected_statements = vec![
            Statement::Return(Expression::Int(Int(5))),
            Statement::Return(Expression::Int(Int(10))),
            Statement::Return(Expression::Int(Int(993322))),
        ];

        assert_eq!(actual_statements, expected_statements);
    }

    #[test]
    // p.56
    fn test_identifier_expression() {
        let input = r#"
foobar;
"#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

        // parse_program がエラーを返していたらパニックして終了
        let program = p.parse_program().unwrap();

        let statements = match program {
            Program::Program(s_s) => {
                assert_eq!(s_s.len(), 1);
                s_s
            }
        };

        let statement = statements.into_iter().next().unwrap();

        let expression = match statement {
            Statement::Expression(expr) => expr,
            _ => {
                panic!("{} is not expression statement", statement);
            }
        };

        let ident = match expression {
            Expression::Ident(ident) => ident,
            _ => {
                panic!("{} is not ident expression", expression);
            }
        };

        assert_eq!(ident.0, "foobar");
    }

    #[test]
    // p.60
    fn test_integral_literal_expression() {
        let input = r#"
5;
"#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

        // parse_program がエラーを返していたらパニックして終了
        let program = p.parse_program().unwrap();

        let statements = match program {
            Program::Program(s_s) => {
                assert_eq!(s_s.len(), 1);
                s_s
            }
        };

        let statement = statements.into_iter().next().unwrap();

        let expression = match statement {
            Statement::Expression(expr) => expr,
            _ => {
                panic!("{} is not expression statement", statement);
            }
        };

        let int = match expression {
            Expression::Int(int) => int,
            _ => {
                panic!("{} is not int expression", expression);
            }
        };

        assert_eq!(int.0, 5);
    }

    #[test]
    // p.63
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", PrefixOperator::Bang, Expression::Int(Int(5))),
            ("-15;", PrefixOperator::Minus, Expression::Int(Int(15))),
        ];

        for (input, expected_prefix_operator, expected_expression) in prefix_tests.iter() {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);

            // parse_program がエラーを返していたらパニックして終了
            let program = p.parse_program().unwrap();

            let statements = match program {
                Program::Program(s_s) => {
                    assert_eq!(s_s.len(), 1);
                    s_s
                }
            };

            let statement = statements.into_iter().next().unwrap();

            let expression = match statement {
                Statement::Expression(expr) => expr,
                _ => {
                    panic!("{} is not expression statement", statement);
                }
            };

            let (prefix_operator, expression2) = match expression {
                Expression::PrefixExpression(prefix_operator, expression) => {
                    (prefix_operator, expression)
                }
                _ => {
                    panic!("{} is not prefix expression", expression);
                }
            };

            assert_eq!(*expected_prefix_operator, prefix_operator);
            assert_eq!(*expected_expression, *expression2);
        }
    }
}
