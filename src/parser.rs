use core::fmt;

use crate::ast::{Expression, Ident, InfixOperator, Int, PrefixOperator, Program, Statement};
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

    fn token_to_precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        Parser::token_to_precedence(&self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        Parser::token_to_precedence(&self.cur_token)
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

    // <Expression> -> [Ident] | [Int] | <Prefix Operator> <Expression> | <Expression> <Infix operator> <Expression>
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
        let mut left_expression = match self.cur_token {
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

        // NOTE: self.peek_token != Token::Semicolon という条件は果たしているのか？ -> 多分いらない
        // while self.cur_token != Token::Semicolon && precedence < self.cur_precedence() {
        while precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Lt
                | Token::Gt
                | Token::Eq
                | Token::NotEq => {
                    self.next_token();
                    left_expression = self.parse_infix_expression_pratt(left_expression)?;
                }
                _ => return Ok(left_expression),
            }
        }

        Ok(left_expression)
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
            _ => unreachable!("prefix"),
        };

        self.next_token();

        Ok(Expression::PrefixExpression(
            prefix_operator,
            Box::new(self.parse_expression_pratt(Precedence::Prefix)?),
        ))
    }

    pub fn parse_infix_expression_pratt(
        &mut self,
        left_expression: Expression,
    ) -> Result<Expression, ParseError> {
        // NOTE: ll1 解析と同じようにやっても良い気がする
        let infix_operator = match self.cur_token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Asterisk,
            Token::Slash => InfixOperator::Slash,
            Token::Lt => InfixOperator::Lt,
            Token::Gt => InfixOperator::Gt,
            Token::Eq => InfixOperator::Eq,
            Token::NotEq => InfixOperator::NotEq,
            _ => unreachable!("infix"),
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right_expression = self.parse_expression_pratt(precedence)?;

        Ok(Expression::InfixExpression(
            Box::new(left_expression),
            infix_operator,
            Box::new(right_expression),
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
    use crate::ast::{Expression, Ident, InfixOperator, Int, PrefixOperator, Statement};
    use crate::lexer::{self, Lexer};

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

    // p.56
    #[test]
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

    // p.60
    #[test]
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

    // p.63
    #[test]
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

    // p.69
    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            (
                "5 + 5;",
                Expression::Int(Int(5)),
                InfixOperator::Plus,
                Expression::Int(Int(5)),
            ),
            (
                "5 - 5;",
                Expression::Int(Int(5)),
                InfixOperator::Minus,
                Expression::Int(Int(5)),
            ),
            (
                "5 * 5;",
                Expression::Int(Int(5)),
                InfixOperator::Asterisk,
                Expression::Int(Int(5)),
            ),
            (
                "5 / 5;",
                Expression::Int(Int(5)),
                InfixOperator::Slash,
                Expression::Int(Int(5)),
            ),
            (
                "5 > 5;",
                Expression::Int(Int(5)),
                InfixOperator::Gt,
                Expression::Int(Int(5)),
            ),
            (
                "5 < 5;",
                Expression::Int(Int(5)),
                InfixOperator::Lt,
                Expression::Int(Int(5)),
            ),
            (
                "5 == 5;",
                Expression::Int(Int(5)),
                InfixOperator::Eq,
                Expression::Int(Int(5)),
            ),
            (
                "5 != 5;",
                Expression::Int(Int(5)),
                InfixOperator::NotEq,
                Expression::Int(Int(5)),
            ),
        ];

        for (input, expected_left_expression, expected_infix_operator, expected_right_expression) in
            infix_tests.iter()
        {
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

            let (left_expression, infix_operator, right_expression) = match expression {
                Expression::InfixExpression(left_expression, infix_operator, right_expression) => {
                    (left_expression, infix_operator, right_expression)
                }
                _ => {
                    panic!("{} is not infix expression", expression);
                }
            };

            assert_eq!(*expected_left_expression, *left_expression);
            assert_eq!(*expected_infix_operator, infix_operator);
            assert_eq!(*expected_right_expression, *right_expression);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b;", "((-a) * b);"),
            ("!-a;", "(!(-a));"),
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -6 * 5;", "(3 + 4);((-6) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
        ];

        for (input, expected) in tests {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);

            let program = p.parse_program().unwrap();

            let actual = program.to_string();

            assert_eq!(actual, expected);
        }
    }
}
