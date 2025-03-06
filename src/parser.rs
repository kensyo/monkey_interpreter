use core::fmt;

use crate::ast::{Expression, Ident, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

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

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut statements = vec![];

        match self.cur_token {
            // <Program> -> { <Statement> } EOF の Director
            Token::Let | Token::EOF => {
                // T({ <Statement> })
                while self.cur_token == Token::Let {
                    // T(<Statement>)
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }

                // T(EOF)
                match self.cur_token {
                    Token::EOF => {
                        self.next_token();
                    }
                    _ => {
                        return Err(ParserError::new("EOF", self.cur_token.clone()));
                    }
                }

                Ok(Program::Program(statements))
            }

            _ => Err(ParserError::new("<program>", self.cur_token.clone())),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur_token {
            // <Statement> -> let [Ident] = <Expression>; の Director
            Token::Let => {
                // T(let)
                match self.cur_token {
                    Token::Let => {
                        self.next_token();
                    }
                    _ => {
                        return Err(ParserError::new("let", self.cur_token.clone()));
                    }
                }

                // T([ident])
                let ident = match self.cur_token {
                    Token::Ident(ref ident) => {
                        let tmp = ident.clone();
                        self.next_token();
                        tmp
                    }
                    _ => {
                        return Err(ParserError::new("[ident]", self.cur_token.clone()));
                    }
                };

                // T(=)
                match self.cur_token {
                    Token::Assign => {
                        self.next_token();
                    }
                    _ => {
                        return Err(ParserError::new("assign", self.cur_token.clone()));
                    }
                }

                // T(<Expression>)
                let expression = self.parse_expression()?;

                // T(;)
                match self.cur_token {
                    Token::Semicolon => {
                        self.next_token();
                    }
                    _ => {
                        return Err(ParserError::new("semicolon", self.cur_token.clone()));
                    }
                }

                Ok(Statement::Let(Ident(ident), expression))
            }

            _ => Err(ParserError::new("statement", self.cur_token.clone())),
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        // <Expression> -> [Ident]
        let ident = match self.cur_token {
            Token::Ident(ref ident) => {
                let tmp = ident.clone();
                self.next_token();
                tmp
            }
            _ => {
                return Err(ParserError::new("[ident]", self.cur_token.clone()));
            }
        };

        return Ok(Expression::Ident(Ident(ident)));
    }
}

#[derive(Debug)]
pub struct ParserError {
    target_symbol: &'static str,
    current_token: Token,
}

impl ParserError {
    pub fn new(target_symbol: &'static str, current_token: Token) -> Self {
        Self {
            target_symbol,
            current_token,
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: An inconsistency was detected while executing the parsing function for the symbol {:?}. Encountered unexpected token: {:?}.",
            self.target_symbol,
            self.current_token
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, Ident, Statement};
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
}
