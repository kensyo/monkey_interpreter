use core::fmt;
use std::collections::HashMap;

use crate::ast::{Expression, Ident, Int, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

pub type PrefixParseFn = fn() -> Expression;
pub type InfixParseFn = fn(Expression) -> Expression;

pub struct Parser<'a> {
    l: &'a mut Lexer<'a>,

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer<'a>) -> Self {
        let cur_token = l.next_token();
        let peek_token = l.next_token();

        Parser {
            l,
            cur_token,
            peek_token,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn register_prefix(&mut self, token: Token, f: PrefixParseFn) {
        // upsert
        self.prefix_parse_fns
            .entry(token)
            .and_modify(|v| *v = f)
            .or_insert(f);
    }

    pub fn register_infix(&mut self, token: Token, f: InfixParseFn) {
        // upsert
        self.infix_parse_fns
            .entry(token)
            .and_modify(|v| *v = f)
            .or_insert(f);
    }

    // <Program> -> { <Statement> } EOF
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        match self.cur_token {
            // <Program> -> { <Statement> } EOF の Director
            Token::Let | Token::EOF | Token::Return => {
                // T({ <Statement> })
                let mut statements = vec![];
                // <Statement> の First
                while matches!(self.cur_token, Token::Let | Token::Return) {
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

    // <Statement> -> let [Ident] = <Expression> ; | return <Expression> ;
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

            // <Program> -> return <Expression> ; の Director
            Token::Return => {
                // T(return)
                self.parse_token(Token::Return)?;

                // T(<Expression>)
                let expression = self.parse_expression()?;

                // T(;)
                self.parse_token(Token::Semicolon)?;

                Ok(Statement::Return(expression))
            }

            _ => Err(ParseError::Symbol {
                symbol: "<statement>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Expression> -> [Ident] | [Int]
    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // <Expression> -> [Ident] | [Int] の Director
        match self.cur_token {
            // <Expression> -> [Ident] の Director
            Token::Ident(_) => {
                let ident = self.parse_ident_token()?;
                Ok(Expression::Ident(ident))
            }
            // <Expression> -> [Int] の Director
            Token::Int(_) => {
                let int = self.parse_int_token()?;
                Ok(Expression::Int(int))
            }

            _ => Err(ParseError::Symbol {
                symbol: "<expression>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
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
}

#[derive(Debug)]
pub enum ParseError {
    Symbol {
        symbol: String,
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, Ident, Int, Statement};
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
}
