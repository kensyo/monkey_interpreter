use core::fmt;

use crate::ast::{
    BlockStatement, Boolean, CommaSeparatedExpression, Expression, Ident, InfixOperator, Int,
    Parameters, PrefixOperator, Program, Statement,
};
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

    // 前置演算子の優先度
    fn token_to_precedence_prefix(token: &Token) -> Precedence {
        match token {
            Token::Bang | Token::Minus => Precedence::Prefix,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence_prefix(&self) -> Precedence {
        Parser::token_to_precedence_prefix(&self.peek_token)
    }

    fn cur_precedence_prefix(&self) -> Precedence {
        Parser::token_to_precedence_prefix(&self.cur_token)
    }

    // 中置演算子の優先度
    fn token_to_precedence_infix(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence_infix(&self) -> Precedence {
        Parser::token_to_precedence_infix(&self.peek_token)
    }

    fn cur_precedence_infix(&self) -> Precedence {
        Parser::token_to_precedence_infix(&self.cur_token)
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
            | Token::Minus
            | Token::True
            | Token::False
            | Token::LParen
            | Token::If
            | Token::Function => {
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
                        | Token::True
                        | Token::False
                        | Token::LParen
                        | Token::If
                        | Token::Function
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
            Token::Int(_)
            | Token::Ident(_)
            | Token::Bang
            | Token::Minus
            | Token::True
            | Token::False
            | Token::LParen
            | Token::If
            | Token::Function => {
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

    // <Expression> -> [Ident]
    //                   | [Int]
    //                   | <Prefix operator> <Expression>
    //                   | <Expression> <Infix operator> <Expression>
    //                   | <Boolean>
    //                   | ( <Expression> )
    //                   | if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε )
    //                   | fn <Parameters> <Block statement>
    //                   | <Expression> '(' <Comma separated expression> ')'
    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let expression = self.parse_expression_pratt(Precedence::Lowest)?;

        // pratt パーシングではその中で next_token を呼ばないのでここで呼んでおく
        // self.next_token();
        Ok(expression)
    }

    // <Prefix operator> -> ! | -
    pub fn parse_prefix_operator(&mut self) -> Result<PrefixOperator, ParseError> {
        match self.cur_token {
            // <Prefix operator> -> ! の Director
            Token::Bang => {
                self.parse_token(Token::Bang)?;

                Ok(PrefixOperator::Bang)
            }
            // <Prefix operator> -> - の Director
            Token::Minus => {
                self.parse_token(Token::Minus)?;

                Ok(PrefixOperator::Minus)
            }

            _ => Err(ParseError::Symbol {
                symbol: "<prefix operator>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Infix operator> -> + | - | * | / | > | < | == | !=
    pub fn parse_infix_operator(&mut self) -> Result<InfixOperator, ParseError> {
        match self.cur_token {
            // <Infix operator> -> + の Director
            Token::Plus => {
                // T(+)
                self.parse_token(Token::Plus)?;

                Ok(InfixOperator::Plus)
            }

            // <Infix operator> -> - の Director
            Token::Minus => {
                // T(-)
                self.parse_token(Token::Minus)?;

                Ok(InfixOperator::Minus)
            }

            // <Infix operator> -> * の Director
            Token::Asterisk => {
                // T(*)
                self.parse_token(Token::Asterisk)?;

                Ok(InfixOperator::Asterisk)
            }

            // <Infix operator> -> / の Director
            Token::Slash => {
                // T(/)
                self.parse_token(Token::Slash)?;

                Ok(InfixOperator::Slash)
            }

            // <Infix operator> -> > の Director
            Token::Gt => {
                // T(>)
                self.parse_token(Token::Gt)?;

                Ok(InfixOperator::Gt)
            }

            // <Infix operator> -> < の Director
            Token::Lt => {
                // T(<)
                self.parse_token(Token::Lt)?;

                Ok(InfixOperator::Lt)
            }

            // <Infix operator> -> == の Director
            Token::Eq => {
                // T(==)
                self.parse_token(Token::Eq)?;

                Ok(InfixOperator::Eq)
            }

            // <Infix operator> -> != の Director
            Token::NotEq => {
                // T(!=)
                self.parse_token(Token::NotEq)?;

                Ok(InfixOperator::NotEq)
            }

            _ => Err(ParseError::Symbol {
                symbol: "<infix operator>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Boolean> -> true | false
    pub fn parse_boolean(&mut self) -> Result<Boolean, ParseError> {
        match self.cur_token {
            // <Boolean> -> true の Director
            Token::True => {
                // T(true
                self.parse_token(Token::True)?;

                Ok(Boolean::True)
            }

            // <Boolean> -> false の Director
            Token::False => {
                // T(true
                self.parse_token(Token::False)?;

                Ok(Boolean::False)
            }

            _ => Err(ParseError::Symbol {
                symbol: "<boolean>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Block statement> -> '{' { <Statement> } '}'
    pub fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        match self.cur_token {
            // <Block statement> -> '{' { <Statement> } '}' の Director
            Token::LBrace => {
                // T( '{' )
                self.parse_token(Token::LBrace)?;

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
                        | Token::True
                        | Token::False
                        | Token::LParen
                        | Token::If
                ) {
                    // T(<Statement>)
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }

                // T( '}' )
                self.parse_token(Token::RBrace)?;

                Ok(BlockStatement::BlockStatement(statements))
            }

            _ => Err(ParseError::Symbol {
                symbol: "<block statement>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Parameters> -> '(' ( [ident] { , [ident] } | ε ) ')'
    pub fn parse_parameters(&mut self) -> Result<Parameters, ParseError> {
        match self.cur_token {
            // <Parameters> -> '(' ( [ident] { , [ident] } | ε ) ')' の Director
            Token::LParen => {
                // T('(')
                self.parse_token(Token::LParen)?;

                // T( [ident] { , [ident] } | ε )
                let mut parameters = vec![];
                //  [ident] { , [ident] } の First
                if matches!(self.cur_token, Token::Ident(_)) {
                    // T([ident])
                    let ident = self.parse_ident_token()?;
                    parameters.push(ident);

                    // T({ , [ident] })
                    // , [ident] の First
                    while matches!(self.cur_token, Token::Comma) {
                        // T(,)
                        self.parse_token(Token::Comma)?;

                        // T([ident])
                        let ident = self.parse_ident_token()?;
                        parameters.push(ident);
                    }
                }
                // ε の First は空なので false にしておく
                else if false {
                    // T(ε)
                    // 何もしない
                }

                // T( ')' )
                self.parse_token(Token::RParen)?;

                Ok(Parameters::Parameters(parameters))
            }
            _ => Err(ParseError::Symbol {
                symbol: "<parameters>".to_string(),
                current_token: self.cur_token.clone(),
            }),
        }
    }

    // <Comma separated expression> -> ( <Expression> { , <Expression> } | ε )
    pub fn parse_comma_separated_expressions(
        &mut self,
    ) -> Result<CommaSeparatedExpression, ParseError> {
        match self.cur_token {
            // <Comma separated expression> -> ( <Expression> { , <Expression> } | ε ) の Director
            Token::True
            | Token::False
            | Token::LParen
            | Token::Function
            | Token::If
            | Token::Int(_)
            | Token::Minus
            | Token::Bang
            | Token::Ident(_)
            | Token::RParen => {
                // T( <Expression> { , <Expression> } | ε )
                let mut arguments = vec![];
                // <Expression> { , <Expression> } の First
                if matches!(
                    self.cur_token,
                    Token::True
                        | Token::False
                        | Token::LParen
                        | Token::Function
                        | Token::If
                        | Token::Int(_)
                        | Token::Minus
                        | Token::Bang
                        | Token::Ident(_)
                ) {
                    // T(<Expression>)
                    let expression = self.parse_expression()?;
                    arguments.push(expression);

                    // T({ , <Expression> })
                    // , <Expression> の First
                    while matches!(self.cur_token, Token::Comma) {
                        // T(,)
                        self.parse_token(Token::Comma)?;

                        // T(<Expression>)
                        let expression = self.parse_expression()?;
                        arguments.push(expression);
                    }
                }
                // ε の First は空なので false にしておく
                else if false {
                    // T(ε)
                    // 何もしない
                }

                Ok(CommaSeparatedExpression::CommaSeparatedExpression(
                    arguments,
                ))
            }
            _ => Err(ParseError::Symbol {
                symbol: "<comma separated expressions>".to_string(),
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

    // 以下 pratt 構文解析用のメソッド
    // pratt 構文解析関数(これは <Expression> -> A1 | ... | An としたとき、n個作ることになるだろう)
    // の返り値は全て Expression であることに注意
    // 中核である parse_expression_pratt, parse_infix_expression_pratt, parse_prefix_expression_pratt
    // 以外の pratt 構文解析メソッドは ll(1) 解析と似ている(返り値がExpressionというだけ)
    //
    pub fn parse_expression_pratt(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        // prefix
        let mut left_expression = match self.cur_token {
            // NOTE: cur_token だけを使う場合、pratt パースはここの First たちが互いに素じゃないと
            // うまくいかないだろう(peek_token まで使って場合分けができればいけるとは思う)
            // NOTE: ここには <Expression> -> <Expression> ...  という形でない規則
            // (LHS が <Expression> でかつ、RHS の最初の記号が<Expression> でないもの) の First とそれに対応する
            // pratt 構文解析関数 を書く

            // <Expression> -> [Ident] ... ( [Ident] の First )
            Token::Ident(_) => self.parse_ident_pratt()?,
            // <Expression> -> [Int] ... ( [Int] の First )
            Token::Int(_) => self.parse_int_pratt()?,
            // <Expression> -> <Boolean> ... ( <Boolean> の First )
            Token::True | Token::False => self.parse_boolean_pratt()?,
            // <Expression> -> '(' <Expression> ')' ... ( '(' <Expression> ')' の First )
            Token::LParen => self.parse_grouped_expression_pratt()?,
            // <Expression> -> if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε ) ... ( if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε ) の First)
            Token::If => self.parse_if_expression_pratt()?,
            // <Expression> -> fn <Parameters> <Block statement> ... (  の First )
            Token::Function => self.parse_function_literal_partt()?,

            // 前置演算子
            // <Expression> -> <Prefix operator> <Expression> ... ( <Prefix operator> <Expression> の First )
            Token::Bang | Token::Minus => self.parse_prefix_expression_pratt()?,

            _ => {
                return Err(ParseError::PrattPrefix {
                    current_token: self.cur_token.clone(),
                })
            }
        };

        // while self.cur_token != Token::Semicolon && precedence < self.cur_precedence() {
        // NOTE1: self.peek_token != Token::Semicolon という条件は果たしているのか？ -> 多分いらない
        // NOTE2: self.cur_precedece() は今のトークンが中置演算子以外だとLowestを
        // 返すようになっているので、その場合 while の中身は実行されなくなる
        // NOTE3: < を <= に変えると同じ優先度の演算子が右結合になる(と思われる)
        //
        while precedence < self.cur_precedence_infix() {
            match self.cur_token {
                // 中置演算子
                // <Expression> -> <Expression> <Infix operator> <Expression>
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Lt
                | Token::Gt
                | Token::Eq
                | Token::NotEq => {
                    left_expression = self.parse_infix_expression_pratt(left_expression)?;
                }
                // <Expression> -> <Expression> ( <Comma separated expressions> )
                // NOTE: ( は中値演算子ではないが、しょうがなく優先度を設定して解析できるようにするのがコツ
                Token::LParen => {
                    left_expression = self.parse_call_expression_pratt(left_expression)?;
                }
                _ => return Ok(left_expression),
            }
        }

        Ok(left_expression)
    }

    // pratt parse for <Expression> -> [Ident]
    pub fn parse_ident_pratt(&mut self) -> Result<Expression, ParseError> {
        // T([Ident])
        let ident = self.parse_ident_token()?;
        Ok(Expression::Ident(ident))
    }

    // pratt parse for <Expression> -> [Int]
    pub fn parse_int_pratt(&mut self) -> Result<Expression, ParseError> {
        // T([Int])
        let int = self.parse_int_token()?;
        Ok(Expression::Int(int))
    }

    // pratt parse for <Expression> -> <Boolean>
    pub fn parse_boolean_pratt(&mut self) -> Result<Expression, ParseError> {
        // T(<Boolean>)
        let boolean = self.parse_boolean()?;
        Ok(Expression::Boolean(boolean))
    }

    // pratt parse for <Expression> -> '(' <Expression> ')'
    pub fn parse_grouped_expression_pratt(&mut self) -> Result<Expression, ParseError> {
        // T( '(' )
        self.parse_token(Token::LParen)?;

        // T(<Expression>)
        let expression = self.parse_expression()?;

        // T( ')' )
        self.parse_token(Token::RParen)?;

        Ok(Expression::GroupedExpression(Box::new(expression)))
    }

    // pratt parse for <Expression> -> if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε )
    pub fn parse_if_expression_pratt(&mut self) -> Result<Expression, ParseError> {
        // T(if)
        self.parse_token(Token::If)?;

        // T( '(' )
        self.parse_token(Token::LParen)?;

        // T(<Expression>)
        let condition = self.parse_expression()?;

        // T( ')' )
        self.parse_token(Token::RParen)?;

        // T(<Block statement>)
        let consequence = self.parse_block_statement()?;

        // T( else <Block statement> | ε )
        let mut alternative = None;
        // else <Block statement> の First
        if matches!(self.cur_token, Token::Else) {
            // T(else)
            self.parse_token(Token::Else)?;

            // T(<Block statement>)
            let alt = self.parse_block_statement()?;

            alternative = Some(alt);
        }
        // ε の First （これは空集合なので条件式は false ）
        else if false {
            // T(ε)
            // 何もしない
        };

        Ok(Expression::IfExpression(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    // pratt parse for <Expression> -> fn <Parameters> <Block statement>
    pub fn parse_function_literal_partt(&mut self) -> Result<Expression, ParseError> {
        // T(fn)
        self.parse_token(Token::Function)?;

        // T(<Parameters>)
        let parameters = self.parse_parameters()?;

        // T(<Block statement>)
        let block_statement = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral(parameters, block_statement))
    }

    // pratt parse for <Expression> -> <Expression> ( <Comma separated expressions> )
    pub fn parse_call_expression_pratt(
        &mut self,
        left_expression: Expression,
    ) -> Result<Expression, ParseError> {
        // T( '(' )
        self.parse_token(Token::LParen)?;

        // T(<Comma separated expressions>)
        let arguments = self.parse_comma_separated_expressions()?;

        // T( ')' )
        self.parse_token(Token::RParen)?;

        Ok(Expression::CallExpression(
            Box::new(left_expression),
            arguments,
        ))
    }

    // 以下は (前置 | 中置) 演算子用の pratt パース関数

    // pratt parse for <Expression> -> <Prefix Operator> <Expression>
    pub fn parse_prefix_expression_pratt(&mut self) -> Result<Expression, ParseError> {
        // <Prefix Operator>
        let precedence = self.cur_precedence_prefix();
        let prefix_operator = self.parse_prefix_operator()?;

        // <Expression>
        let expression = self.parse_expression_pratt(precedence)?;

        Ok(Expression::PrefixExpression(
            prefix_operator,
            Box::new(expression),
        ))
    }

    // pratt parse for <Expression> -> <Expression> <Infix operator> <Expression>
    pub fn parse_infix_expression_pratt(
        &mut self,
        left_expression: Expression,
    ) -> Result<Expression, ParseError> {
        // <Infix operator>
        let precedence = self.cur_precedence_infix();
        let infix_operator = self.parse_infix_operator()?;

        // <Expression>
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
    use crate::ast::{
        BlockStatement, Boolean, Expression, Ident, InfixOperator, Int, Parameters, PrefixOperator,
        Statement,
    };
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
            (
                "!true;",
                PrefixOperator::Bang,
                Expression::Boolean(Boolean::True),
            ),
            (
                "!false;",
                PrefixOperator::Bang,
                Expression::Boolean(Boolean::False),
            ),
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
            (
                "true == true;",
                Expression::Boolean(Boolean::True),
                InfixOperator::Eq,
                Expression::Boolean(Boolean::True),
            ),
            (
                "true != false;",
                Expression::Boolean(Boolean::True),
                InfixOperator::NotEq,
                Expression::Boolean(Boolean::False),
            ),
            (
                "false == false;",
                Expression::Boolean(Boolean::False),
                InfixOperator::Eq,
                Expression::Boolean(Boolean::False),
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
            ("true;", "true;"),
            ("false;", "false;"),
            ("3 > 5 == false;", "((3 > 5) == false);"),
            ("3 < 5 == true;", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("!(true == false);", "(!(true == false));"),
            ("(5);", "5;"),
            ("a + add(b * c) + d;", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d/ f + g);",
                "add((((a + b) + ((c * d) / f)) + g));",
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

    // p.88
    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", Boolean::True), ("false;", Boolean::False)];

        for (input, expected) in tests {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);

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

            let boolean = match expression {
                Expression::Boolean(boolean) => boolean,
                _ => {
                    panic!("{} is not boolean expression", expression);
                }
            };

            assert_eq!(boolean, expected);
        }
    }

    // p.95
    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x; };";

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

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

        let (condition, consequence, alternative) = match expression {
            Expression::IfExpression(condition, consequence, alternative) => {
                (condition, consequence, alternative)
            }
            _ => {
                panic!("{} is not if expression", expression);
            }
        };

        assert_eq!(
            *condition,
            Expression::InfixExpression(
                Box::new(Expression::Ident(Ident("x".to_string()))),
                InfixOperator::Lt,
                Box::new(Expression::Ident(Ident("y".to_string())))
            )
        );

        assert_eq!(
            consequence,
            BlockStatement::BlockStatement(vec![Statement::Expression(Expression::Ident(Ident(
                "x".to_string()
            )))])
        );

        assert_eq!(alternative, None);
    }

    // p.95
    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x; } else { y; };";

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

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

        let (condition, consequence, alternative) = match expression {
            Expression::IfExpression(condition, consequence, alternative) => {
                (condition, consequence, alternative)
            }
            _ => {
                panic!("{} is not if expression", expression);
            }
        };

        assert_eq!(
            *condition,
            Expression::InfixExpression(
                Box::new(Expression::Ident(Ident("x".to_string()))),
                InfixOperator::Lt,
                Box::new(Expression::Ident(Ident("y".to_string())))
            )
        );

        assert_eq!(
            consequence,
            BlockStatement::BlockStatement(vec![Statement::Expression(Expression::Ident(Ident(
                "x".to_string()
            )))])
        );

        let alt = alternative.unwrap();

        assert_eq!(
            alt,
            BlockStatement::BlockStatement(vec![Statement::Expression(Expression::Ident(Ident(
                "y".to_string()
            )))])
        );
    }

    // p.101
    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; };";

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

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

        let (parameters, body) = match expression {
            Expression::FunctionLiteral(parameters, body) => (parameters, body),
            _ => {
                panic!("{} is not function literal expression", expression);
            }
        };

        assert_eq!(
            parameters,
            Parameters::Parameters(vec![Ident("x".to_string()), Ident("y".to_string())])
        );

        assert_eq!(
            body,
            BlockStatement::BlockStatement(vec![Statement::Expression(
                Expression::InfixExpression(
                    Box::new(Expression::Ident(Ident("x".to_string()))),
                    InfixOperator::Plus,
                    Box::new(Expression::Ident(Ident("y".to_string())))
                )
            )])
        );
    }

    // p.104
    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", Parameters::Parameters(vec![])),
            (
                "fn(x) {};",
                Parameters::Parameters(vec![Ident("x".to_string())]),
            ),
            (
                "fn(x, y, z) {};",
                Parameters::Parameters(vec![
                    Ident("x".to_string()),
                    Ident("y".to_string()),
                    Ident("z".to_string()),
                ]),
            ),
        ];

        for (input, expected_params) in tests {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);

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

            let (parameters, _body) = match expression {
                Expression::FunctionLiteral(parameters, body) => (parameters, body),
                _ => {
                    panic!("{} is not function literal expression", expression);
                }
            };

            assert_eq!(parameters, expected_params);
        }
    }

    // p.106
    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

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

        let (function, arguments) = match expression {
            Expression::CallExpression(function, aruguments) => (function, aruguments),
            _ => {
                panic!("{} is not call expression", expression);
            }
        };

        assert_eq!(*function, Expression::Ident(Ident("add".to_string())));
        assert_eq!(
            arguments,
            CommaSeparatedExpression::CommaSeparatedExpression(vec![
                Expression::Int(Int(1)),
                Expression::InfixExpression(
                    Box::new(Expression::Int(Int(2))),
                    InfixOperator::Asterisk,
                    Box::new(Expression::Int(Int(3)))
                ),
                Expression::InfixExpression(
                    Box::new(Expression::Int(Int(4))),
                    InfixOperator::Plus,
                    Box::new(Expression::Int(Int(5)))
                ),
            ])
        );
    }

    // p.106
    #[test]
    fn test_call_expression_parameter_parsing() {
        let tests = vec![
            (
                "add();",
                Expression::Ident(Ident("add".to_string())),
                CommaSeparatedExpression::CommaSeparatedExpression(vec![]),
            ),
            (
                "add(1);",
                Expression::Ident(Ident("add".to_string())),
                CommaSeparatedExpression::CommaSeparatedExpression(vec![Expression::Int(Int(1))]),
            ),
            (
                "add(1, 2 * 3, 4 + 5);",
                Expression::Ident(Ident("add".to_string())),
                CommaSeparatedExpression::CommaSeparatedExpression(vec![
                    Expression::Int(Int(1)),
                    Expression::InfixExpression(
                        Box::new(Expression::Int(Int(2))),
                        InfixOperator::Asterisk,
                        Box::new(Expression::Int(Int(3))),
                    ),
                    Expression::InfixExpression(
                        Box::new(Expression::Int(Int(4))),
                        InfixOperator::Plus,
                        Box::new(Expression::Int(Int(5))),
                    ),
                ]),
            ),
        ];

        for (input, expected_function, expected_aruguments) in tests.iter() {
            let mut l = Lexer::new(input);
            let mut p = Parser::new(&mut l);

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

            let (function, arguments) = match expression {
                Expression::CallExpression(function, aruguments) => (function, aruguments),
                _ => {
                    panic!("{} is not call expression", expression);
                }
            };

            assert_eq!(*function, *expected_function);
            assert_eq!(arguments, *expected_aruguments);
        }
    }
}
