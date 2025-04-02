// Monkey 言語 ECFG
// <Program> -> { <Statement> } EOF
// <Statement> -> let [Ident] = <Expression> ; | return <Expression> ; | <Expression> ;
// <Expression> -> [Ident]
//                   | [Int]
//                   | <Prefix operator> <Expression>
//                   | <Expression> <Infix operator> <Expression>
//                   | <Boolean>
//                   | '(' <Expression> ')'
//                   | if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε )
//                   | fn <Parameters> <Block statement>
//                   | <Expression> '(' <Comma separated expressions> ')'
// <Prefix operator> -> ! | -
// <Infix operator> -> + | - | * | / | > | < | == | !=
// <Boolean> -> true | false
// <Block statement> -> '{' { <Statement> } '}'
// <Parameters> -> '(' ( [ident] { , [ident] } | ε ) ')'
// <Comma separated expressions> -> ( <Expression> { , <Expression> } | ε )
// 注意: '(', ')' などは終端記号。（）や {} が特殊記号として使われてしまっているため、'' で囲んだ

// <Program> -> { <Statement> } EOF
#[derive(Debug, PartialEq)]
pub enum Program {
    Program(Vec<Statement>), // 1種類しかないやつはHoge::Hogeのようなバリアントの命名をすることにする
}

// <Statement> -> let [Ident] = <Expression> ; | return <Expression> ; | <Expression> ;
#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression), // 式文(セミコロンの省略はしないものとする)
}

// <Expression> -> [Ident]
//                   | [Int]
//                   | <Prefix operator> <Expression>
//                   | <Expression> <Infix operator> <Expression>
//                   | <Boolean>
//                   | ( <Expression> )
//                   | if '(' <Expression> ')' <Block statement> ( else <Block statement> | ε )
//                   | fn <Parameters> <Block statement>
//                   | <Expression> '(' <Comma separated expressions> ')'
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
    Int(Int),
    PrefixExpression(PrefixOperator, Box<Expression>),
    InfixExpression(Box<Expression>, InfixOperator, Box<Expression>),
    Boolean(Boolean),
    GroupedExpression(Box<Expression>),
    IfExpression(Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Parameters, BlockStatement),
    CallExpression(Box<Expression>, CommaSeparatedExpression),
}

// <Prefix operator> -> ! | -
#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

// <Infix operator> -> + | - | * | / | > | < | == | !=
#[derive(Debug, PartialEq)]
pub enum InfixOperator {
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /

    Lt, // <
    Gt, // >

    Eq,    // ==
    NotEq, // !=
}

// <Boolean> -> true | false
#[derive(Debug, PartialEq)]
pub enum Boolean {
    True,
    False,
}

// <Block statement> -> '{' { <Statement> } '}'
#[derive(Debug, PartialEq)]
pub enum BlockStatement {
    BlockStatement(Vec<Statement>),
}

// <Parameters> -> '(' ( [ident] { , [ident] } | ε ) ')'
#[derive(Debug, PartialEq)]
pub enum Parameters {
    Parameters(Vec<Ident>),
}

// <Comma separated expressions> -> ( <Expression> { , <Expression> } | ε )
#[derive(Debug, PartialEq)]
pub enum CommaSeparatedExpression {
    CommaSeparatedExpression(Vec<Expression>),
}

// 付随する値を持つトークン
#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub struct Int(pub usize);

// 以下 Display トレイト実装

use std::fmt;

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Program::Program(statements) => {
                for stmt in statements {
                    write!(f, "{}", stmt)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Int(int) => write!(f, "{}", int),
            Expression::PrefixExpression(prefix_operator, expression) => {
                write!(f, "({}{})", prefix_operator, expression)
            }
            Expression::InfixExpression(left_expression, infix_operator, right_expression) => {
                write!(
                    f,
                    "({} {} {})",
                    left_expression, infix_operator, right_expression
                )
            }
            Expression::Boolean(boolean) => {
                write!(f, "{}", boolean)
            }
            Expression::GroupedExpression(expression) => {
                write!(f, "{}", expression)
            }
            Expression::IfExpression(condition, consequence, alternative) => {
                write!(f, "if")?;
                write!(f, "{}", condition)?;
                write!(f, " ")?;
                write!(f, "{}", consequence)?;

                if let Some(alt) = alternative {
                    write!(f, "else")?;
                    write!(f, "{}", alt)?;
                }

                Ok(())
            }
            Expression::FunctionLiteral(parameters, body) => {
                write!(f, "fn{} {}", parameters, body)
            }
            Expression::CallExpression(function, arguments) => {
                write!(f, "{}({})", function, arguments)
            }
        }
    }
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Asterisk => write!(f, "*"),
            InfixOperator::Slash => write!(f, "/"),
            InfixOperator::Lt => write!(f, "<"),
            InfixOperator::Gt => write!(f, ">"),
            InfixOperator::Eq => write!(f, "=="),
            InfixOperator::NotEq => write!(f, "!="),
        }
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Boolean::True => write!(f, "true"),
            Boolean::False => write!(f, "false"),
        }
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockStatement::BlockStatement(block_statement) => {
                for statement in block_statement {
                    write!(f, "{}", statement)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Parameters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Parameters::Parameters(parameters) => {
                write!(
                    f,
                    "({})",
                    parameters
                        .iter()
                        .map(|ident| ident.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl fmt::Display for CommaSeparatedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommaSeparatedExpression::CommaSeparatedExpression(cse) => {
                write!(
                    f,
                    "{}",
                    cse.iter()
                        .map(|expression| expression.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    // p.54 の TestString テストに相当
    fn test_display_trait() {
        let input = r#"
let myVar = anotherVar;
"#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);

        // parse_program がエラーを返していたらパニックして終了
        let program = p.parse_program().unwrap();

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
