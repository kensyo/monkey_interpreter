// 指針：<statement> -> let [ident] = <expression> | <statement>{ ; <statement> }
// という正規右辺文法({} は0回以上の繰り返し)による Production があったとする(<> は non terminal を表す。それ以外は terminal
// だが、[]で囲ったものは字句解析にて作ったトークンにて付随する値があるものとする
// ([ident] なら具体的な変数名が付随してる)。
// statement に対応する enum を作るが、今回の場合、let
// 文には[ident]が一つ、<expression>が一つあるので、この二つを持てるような要素Let
// と、繰り返し文には<statement>が任意複数個あるので、これを持てるような要素MultiStatement
// というものを用意する。
//
// enum Statement {
//     Let(Ident, Expression),
//     MultiStatement(Vec<Statement>)
// }
//
// を用意する(Rustの言語仕様によって場合によってはBox<Statement>を使うこともあろう)。
// Ident, Expressionはまだ用意していないが、Experssionはstatementと同様にenumとして用意するが、Identは[]なので構造体で用意する。
//
// enum Expression {
//    // <expression> -> ... という Production があるはずなので、それにしたがってここを実装する
// }
// struct Ident (String) // タプル構造体。別にフィールド名を与えても良いし、なんなら構造体じゃなくて type Ident = String; としても良いかも
//
// なお
// X -> A | B { C | D }
// のような規則で { C | D } の部分については対応する生成規則はないものの
// enum Hoge {
//     C,
//     D
// }
// のような enum を作るのが自然だろう
//
// なお、本当に機械的に導出したいのであれば、ECFGの構文解析にしたがったデータ構造を考えることになり、
// astではなくsyntax treeを作ると良いだろう(astはそこから[]と<>に対応するもの以外を抜いたものになる)。例
// <statement> -> let [ident] = <expression> | <statement>{ ; <statement> } | return;
// で考えると
// enum Statement {
//    LetStatement(Let, Ident, Equal, Expression),
//
//    // {}は新しいenum(OtherStatements)に切り出し、Vec で表現。() なら新しい表現で切り出すだけ。
//    MultiStatement(Statement, Vec<OtherStatements>), // 実際には一つ目の引数は Box<Statement> となるだろうが省略する
//    ReturnStatement(Return, SemiColon)
// }
//
// struct Ident (String) // タプル構造体
//
// enum Expression {
//    // Expression -> ... という production があるはずなので Statement と同じように実装
// }
//
// enum OtherStatements {
//     Others(StatementWithSemiColon)
// }
//
// enum StatementWithSemiColon {
//    SWS(SemiColon, Statement)
// }
//
// struct Let // ユニット構造体
//
// （Equal, SemiColon, Return などの他のものも同様にするi.e.Terminalシンボル(=トークン)は全て構造体にする（データを保持してないトークンについてはユニット構造体、保持するものはタプル構造体）、それ以外(NonTerminalや切り出したもの)は enum 。なおast にするならユニット構造体に相当するものはいらなくなる）
//
// というように syntax tree を作ればよかろう。 ast にするなら、ユニット構造体は全て無くせば良い。
// なので、
// enum Statement {
//     LetStatement(Ident, Expression),
//     MultiStatement(Statement, Vec<OtherStatements>),
//     ReturnStatment
// }
//
// struct Idnet (String)
//
// enum Expression {
//    // Expression -> ... という production があるはずなので Statement と同じように実装
// }
//
// enum OtherStatements {
//     Others(StatementWithSemiColon)
// }
//
// enum StatementWithSemiColon {
//    SWS(Statement)
// }
//
// となるだろう。OtherStatements 以下あたりは冗長に感じるかもしれないが、機械的にやるとこのように効率は落ちる。
//
// ただ、ast を作るならもう少し無駄のなくなる方法があって
// <statement> -> "Let文"[ident] <expression> | "マルチ文" <statement>{ <statement> } | "return文"
// と最初から[]と<>以外を削除したものを考えて syntax tree を作った時と同じように考えて
//
// enum Statement {
//     LetStatement(Ident, Expression),
//     MultiStatement(Statement, Vec<Statement>), // 切り出す時に中身に | がなければ新しい enum は作らない
//     ReturnStatment
// }
//
// struct Ident (String)
//
// enum Expression {
//    // Expression -> ... という production があるはずなので Statement と同じように実装
// }
//
// となり、すっきりする。
// 注意： 仮に[]と<>以外を削除後に、マルチ文のところが
// <statement> { <statement> | <statement> }
// や
// <statement> { [ident] }
// や
// <statement> { [ident] {<A> | <B>} (<A> | <C>)}
// や
// <statement> {  }
// や
// <statement> { | }
// のようになっていたとしたら
// enum Statement {
//     ...
//     MultiStatement(Statement, Vec<Others>)
//     ...
// }
// enum Others {
//     // 二つのStatementは別ものである
//     s1(Statement),
//     s2(Statement)
// }
// や
// enum Statement {
//     ...
//     MultiStatement(Statement, Vec<Ident>)
//     ...
// }
// や
// enum Statement {
//     ...
//     MultiStatement(Statement, Vec<AorB>, AorC)
//     ...
// }
// enum AorB {
//    Adesu(A),
//    Bdesu(B)
// }
//
// enum AorC {
//    Adesu(A),
//    Cdesu(C)
// }
//
// enum A {
//    // A -> ... というプロダクションにしたがって実装
// }
// enum B {
//    // B -> ... というプロダクションにしたがって実装
// }
// enum C {
//    // C -> ... というプロダクションにしたがって実装
// }
// や
// enum Statement {
//     ...
//     MultiStatement(Statement, Vec<()>)
//     ...
// }
// や
// enum Statement {
//     ...
//     MultiStatement(Statement, Vec<Empty>)
//     ...
// }
// enum Empty {
//     Empty1(()),
//     Empty2(())
// }
// のようになる。

// Monkey言語 正規右辺文法
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
//

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
