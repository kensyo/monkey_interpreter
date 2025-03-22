use std::io::{self, Write};

use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"
   .--,       .--,
  ( (  \.---./  ) )
   '.__/o   o\__.'
      {=  ^  =}
       >  -  <
      /       \
     //       \\
    //|   .   |\\
    "'\       /'"_.-~^`'-.
       \  _  /--'         `
     ___)( )(___
    (((__) (__)))    ヒヒッ
"#;

pub fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        // プロンプトを表示
        print!("{}", PROMPT);
        stdout.flush().expect("Failed to flush stdout");

        // 入力を読み取る
        input.clear();
        if stdin.read_line(&mut input).is_err() {
            eprintln!("Failed to read input");
            return;
        }

        let line = &input.trim(); // trim しないとエンターしたときi.e.改行文字だけ入力したときに終了されなくなる
        if line.is_empty() {
            return; // 空行で終了
        }

        let mut lexer = Lexer::new(line);
        let mut parser = Parser::new(&mut lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                print_parse_error(&e);
                continue;
            }
        };

        println!("{}", program.to_string());
    }
}

fn print_parse_error(e: &ParseError) {
    println!("{}", MONKEY_FACE);
    println!("{}", e);
}
