use std::io::{self, Write};

use crate::lexer::Lexer;
use crate::token::Token;

const PROMPT: &str = ">> ";

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

        // トークンを出力
        loop {
            let token = lexer.next_token();
            if token == Token::EOF {
                break;
            }
            println!("{:?}", token);
        }
    }
}
