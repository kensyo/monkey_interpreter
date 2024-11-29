#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // 識別子 + リテラル
    Ident(String),
    Int(usize),

    // 演算子
    Assign, // =
    Plus,   // +
    Minus,  // -
    Bang,   // !
    Asterisk, // *
    Slash, // /

    Lt, // <
    Gt, // >

    // デリミタ
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // キーワード
    Function,
    Let,
}

pub const KEYWORDS: &[(&str, Token)] = &[("fn", Token::Function), ("let", Token::Let)];

pub fn lookup_ident(s: String) -> Token {
    for &(key, ref value) in KEYWORDS.iter() {
        if s == key {
            return value.clone();
        }
    }
    Token::Ident(s)
}

// #[cfg(test)]
// mod token_tests {
//     use super::*;
// }
