use std::mem::discriminant;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // 識別子 + リテラル
    Ident(String),
    Int(usize),

    // 演算子
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    Lt, // <
    Gt, // >

    Eq,    // ==
    NotEq, // !=

    // デリミタ
    Comma,
    Semicolon,

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn is_same_variant(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
}

pub const KEYWORDS: &[(&str, Token)] = &[
    ("fn", Token::Function),
    ("let", Token::Let),
    ("true", Token::True),
    ("false", Token::False),
    ("if", Token::If),
    ("else", Token::Else),
    ("return", Token::Return),
];

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
