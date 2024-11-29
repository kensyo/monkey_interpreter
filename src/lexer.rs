use crate::token::{self, Token};

pub struct Lexer<'a> {
    input: &'a str,
    position: std::str::CharIndices<'a>,
    read_position: std::str::CharIndices<'a>,
    index_char: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut read_position = input.char_indices();
        let index_char = read_position.next();

        Lexer {
            input,
            position: input.char_indices(),
            read_position, // 今回は二つ以上の先読みはしないっぽいので、一つの peekable イテレータがあれば多分間に合うが、本に合わせる
            index_char,
        }
    }

    fn read_char(&mut self) {
        self.index_char = self.read_position.next();
        self.position.next();
    }

    pub fn next_token(&mut self) -> Token {
        let tok = match self.index_char {
            Some(_index_ch @ (_, '=')) => Token::Assign,
            Some(_index_ch @ (_, ';')) => Token::Semicolon,
            Some(_index_ch @ (_, '(')) => Token::LParen,
            Some(_index_ch @ (_, ')')) => Token::RParen,
            Some(_index_ch @ (_, ',')) => Token::Comma,
            Some(_index_ch @ (_, '+')) => Token::Plus,
            Some(_index_ch @ (_, '{')) => Token::LBrace,
            Some(_index_ch @ (_, '}')) => Token::RBrace,

            Some(_index_ch) => Token::Illegal,

            None => Token::EOF,
        };

        self.read_char();
        tok
    }
}

#[cfg(test)]
mod tests {
    use self::token::Token;

    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"=+(){},;"#;

        let tests = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for expect in tests {
            let tok = lexer.next_token();

            assert_eq!(expect, tok);
        }
    }
}
