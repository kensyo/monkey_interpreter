use crate::token::{lookup_ident, Token};

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
        self.skip_whitespace();

        let tok = match self.index_char {
            Some(_i_ch @ (_, '=')) => Token::Assign,
            Some(_i_ch @ (_, '+')) => Token::Plus,
            Some(_i_ch @ (_, '-')) => Token::Minus,
            Some(_i_ch @ (_, '!')) => Token::Bang,
            Some(_i_ch @ (_, '/')) => Token::Slash,
            Some(_i_ch @ (_, '*')) => Token::Asterisk,
            Some(_i_ch @ (_, '<')) => Token::Lt,
            Some(_i_ch @ (_, '>')) => Token::Gt,
            Some(_i_ch @ (_, ';')) => Token::Semicolon,
            Some(_i_ch @ (_, ',')) => Token::Comma,
            Some(_i_ch @ (_, '(')) => Token::LParen,
            Some(_i_ch @ (_, ')')) => Token::RParen,
            Some(_i_ch @ (_, '{')) => Token::LBrace,
            Some(_i_ch @ (_, '}')) => Token::RBrace,

            Some((_, ch)) => {
                if Lexer::is_letter(ch) {
                    return self.read_identifier();
                } else if Lexer::is_digit(ch) {
                    return self.read_number();
                } else {
                    Token::Illegal
                }
            }

            None => Token::EOF,
        };

        self.read_char();
        tok
    }

    pub fn read_identifier(&mut self) -> Token {
        let mut identifier = String::new();
        while let Some((_, ch)) = self.index_char {
            if !Lexer::is_letter(ch) {
                break;
            }
            identifier.push(ch);
            self.read_char();
        }

        lookup_ident(identifier)
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn skip_whitespace(&mut self) {
        while let Some((_, ch)) = self.index_char {
            if [' ', '\t', '\n', '\r'].contains(&ch) {
                self.read_char();
            } else {
                break;
            }
        }
    }

    pub fn read_number(&mut self) -> Token {
        let mut number_string = String::new();
        while let Some((_, ch)) = self.index_char {
            if !Lexer::is_digit(ch) {
                break;
            }
            number_string.push(ch);
            self.read_char();
        }

        Token::Int(number_string.parse::<usize>().unwrap())
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit() 
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;
"#;

        let tests = [
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
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
