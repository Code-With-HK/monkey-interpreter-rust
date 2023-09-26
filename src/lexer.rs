use crate::token::{lookup_ident, Token, TokenKind};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: Default::default(),
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        kind: TokenKind::Eq,
                        literal: String::from("=="),
                    }
                } else {
                    Lexer::new_token(TokenKind::Assign, self.ch)
                }
            }
            ';' => Lexer::new_token(TokenKind::Semicolon, self.ch),
            '(' => Lexer::new_token(TokenKind::Lparen, self.ch),
            ')' => Lexer::new_token(TokenKind::Rparen, self.ch),
            ',' => Lexer::new_token(TokenKind::Comma, self.ch),
            '+' => Lexer::new_token(TokenKind::Plus, self.ch),
            '{' => Lexer::new_token(TokenKind::Lbrace, self.ch),
            '}' => Lexer::new_token(TokenKind::Rbrace, self.ch),
            '\0' => Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
            '-' => Lexer::new_token(TokenKind::Minus, self.ch),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token {
                        kind: TokenKind::NotEq,
                        literal: String::from("!="),
                    }
                } else {
                    Lexer::new_token(TokenKind::Bang, self.ch)
                }
            }
            '/' => Lexer::new_token(TokenKind::Slash, self.ch),
            '*' => Lexer::new_token(TokenKind::Asterisk, self.ch),
            '<' => Lexer::new_token(TokenKind::Lt, self.ch),
            '>' => Lexer::new_token(TokenKind::Gt, self.ch),
            '"' => Token {
                kind: TokenKind::String,
                literal: self.read_string(),
            },
            '[' => Lexer::new_token(TokenKind::Lbracket, self.ch),
            ']' => Lexer::new_token(TokenKind::Rbracket, self.ch),
            _ => {
                return if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let kind = lookup_ident(&literal);
                    Token { kind, literal }
                } else if Lexer::is_digit(self.ch) {
                    let kind = TokenKind::Int;
                    let literal = self.read_number();
                    Token { kind, literal }
                } else {
                    Lexer::new_token(TokenKind::Illegal, self.ch)
                };
            }
        };

        self.read_char();

        return token;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        return if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        };
    }

    fn new_token(kind: TokenKind, ch: char) -> Token {
        Token {
            kind,
            literal: ch.to_string(),
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        while Lexer::is_letter(self.ch) {
            identifier.push(self.ch);
            self.read_char();
        }

        identifier
    }

    fn is_digit(ch: char) -> bool {
        ch.is_numeric()
    }

    fn read_number(&mut self) -> String {
        let mut num = String::from("");

        while Lexer::is_digit(self.ch) {
            num.push(self.ch);
            self.read_char();
        }
        num
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        self.read_char();
        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }

        let string_slice = &self.input[position..self.position];
        string_slice.into_iter().collect()
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Token, TokenKind};

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "foobar"
        "foo bar"
        [1, 2];
        "#;

        let expected: Vec<Token> = vec![
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "five".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "ten".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "add".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Function,
                literal: "fn".to_string(),
            },
            Token {
                kind: TokenKind::Lparen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "x".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "y".to_string(),
            },
            Token {
                kind: TokenKind::Rparen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::Lbrace,
                literal: "{".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "x".to_string(),
            },
            Token {
                kind: TokenKind::Plus,
                literal: "+".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "y".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Rbrace,
                literal: "}".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "result".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "add".to_string(),
            },
            Token {
                kind: TokenKind::Lparen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "five".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "ten".to_string(),
            },
            Token {
                kind: TokenKind::Rparen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Bang,
                literal: "!".to_string(),
            },
            Token {
                kind: TokenKind::Minus,
                literal: "-".to_string(),
            },
            Token {
                kind: TokenKind::Slash,
                literal: "/".to_string(),
            },
            Token {
                kind: TokenKind::Asterisk,
                literal: "*".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Lt,
                literal: "<".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Gt,
                literal: ">".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::If,
                literal: "if".to_string(),
            },
            Token {
                kind: TokenKind::Lparen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Lt,
                literal: "<".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Rparen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::Lbrace,
                literal: "{".to_string(),
            },
            Token {
                kind: TokenKind::Return,
                literal: "return".to_string(),
            },
            Token {
                kind: TokenKind::True,
                literal: "true".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Rbrace,
                literal: "}".to_string(),
            },
            Token {
                kind: TokenKind::Else,
                literal: "else".to_string(),
            },
            Token {
                kind: TokenKind::Lbrace,
                literal: "{".to_string(),
            },
            Token {
                kind: TokenKind::Return,
                literal: "return".to_string(),
            },
            Token {
                kind: TokenKind::False,
                literal: "false".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Rbrace,
                literal: "}".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Eq,
                literal: "==".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::NotEq,
                literal: "!=".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "9".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::String,
                literal: "foobar".to_string(),
            },
            Token {
                kind: TokenKind::String,
                literal: "foo bar".to_string(),
            },
            Token {
                kind: TokenKind::Lbracket,
                literal: "[".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "1".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "2".to_string(),
            },
            Token {
                kind: TokenKind::Rbracket,
                literal: "]".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for (idx, exp_token) in expected.into_iter().enumerate() {
            let recv_token = lexer.next_token();
            assert_eq!(
                exp_token.kind, recv_token.kind,
                "tests[{idx}] - token type wrong. expected={}, got={}",
                exp_token.kind, recv_token.kind
            );
            assert_eq!(
                exp_token.literal, recv_token.literal,
                "tests[{idx}] - literal wrong. expected={}, got={}",
                exp_token.literal, recv_token.literal
            );
        }
    }
}
