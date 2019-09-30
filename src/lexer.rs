use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::iter;
use std::str::Chars;

use crate::internals::peek_while;

#[derive(PartialEq, Debug)]
pub enum Token {
    IDENTIFIER(String),
    BOOLEAN(bool),
    INTEGER(i64),
    FLOAT(f64),
    CHARACTER(char),
    STRING(String),
    LPAREN,
    RPAREN,
    LBRACK,
    RBRACK,
    VEC_LPAREN,
    QUOTE,
    QUASIQUOTE,
    UNQUOTE,
    UNQUOTE_SPLICING,
    DOT,
    ELLIPSIS,
}

pub type Position = (usize, usize);

pub struct Lexer<'a> {
    chars: iter::Peekable<Chars<'a>>,
    current: Option<char>,
    tokens: Vec<(Token, Position)>,
    line: usize,
    column: usize,
}

pub struct LexError {
    msg: String,
    line: usize,
    column: usize,
}

impl Debug for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "LEX ERR({}:{}): {}", self.line, self.column, self.msg)
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "LEX ERR({}:{}): {}", self.line, self.column, self.msg)
    }
}

impl StdError for LexError {}

macro_rules! lex_error {
    ($lexer:ident, $($arg:tt)*) => (
        return Err(LexError { msg: format!($($arg)*), line: $lexer.line, column: $lexer.column })
    )
}

impl<'a> Lexer<'a> {
    fn position(&self) -> Position {
        (self.line, self.column)
    }

    fn push_token(&mut self, tok: Token) {
        self.tokens.push((tok, self.position()));
    }

    fn checkpoint(&self) -> Self {
        Lexer {
            chars: self.chars.clone(),
            current: self.current.clone(),
            tokens: Vec::new(), // do not copy tokens
            line: self.line,
            column: self.column,
        }
    }

    fn reset(&mut self, cp: Lexer<'a>) {
        self.chars = cp.chars;
        self.current = cp.current;
        self.line = cp.line;
        self.column = cp.column;
    }

    fn advance(&mut self) {
        match self.current {
            Some(c) if c == '\n' => {
                self.line += 1;
                self.column = 1;
            }
            _ => {
                self.column += 1;
            }
        }
        self.current = self.chars.next();
    }

    fn advancec(&mut self) -> Option<char> {
        self.advance();
        self.current
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|c: &char| *c) // copy char
    }

    fn is_delimiter(c: &char) -> bool {
        match c {
            _ if c.is_whitespace() => true,
            '(' | ')' | '"' | ';' => true,
            _ => false,
        }
    }

    fn parse_delimiter(&mut self) -> Result<(), LexError> {
        match self.current {
            Some(c) => match c {
                _ if c.is_whitespace() => {
                    self.advance();
                    Ok(())
                },
                '(' => {
                    self.push_token(Token::LPAREN);
                    self.advance();
                    Ok(())
                },
                ')' => {
                    self.push_token(Token::RPAREN);
                    self.advance();
                    Ok(())
                },
                '[' => {
                    self.push_token(Token::LBRACK);
                    self.advance();
                    Ok(())
                },
                ']' => {
                    self.push_token(Token::RBRACK);
                    self.advance();
                    Ok(())
                },
                '"' => {
                    let s = self.parse_string()?;
                    self.push_token(Token::STRING(s));
                    self.advance();
                    Ok(())
                },
                ';' => {
                    self.parse_comment()?;
                    Ok(())
                },
                _ => lex_error!(self, "unexpected character: `{}', looking for delimiter", c),
            },
            _ => Ok(()),
        }
    }

    fn parse_comment(&mut self) -> Result<(), LexError> {
        match self.current {
            Some(';') => {
                while let Some(c) = self.advancec() {
                    if c == '\n' {
                        self.advance(); // next non-comment char
                        break;
                    }
                }
                Ok(())
            }
            Some(c) => lex_error!(self, "unexpected character: `{}'", c),
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    fn parse_boolean(&mut self) -> Result<bool, LexError> {
        match self.current {
            Some('t') => Ok(true),
            Some('f') => Ok(false),
            Some(c) => lex_error!(
                self,
                "unexpected character: `{}', looking for boolean(t or f)",
                c
            ),
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    // NOTE: begins with #\, current is \
    fn parse_character(&mut self) -> Result<char, LexError> {
        match self.current {
            Some('\\') => {
                let s =
                    peek_while(&mut self.chars, |c: &char| c.is_alphabetic()).collect::<String>();
                match s.as_ref() {
                    _ if s.len() == 1 => Ok(s.chars().next().unwrap()),
                    "space" => Ok(' '),
                    "newline" => Ok('\n'),
                    _ => lex_error!(self, "illegal chracter sequence: `{}'", s),
                }
            }
            Some(c) => lex_error!(self, "unexpected character: `{}'", c),
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    // NOTE: only escape: \" \t \n \r
    // TODO: line wrap?
    fn parse_string(&mut self) -> Result<String, LexError> {
        match self.current {
            Some('"') => {
                let mut prev = '"';
                let mut s = String::new();
                loop {
                    match self.advancec() {
                        Some(c) => {
                            match c {
                                '"' if prev != '\\' => {
                                    break;
                                }
                                't' if prev == '\\' => {
                                    s.push('\t');
                                }
                                'n' if prev == '\\' => {
                                    s.push('\n');
                                }
                                'r' if prev == '\\' => {
                                    s.push('\r');
                                }
                                '\\' => {}
                                _ => {
                                    s.push(c);
                                }
                            }
                            prev = c;
                        }
                        _ => lex_error!(self, "unexpected eof, looking for string end quote"),
                    }
                }
                Ok(s)
            }
            Some(c) => lex_error!(self, "unexpected character: `{}'", c),
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    fn parse_sign(&mut self) -> Result<Option<char>, LexError> {
        match self.current {
            Some(c) if c == '+' || c == '-' => {
                //                self.advance();
                Ok(Some(c))
            }
            Some(_) => Ok(None), // do not advance, leaving number parse to subsequent ops
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    fn parse_radix(&mut self) -> Result<u32, LexError> {
        match self.current {
            Some('b') => Ok(2),
            Some('o') => Ok(8),
            Some('d') => Ok(10),
            Some('x') => Ok(16),
            Some(c) => lex_error!(
                self,
                "unexpected character: `{}', looking for radix symbol",
                c
            ),
            _ => lex_error!(self, "unexpected eof"),
        }
    }

    // scan until delimiter
    fn parse_integer(&mut self, radix: u32) -> Result<i64, LexError> {
        let sign = self.parse_sign()?;
        let s = if let Some(c) = sign {
            let mut s =
                peek_while(&mut self.chars, |c: &char| !Self::is_delimiter(c)).collect::<String>();
            s.insert(0, c);
            s
        } else {
            // current is digit now
            match self.current {
                Some(c) if c.is_digit(radix) => {
                    let mut s = peek_while(&mut self.chars, |c: &char| !Self::is_delimiter(c))
                        .collect::<String>();
                    s.insert(0, c);
                    s
                }
                _ => String::new(),
            }
        };
        match i64::from_str_radix(&s, radix) {
            Ok(v) => Ok(v),
            Err(e) => lex_error!(self, "{}: {}", e, s),
        }
    }

    fn parse_float(&mut self) -> Result<f64, LexError> {
        let sign = self.parse_sign()?;
        let s = if let Some(sgn) = sign {
            let mut s =
                peek_while(&mut self.chars, |c: &char| !Self::is_delimiter(c)).collect::<String>();
            s.insert(0, sgn);
            s
        } else {
            // current is digit now
            match self.current {
                Some(c) if c.is_digit(10) => {
                    let mut s = peek_while(&mut self.chars, |c: &char| !Self::is_delimiter(c))
                        .collect::<String>();
                    s.insert(0, c);
                    s
                }
                _ => String::new(),
            }
        };
        match s.parse::<f64>() {
            Ok(v) => Ok(v),
            Err(e) => lex_error!(self, "{}: {}", e, s),
        }
    }

    fn is_ident_initial(c: &char) -> bool {
        match c {
            _ if c.is_alphabetic() => true,
            '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'
            | '+' | '-' => true,
            _ => false,
        }
    }

    fn is_ident_subsequent(c: &char) -> bool {
        match c {
            _ if Self::is_ident_initial(c) => true,
            _ if c.is_digit(10) => true,
            '+' | '-' | '.' | '@' => true, // special subsequent
            _ => false,
        }
    }

    fn parse_ident(&mut self) -> Result<String, LexError> {
        let mut s = String::new();
        // 1. initial
        match self.current {
            Some(c) if Self::is_ident_initial(&c) => {
                s.push(c);
            }
            Some(c) => lex_error!(self, "unexpected character: `{}'", c),
            _ => lex_error!(self, "unexpected eof"),
        }
        // 2. subsequent*
        let ss = peek_while(&mut self.chars, Self::is_ident_subsequent).collect::<String>();
        s.push_str(&ss);
        Ok(s)
    }

    fn parse(&mut self) -> Result<(), LexError> {
        self.advance();
        loop {
            // always new char at loop beginning
            match self.current {
                Some(c) => {
                    match c {
                        // whitespace
                        _ if c.is_whitespace() => self.advance(),
                        // comment
                        ';' => {
                            self.parse_comment()?;
                        }
                        // dot
                        '.' => {
                            // peculiar ident: ellipsis
                            if let Some('.') = self.peek() {
                                let s =
                                    peek_while(&mut self.chars, |c| c == &'.').collect::<String>();
                                if s != ".." {
                                    lex_error!(
                                        self,
                                        "unexpected sequence: `{}', looking for ...",
                                        s
                                    );
                                } else {
                                    self.push_token(Token::ELLIPSIS);
                                }
                            } else {
                                self.push_token(Token::DOT);
                            }
                            self.advance();
                        }
                        // quote
                        '\'' => {
                            self.push_token(Token::QUOTE);
                            self.advance();
                        }
                        '`' => {
                            self.push_token(Token::QUASIQUOTE);
                            self.advance();
                        }
                        ',' => {
                            if self.peek() == Some('@') {
                                self.advance();
                                self.push_token(Token::UNQUOTE_SPLICING);
                            } else {
                                self.push_token(Token::UNQUOTE);
                            }
                            self.advance();
                        }
                        // paren
                        '(' => {
                            self.push_token(Token::LPAREN);
                            self.advance()
                        }
                        ')' => {
                            self.push_token(Token::RPAREN);
                            self.advance()
                        }
                        '[' => {
                            self.push_token(Token::LBRACK);
                            self.advance()
                        }
                        ']' => {
                            self.push_token(Token::RBRACK);
                            self.advance()
                        }
                        // boolean | char | vec
                        '#' => {
                            match self.peek() {
                                Some(cc) => {
                                    match cc {
                                        // boolean
                                        't' | 'f' => {
                                            self.advance(); // current: 't' | 'f'
                                            let v = self.parse_boolean()?;
                                            self.push_token(Token::BOOLEAN(v));
                                            self.advance();
                                        }
                                        // vec
                                        '(' => {
                                            self.advance();
                                            self.push_token(Token::VEC_LPAREN);
                                            self.advance();
                                        }
                                        // char(sp: #\space #\newline)
                                        '\\' => {
                                            self.advance();
                                            let v = self.parse_character()?;
                                            self.push_token(Token::CHARACTER(v));
                                            self.advance();
                                            self.parse_delimiter()?;
                                        }
                                        // integer with radix
                                        'b' | 'o' | 'd' | 'x' => {
                                            self.advance();
                                            let r = self.parse_radix()?;
                                            self.advance();
                                            let v = self.parse_integer(r)?;
                                            self.push_token(Token::INTEGER(v));
                                            self.advance();
                                            self.parse_delimiter()?;
                                        }
                                        _ => lex_error!(self, "unexpected character: `{}'", cc),
                                    }
                                }
                                _ => lex_error!(self, "unexpected eof"),
                            }
                        }
                        // string
                        '"' => {
                            let v = self.parse_string()?;
                            self.push_token(Token::STRING(v));
                            self.advance();
                        }
                        // number
                        '+' | '-' => {
                            match self.chars.peek() {
                                Some(c) if c.is_digit(10) => {
                                    // NOTE: with try we need to reset stream!
                                    let restore = self.checkpoint();
                                    // try integer
                                    match self.parse_integer(10) {
                                        Ok(v) => {
                                            self.push_token(Token::INTEGER(v));
                                            self.advance();
                                            self.parse_delimiter()?;
                                        }
                                        Err(_) => {
                                            self.reset(restore);
                                            let v = self.parse_float()?;
                                            self.push_token(Token::FLOAT(v));
                                            self.parse_delimiter()?;
                                        }
                                    }
                                }
                                _ => {
                                    // then peculiar ident
                                    // or special ident for macro
                                    let v = self.parse_ident()?;
                                    self.push_token(Token::IDENTIFIER(v));
                                    self.advance();
                                    self.parse_delimiter()?;
                                }
                            }
                        }
                        // number w/o radix
                        '0'..='9' => {
                            // try integer
                            let restore = self.checkpoint();
                            match self.parse_integer(10) {
                                Ok(v) => {
                                    self.push_token(Token::INTEGER(v));
                                    self.advance();
                                    self.parse_delimiter()?;
                                }
                                Err(_) => {
                                    self.reset(restore);
                                    let v = self.parse_float()?;
                                    self.push_token(Token::FLOAT(v));
                                    self.advance();
                                    self.parse_delimiter()?;
                                }
                            }
                        }
                        // reserved
                        '{' | '}' | '|' | '\\' => {
                            lex_error!(self, "reserved token: {}", c);
                        }
                        // ident
                        _ => {
                            let v = self.parse_ident()?;
                            self.push_token(Token::IDENTIFIER(v));
                            self.advance();
                            self.parse_delimiter()?;
                        }
                    }
                }
                // eof
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    pub fn tokenize(i: &str) -> Result<Vec<(Token, Position)>, LexError> {
        let mut lex = Lexer {
            chars: i.chars().peekable(),
            current: None,
            tokens: Vec::new(),
            line: 1,
            column: 0,
        };
        lex.parse()?;
        Ok(lex.tokens)
    }

    pub fn tokenize_line(i: &str, line: usize) -> Result<Vec<(Token, Position)>, LexError> {
        let mut lex = Lexer {
            chars: i.chars().peekable(),
            current: None,
            tokens: Vec::new(),
            line,
            column: 0,
        };
        lex.parse()?;
        Ok(lex.tokens)
    }
}

pub fn tokenize(i: &str) -> Result<Vec<Token>, LexError> {
    Lexer::tokenize(i).map(|v| v.into_iter().map(|i| i.0).collect())
}

mod tests {
    use super::*;

    fn lexer(i: &str) -> Lexer {
        Lexer {
            chars: i.chars().peekable(),
            current: None,
            tokens: Vec::new(),
            line: 1,
            column: 0,
        }
    }

    #[test]
    fn test_lex_simple() {
        assert_eq!(
            tokenize("(+ 11 -234)").unwrap(),
            vec![
                Token::LPAREN,
                Token::IDENTIFIER("+".to_string()),
                Token::INTEGER(11),
                Token::INTEGER(-234),
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_boolean() {
        assert_eq!(tokenize("#t").unwrap(), vec![Token::BOOLEAN(true)]);
        assert_eq!(tokenize("#f").unwrap(), vec![Token::BOOLEAN(false)]);
    }

    #[test]
    fn test_lexer_identifiers() {
        for identifier in ["*", "<", "<=", "if", "while", "$t$%*=:t059s"].iter() {
            assert_eq!(
                tokenize(*identifier).unwrap(),
                vec![Token::IDENTIFIER(identifier.to_string())]
            );
        }
    }

    #[test]
    fn test_lexer_delim() {
        assert_eq!(
            tokenize("#t#f").unwrap(),
            vec![Token::BOOLEAN(true), Token::BOOLEAN(false)]
        );
        assert_eq!(
            tokenize("\"a\"\"b\"").unwrap(),
            vec![
                Token::STRING("a".to_string()),
                Token::STRING("b".to_string())
            ]
        );
        assert_eq!(
            tokenize("(cons #\\a #\\newline )").unwrap(),
            vec![
                Token::LPAREN,
                Token::IDENTIFIER("cons".to_string()),
                Token::CHARACTER('a'),
                Token::CHARACTER('\n'),
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_lexer_strings() {
        assert_eq!(
            tokenize("\"hello\"").unwrap(),
            vec![Token::STRING("hello".to_string())]
        );
        assert_eq!(
            tokenize("\"a _ $ snthoeau(*&G#$()*^!\"").unwrap(),
            vec![Token::STRING("a _ $ snthoeau(*&G#$()*^!".to_string())]
        );
        let e = tokenize("\"truncated");
        assert!(e.is_err());
        println!("{}", e.err().unwrap().to_string());
    }

    #[test]
    fn test_lexer_whitespace() {
        assert_eq!(
            tokenize("(+ 1 1)\n(+\n    2\t+2 \n )\r\n  \n").unwrap(),
            vec![
                Token::LPAREN,
                Token::IDENTIFIER("+".to_string()),
                Token::INTEGER(1),
                Token::INTEGER(1),
                Token::RPAREN,
                Token::LPAREN,
                Token::IDENTIFIER("+".to_string()),
                Token::INTEGER(2),
                Token::INTEGER(2),
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_lexer_delimiter_checking() {
        let e = tokenize("(+ 2 3)\n(+ 1 2-)");
        assert!(e.is_err());
        println!("{}", e.err().unwrap().to_string());
    }

    #[test]
    fn test_lexer_quoting() {
        assert_eq!(
            tokenize("'(a)").unwrap(),
            vec![
                Token::QUOTE,
                Token::LPAREN,
                Token::IDENTIFIER("a".to_string()),
                Token::RPAREN
            ]
        );
        assert_eq!(
            tokenize("'('a 'b)").unwrap(),
            vec![
                Token::QUOTE,
                Token::LPAREN,
                Token::QUOTE,
                Token::IDENTIFIER("a".to_string()),
                Token::QUOTE,
                Token::IDENTIFIER("b".to_string()),
                Token::RPAREN
            ]
        );
        assert_eq!(
            tokenize("(list 'a b)").unwrap(),
            vec![
                Token::LPAREN,
                Token::IDENTIFIER("list".to_string()),
                Token::QUOTE,
                Token::IDENTIFIER("a".to_string()),
                Token::IDENTIFIER("b".to_string()),
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_lexer_quasiquoting() {
        assert_eq!(
            tokenize("`(,a)").unwrap(),
            vec![
                Token::QUASIQUOTE,
                Token::LPAREN,
                Token::UNQUOTE,
                Token::IDENTIFIER("a".to_string()),
                Token::RPAREN
            ]
        );
        assert_eq!(
            tokenize("`(,a b ,c)").unwrap(),
            vec![
                Token::QUASIQUOTE,
                Token::LPAREN,
                Token::UNQUOTE,
                Token::IDENTIFIER("a".to_string()),
                Token::IDENTIFIER("b".to_string()),
                Token::UNQUOTE,
                Token::IDENTIFIER("c".to_string()),
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_vardiac_arg() {
        assert_eq!(
            tokenize("(lambda (x y . z))").unwrap(),
            vec![
                Token::LPAREN,
                Token::IDENTIFIER("lambda".to_string()),
                Token::LPAREN,
                Token::IDENTIFIER("x".to_string()),
                Token::IDENTIFIER("y".to_string()),
                Token::DOT,
                Token::IDENTIFIER("z".to_string()),
                Token::RPAREN,
                Token::RPAREN
            ]
        );
    }

    #[test]
    fn test_lexer_complex_code_block() {
        assert_eq!(tokenize(
            "(define (list-of-squares n)\n  (let loop ((i n) (res (list)))\n    (if (< i 0)\n        res\n        (loop (- i 1) (cons (* i i) res)))))").unwrap(),
                   vec![Token::LPAREN, Token::IDENTIFIER("define".to_string()),
                        Token::LPAREN, Token::IDENTIFIER("list-of-squares".to_string()), Token::IDENTIFIER("n".to_string()), Token::RPAREN,
                        Token::LPAREN, Token::IDENTIFIER("let".to_string()), Token::IDENTIFIER("loop".to_string()), Token::LPAREN,
                        Token::LPAREN, Token::IDENTIFIER("i".to_string()), Token::IDENTIFIER("n".to_string()), Token::RPAREN,
                        Token::LPAREN, Token::IDENTIFIER("res".to_string()), Token::LPAREN, Token::IDENTIFIER("list".to_string()), Token::RPAREN,
                        Token::RPAREN, Token::RPAREN,
                        Token::LPAREN, Token::IDENTIFIER("if".to_string()), Token::LPAREN, Token::IDENTIFIER("<".to_string()), Token::IDENTIFIER("i".to_string()), Token::INTEGER(0), Token::RPAREN,
                        Token::IDENTIFIER("res".to_string()),
                        Token::LPAREN, Token::IDENTIFIER("loop".to_string()),
                        Token::LPAREN, Token::IDENTIFIER("-".to_string()), Token::IDENTIFIER("i".to_string()), Token::INTEGER(1), Token::RPAREN,
                        Token::LPAREN, Token::IDENTIFIER("cons".to_string()),
                        Token::LPAREN, Token::IDENTIFIER("*".to_string()), Token::IDENTIFIER("i".to_string()), Token::IDENTIFIER("i".to_string()), Token::RPAREN,
                        Token::IDENTIFIER("res".to_string()), Token::RPAREN, Token::RPAREN, Token::RPAREN, Token::RPAREN, Token::RPAREN]);
    }

    #[test]
    fn test_lexer_unicode_identifiers() {
        assert_eq!(
            tokenize("λ").unwrap(), // alphabetic
            vec![Token::IDENTIFIER("λ".to_string())]
        );
    }

    #[test]
    fn test_lexer_reserved() {
        assert!(tokenize("(cons {a b})").is_err());
        assert!(tokenize("(a | b)").is_err());
        assert!(tokenize("(cons \"abc\" #\\c \\b)").is_err());
        println!(
            "{}",
            tokenize("(cons \"abc\" #\\c \\b)")
                .err()
                .unwrap()
                .to_string()
        );
    }
}
