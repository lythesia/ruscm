use std::error::Error as StdError;
use std::fmt::{Debug,Display,Formatter,Result as FmtResult};
use std::slice;
use std::str::FromStr;

//use strum::EnumMessage;
//use strum_macros::Display;

use crate::internals::{List};
use crate::lexer::{Token, Position, Lexer};

#[derive(Debug)]
pub enum AstNode {
    // simple values
    Symbol(String),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    // complex values
    List(List<AstNode>),
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            AstNode::Symbol(ref v) => write!(f, "{}", v),
            AstNode::Boolean(v) => write!(f, "{}", v),
            AstNode::Integer(v) => write!(f, "{}", v),
            AstNode::Float(v) => write!(f, "{}", v),
            AstNode::Character(v) => write!(f, "'{}'", v),
            AstNode::String(ref v) => write!(f, "\"{}\"", v),
            AstNode::List(ref v) => write!(f, "{}", v),
        }
    }
}

pub struct ParseError {
    msg: String,
    line: usize,
    column: usize,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "PARSE ERR({}:{}): {}", self.line, self.column, self.msg)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "PARSE ERR({}:{}): {}", self.line, self.column, self.msg)
    }
}

impl StdError for ParseError {
}

macro_rules! parse_error {
    ($pos:ident, $($arg:tt)*) => (
        return Err(ParseError { msg: format!($($arg)*), line: $pos.0, column: $pos.1 })
    )
}

pub struct Parser<'a> {
    tokens: slice::Iter<'a, (Token, Position)>,
}

impl<'a> Parser<'a> {
    fn parse_tree_node(&mut self, depth: usize) -> Result<Option<AstNode>, ParseError> {
        let mut lastpos = (0, 0);
        match self.tokens.next() {
            Some((token, pos)) => {
                lastpos = *pos;
                match *token {
                    Token::LPAREN | Token::LBRACK => {
                        if let Some(inner) = self.parse_tree(depth + 1)? {
                            Ok(Some(inner))
                        } else {
                            if depth == 0 {
                                parse_error!(pos, "unexpected form ()");
                            } else {
                                Ok(Some(AstNode::List(List::Nil)))
                            }
                        }
                    },
                    Token::RPAREN | Token::RBRACK => {
                        if depth > 0 {
                            Ok(None)
                        } else {
                            parse_error!(pos, "unexpected )");
                        }
                    },
                    Token::DOT => {
                        Ok(Some(AstNode::Symbol(".".to_string())))
                    },
                    Token::ELLIPSIS => {
                        Ok(Some(AstNode::Symbol("...".to_string())))
                    },
                    Token::IDENTIFIER(ref v) => {
                        Ok(Some(AstNode::Symbol(v.clone())))
                    },
                    Token::VEC_LPAREN => {
                        let v = AstNode::Symbol("vec".to_string());
                        if let Some(inner) = self.parse_tree(depth + 1)? {
                            let ret: List<AstNode> = list!(v, inner);
                            Ok(Some(AstNode::List(ret)))
                        } else {
                            let ret: List<AstNode> = list!(v);
                            Ok(Some(AstNode::List(ret)))
                        }
                    },
                    Token::CHARACTER(ref v) => {
                        Ok(Some(AstNode::Character(v.clone())))
                    },
                    Token::INTEGER(ref v) => {
                        Ok(Some(AstNode::Integer(v.clone())))
                    },
                    Token::FLOAT(ref v) => {
                        Ok(Some(AstNode::Float(v.clone())))
                    },
                    Token::BOOLEAN(ref v) => {
                        Ok(Some(AstNode::Boolean(v.clone())))
                    },
                    Token::STRING(ref v) => {
                        Ok(Some(AstNode::String(v.clone())))
                    },
                    Token::QUOTE => {
                        match self.parse_tree_node(depth)? {
                            Some(inner) => {
                                let quoted: List<AstNode> = list!(AstNode::Symbol("quote".to_string()), inner);
                                Ok(Some(AstNode::List(quoted)))
                            },
                            _ => {
                                parse_error!(pos, "missing quote value")
                            },
                        }
                    },
                    Token::UNQUOTE => {
                        match self.parse_tree_node(depth)? {
                            Some(inner) => {
                                let unquoted: List<AstNode> = list!(AstNode::Symbol("unquote".to_string()), inner);
                                Ok(Some(AstNode::List(unquoted)))
                            },
                            _ => {
                                parse_error!(pos, "missing unquote value")
                            },
                        }
                    },
                    Token::QUASIQUOTE => {
                        match self.parse_tree_node(depth)? {
                            Some(inner) => {
                                let quoted: List<AstNode> = list!(AstNode::Symbol("quasiquote".to_string()), inner);
                                Ok(Some(AstNode::List(quoted)))
                            },
                            _ => {
                                parse_error!(pos, "missing quasiquote value")
                            },
                        }
                    },
                    Token::UNQUOTE_SPLICING => {
                        match self.parse_tree_node(depth)? {
                            Some(inner) => {
                                let unquoted: List<AstNode> = list!(AstNode::Symbol("unquote-splicing".to_string()), inner);
                                Ok(Some(AstNode::List(unquoted)))
                            },
                            _ => {
                                parse_error!(pos, "missing unquote-splicing value")
                            },
                        }
                    },
                }
            },
            // incomplete s-exp
            _ => {
                if depth > 0 {
                    parse_error!(lastpos, "unexpected eof within s-exp")
                } else {
                    Ok(None)
                }
            },
        }
    }
    
    // always return list !
    fn parse_tree(&mut self, depth: usize) -> Result<Option<AstNode>, ParseError> {
        let mut tree: List<AstNode> = List::Nil;
        let mut v: Vec<AstNode> = Vec::new();
        while let Some(node) = self.parse_tree_node(depth)? {
            v.push(node);
        }
        if !v.is_empty() {
            for i in v.into_iter().rev() {
                tree = List::Cons(Box::new(i), Box::new(tree));
            }
//            println!("@depth={}: {:?}", depth, tree);
            Ok(Some(AstNode::List(tree)))
        } else {
            Ok(None)
        }
    }
    
    // Vec <=> s-exp*
    pub fn parse(tokens: &Vec<(Token, Position)>) -> Result<List<AstNode>, ParseError> {
        let mut parser = Parser { tokens: tokens.iter() };
        let mut v: Vec<AstNode> = Vec::new();
        while let Some(node) = parser.parse_tree_node(0)? {
            v.push(node);
        }
        let a = v.into_iter().collect::<List<AstNode>>();
        Ok(a)
    }
}

mod tests {
    use super::*;
    
    #[test]
    fn test_list_macro() {
        let l: List<i32> = list!();
        assert!(l.is_nil());
        
        let l1: List<i32> = list!(1);
        let mut i1 = l1.iter();
        assert_eq!(i1.next(), Some(&1));

        let l2: List<i32> = list!(2,3);
        let mut i2 = l2.iter();
        assert_eq!(i2.next(), Some(&2));
        assert_eq!(i2.next(), Some(&3));
    }
    
    #[test]
    fn test_parser() {
//        let prog = r#"(define amb-fail '*)"#;
//        let prog = r#"(define amb-fail '*)
//
//(define initialize-amb-fail
//  (lambda ()
//    (set! amb-fail
//      (lambda ()
//        (error "amb tree exhausted")))))"#;
        let prog = r#"(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                         (lambda ()
                           (set! amb-fail +prev-amb-fail)
                           (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)

          (+prev-amb-fail))))))
        "#;
        let tokens = Lexer::tokenize(prog).unwrap();
        let tree = Parser::parse(&tokens);
        assert!(tree.is_ok());
        let t = tree.unwrap();
        println!("{}", t);
        println!("----");
        for i in t.iter() {
            println!("{:?}", i);
            println!("{}", i);
        }
    }
    
    #[test]
    fn test_ast_tree_op() {
        let s = "(lambda () a)";
        let tokens = Lexer::tokenize(s).unwrap();
        let tree = Parser::parse(&tokens);
        assert!(tree.is_ok());
        let tree = tree.unwrap();
        let t = tree.unpack1().unwrap();
        // TODO: extract AstNode::List is struggling, fix it!
        match t {
            AstNode::List(l) => {
//                match *l {
//                    List::Cons(ref x, ref y) => {
//                        println!("x: {:?}", x);
//                        let (a, b) = List::shift(y.clone()).unwrap();
//                        println!("a: {:?}", a);
//                        println!("b: {:?}", b);
//                        // b is List, so body as cdr of tree is always list
//                    },
//                    _ => {},
//                }
                let(_, y) = l.shift().unwrap();
                let (_, b) = y.shift().unwrap();
                println!("b: {:?}", b);
            },
            _ => {},
        }
    }
}
