use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::str::FromStr; // strum

use crate::internals::List;
use crate::lexer::Lexer;
use crate::parser::{Parser, AstNode};

// seems like we need a new List struct to hold ast=>value tree..
// and List itself should be part of value..

macro_rules! rr {
    ($e:expr) => {
        std::rc::Rc::new(std::cell::RefCell::new($e))
    };
}

pub struct Cons(Rc<RefCell<Value>>, Rc<RefCell<Value>>);
impl Debug for Cons {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self.0.borrow() {
            Value::Cons(ref p) => {
                match *self.1.borrow() {
                    Value::Nil => write!(f, "({:?})", p),
                    ref v => write!(f, "({:?}) . {:?}", p, v),
                }
            },
            ref v1 => {
                match *self.1.borrow() {
                    Value::Nil => write!(f, "{:?}", v1),
                    Value::Cons(ref p2) => write!(f, "{:?} {:?}", v1, p2),
                    ref v2 => write!(f, "{:?} . {:?}", v1, v2),
                }
            },
        }
    }
}
impl Display for Cons {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self.0.borrow() {
            Value::Cons(ref p) => {
                match *self.1.borrow() {
                    Value::Nil => write!(f, "({})", p),
                    ref v => write!(f, "({}) . {}", p, v),
                }
            },
            ref v1 => {
                match *self.1.borrow() {
                    Value::Nil => write!(f, "{}", v1),
                    Value::Cons(ref p2) => write!(f, "{} {}", v1, p2),
                    ref v2 => write!(f, "{} . {}", v1, v2),
                }
            },
        }
    }
}

impl Cons {
    pub fn shift(&self) -> (Rc<RefCell<Value>>, Rc<RefCell<Value>>) {
        (self.0.clone(), self.1.clone())
    }
}

#[derive(Debug, Display, Eq, PartialEq)]
#[derive(EnumString)]
#[strum(serialize_all = "mixed_case")]
pub enum SpecialForm {
    Quote,
    Lambda,
    If,
    #[strum(serialize = "set!")]
    Set,
    Begin,
    Quasiquote,
    Define,
    Unquote,
    #[strum(serialize = "unquote-splicing")]
    UnquoteSplicing,
    Eval,
    Apply,
    #[strum(serialize = "define-syntax")]
    DefineSyntax,
    #[strum(serialize = "call/cc")]
    CallCC,
}

pub enum Continuation {
    Return, // top cont
    ContinueMacroExpand(
        List<AstNode>, // rest expr
        List<AstNode>, // expanded expr
        Rc<RefCell<Env>>, // defining lexical scope
        Rc<RefCell<Env>>, // invoking lexical scope
        Box<Continuation>,
    ),
}
pub struct MacroRule (
    List<String>,   // pattern
    List<AstNode>,  // body
);
pub enum Value {
    Symbol(String),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    Nil,
    Cons(Cons), // RefCell to allow modify if we get cons from env, e.g: (define a-cons (car some-complex list)), then (set-car! a-cons xxx)
//    List(), // TODO: special form of Cons?
    SpecialForm(SpecialForm),
    // primitives (can be override)
    Primitive(
        String, // name: for display only
        fn(Cons, Rc<RefCell<Env>>), // handler
    ),
    // defined lambdas
    Procedure(
        Vec<String>,        // params: immutable
        Rc<List<AstNode>>,  // body: clone of rc, ast tree struct, not-evaluated, not-(macro)-expanded, always list
        Rc<RefCell<Env>>,   // env
    ),
    Macro(
        String,             // name: for display only
        Vec<String>,        // keywords
        List<MacroRule>,    // match arms
        Rc<RefCell<Env>>,   // defining lexical scope
    ),
    Continuation(Continuation),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::Boolean(v) => write!(f, "#{}", if v { "t" } else { "f" }),
            Value::Symbol(ref v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Character(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
            Value::Nil => write!(f, "()"),
            Value::Cons(ref v) => write!(f, "({})", v), // this is top cons, we wrap ()
            Value::SpecialForm(ref v) => write!(f, "#<special form: {:?}>", v),
            Value::Primitive(ref v, _) => write!(f, "#<primitive procedure: {}>", v),
            Value::Procedure(_, _, _) => write!(f, "#<procedure>"),
            Value::Macro(ref v, _, _, _) => write!(f, "#<macro: {}>", v),
            Value::Continuation(_) => write!(f, "#<continuation>"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::String(ref v) => write!(f, "\"{}\"", v),
            Value::Cons(ref v) => write!(f, "({:?})", v),
            _ => write!(f, "{}", self),
        }
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }
    
    pub fn is_pair(&self) -> bool {
        match self {
            Value::Cons(_) => true,
            _ => false,
        }
    }
    
    pub fn is_list(v: Rc<RefCell<Self>>) -> bool {
        match *v.borrow() {
            Value::Cons(ref p) => {
                let (_, cdr) = p.shift();
                Value::is_list(cdr)
            },
            Value::Nil => true,
            _ => false,
        }
    }
}

pub fn from_ast_node(i: Rc<AstNode>) -> Value {
    match *i {
        AstNode::List(ref l) => {
            match l {
                List::Nil => Value::Nil,
                List::Cons(_, _) => Value::Cons(from_ast_nodes(l)),
            }
        },
        AstNode::Symbol(ref v) => {
            match SpecialForm::from_str(v.as_str()) {
                Ok(f) => Value::SpecialForm(f),
                _ => match v.as_str() {
                    "λ" => Value::SpecialForm(SpecialForm::Lambda),
                    "call-with-current-continuation" => Value::SpecialForm(SpecialForm::CallCC),
                    _ => Value::Symbol(v.clone()),
                }
            }
        },
        AstNode::String(ref v) => Value::String(v.clone()),
        AstNode::Integer(v) => Value::Integer(v),
        AstNode::Float(v) => Value::Float(v),
        AstNode::Character(v) => Value::Character(v),
        AstNode::Boolean(v) => Value::Boolean(v),
    }
}
pub fn from_ast_nodes(i: &List<AstNode>) -> Cons {
    let (car, cdr) = i.shift().unwrap();
    let x = from_ast_node(car);
    if cdr.is_nil() {
        let y = Value::Nil;
        Cons(rr!(x), rr!(y))
    } else {
        let y = Value::Cons(from_ast_nodes(cdr.as_ref()));
        Cons(rr!(x), rr!(y))
    }
}

pub struct RuntimeError {
    // TODO: err to common lib?
    pub msg: String,
}
impl StdError for RuntimeError {}
impl Debug for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "RUNTIME ERROR: {}", self.msg)
    }
}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "RUNTIME ERROR: {}", self.msg)
    }
}
macro_rules! runtime_error {
    ($($arg:tt)*) => (
        return Err(RuntimeError { msg: format!($($arg)*) })
    )
}

pub struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    values: HashMap<String, Rc<RefCell<Value>>>, // RefCell to allow modify
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        let mut env = Env {
            outer: None,
            values: HashMap::new(),
        };
        // TODO: initialize primitives
        Rc::new(RefCell::new(env))
    }

    pub fn get(&self, key: &String) -> Option<Rc<RefCell<Value>>> {
        match self.values.get(key) {
            Some(v) => Some(v.clone()),
            None => match self.outer {
                Some(ref p) => p.borrow().get(key),
                _ => None,
            },
        }
    }

    // always define in current env
    // always re-define
    pub fn define(&mut self, key: String, val: Rc<RefCell<Value>>) { // cons as ref, so rc-refcell here(e.g the value get from env)
        self.values.insert(key, val);
    }

    pub fn set(&mut self, key: String, val: Rc<RefCell<Value>>) -> Result<(), RuntimeError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, val);
            Ok(())
        } else {
            match self.outer {
                Some(ref p) => p.borrow_mut().set(key, val),
                _ => runtime_error!("set! undefined variable: {:?}", key),
            }
        }
    }

    // TODO: any case we need define via invoke .root env? or we make it member proc
    pub fn root_of(e: &Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        match e.borrow().outer {
            Some(ref p) => Self::root_of(p),
            _ => e.clone(),
        }
    }
}

impl Continuation {
//    pub fn run(self, val: Value) -> Result<Trampoline, RuntimeError> {}
}

// TODO: or make it procedures?
pub enum Trampoline {
    Bounce(
        Rc<AstNode>,    // node or list to be eval
        Rc<RefCell<Env>>,
        Continuation,
    ),
    Value(
        Rc<RefCell<Value>>,
        Continuation,
    ),
    QuasiBounce(
        Rc<AstNode>,    // node or list to be eval
        Rc<RefCell<Env>>,
        Continuation,
    ),
    MacroBounce(
        Rc<AstNode>,    // node or list to be eval
        Rc<RefCell<Env>>,
        Continuation,
    ),
    Off(Rc<RefCell<Value>>),
}

pub struct Interpreter {}

// eval list of s-exp: expr must be one *complete* s-exp, not an atom
//fn eval_expressions(expr: Rc<List<AstNode>>, env: Rc<RefCell<Env>>, k: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
//    match expr.shift() {
//        Ok(car, cdr) => {},
//        _ =>
//    }
//}
//
//fn process(expr: Rc<List<AstNode>>, env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
//    if expr.len() == 0 {
//        runtime_error!("no s-exp to process");
//    } else {
//        let mut bounce = eval_expressions(expr, env, Box::new(Continuation::Return))?;
//        loop {
//            bounce = match bounce {
//                Trampoline::Bounce(a, e, k) => {
//                    match *a {
//                        AstNode::List(ref l) => {
//                            unimplemented!()
//                        },
//                        // keyword => special form
//                        AstNode::Keyword(ref s) => {
//                            unimplemented!()
//                        },
//                        // symbol => lookup env
//                        AstNode::Symbol(ref s) => {
//                            unimplemented!()
//                        },
//                        // atom
//                        _ => k.run()
//                    }
//                },
//                Trampoline::Off(val) => {
//                    return Ok(val);
//                }
//                _ => unimplemented!()
//            }
//        }
//    }
//}

//fn top_process(expr: Rc<List<AstNode>>, env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
//    if expr.len() == 0 {
//        Ok(rr!(Value::Nil)) // this Nil should not handled
//    } else {
//        process(expr, env)
//    }
//}

mod tests {
    use super::*;

    #[test]
    fn test_cons_print() {
        let x: Rc<RefCell<Value>> = rr!(Value::Integer(1));
        let y: Rc<RefCell<Value>> = rr!(Value::Integer(2));
        let z: Rc<RefCell<Value>> = rr!(Value::Integer(3));
        let w: Rc<RefCell<Value>> = rr!(Value::Integer(4));
        let nil: Rc<RefCell<Value>> = rr!(Value::Nil);
        
        
        let l0 = rr!(Value::Cons(Cons(y.clone(), z.clone()))); // (cons 2 3)
        let l1 = rr!(Value::Cons(Cons(x.clone(), l0.clone()))); // (cons 1 (cons 2 3)) => (1 2 . 3)
        println!("{}", l1.borrow());
        assert!(!Value::is_list(l1));
        
        let m0 = rr!(Value::Cons(Cons(x.clone(), y.clone()))); // (cons 1 2)
        let m1 = Value::Cons(Cons(m0.clone(), z.clone())); // (cons (cons 1 2) 3) => ((1 . 2) . 3)
        assert!(!Value::is_list(rr!(m1)));
//        println!("{}", m1);
    
        let l = rr!(Value::Cons(Cons(w.clone(), nil.clone()))); // (cons 4 '())
        let ll = rr!(Value::Cons(Cons(z.clone(), l.clone()))); // (cons 3 l)
        let lll = rr!(Value::Cons(Cons(y.clone(), ll.clone()))); // (cons 2 ll)
        let llll = rr!(Value::Cons(Cons(x.clone(), lll.clone()))); // (cons 1 lll)
        let list = Value::Cons(Cons(nil.clone(), llll.clone())); // (cons '() llll)
        assert!(Value::is_list(rr!(list)));
//        println!("{}", list);
        
        let t0 = rr!(Value::Cons(Cons(x.clone(), y.clone()))); // (cons 1 2)
        let t1 = rr!(Value::Cons(Cons(z.clone(), w.clone()))); // (cons 3 4)
        let t3 = Value::Cons(Cons(t0.clone(), t1.clone())); // (cons (cons 1 2) (cons 3 4))
        assert!(!Value::is_list(rr!(t3)));
//        println!("{}", t3);
    }
    
    #[test]
    fn test_value_from() {
        let n1 = Rc::new(AstNode::List(List::Nil));
        let v1 = from_ast_node(n1);
        println!("{}", v1);
    }
    
    #[test]
    fn test_value_from_comp() {
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
        let tree = Parser::parse(&tokens).unwrap();
        let eval_ast = Value::Cons(from_ast_nodes(&tree));
        println!("{}", eval_ast);
    }
}
