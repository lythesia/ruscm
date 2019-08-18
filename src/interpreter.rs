use std::error::Error as StdError;
use std::fmt::{Debug,Display,Formatter,Result as FmtResult};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::parser::{AstNode, Keyword};
use crate::internals::List;

enum Value {
    Symbol(String),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    List(List<Value>),
    Builtin(NativeProcedure),   // natives cannot be re-defined
    Primitive(NativeProcedure), // natives can be re-defined
    Procedure(Procedure),       // defined lambdas
//    Macro(), // TODO
}

struct NativeProcedure(
    String, // name
    fn(&List<Value>, Rc<RefCell<Env>>) -> Result<Option<Value>, RuntimeError>);
struct Procedure(
    List<String>,   // params
    List<Value>,    // body
    Rc<RefCell<Env>>);

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::Boolean(v) => write!(f, "#{}", if v {"t"} else {"f"}),
            Value::Symbol(ref v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Character(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
            Value::List(ref v) => write!(f, "{}", v),
            Value::Builtin(NativeProcedure(ref v, _)) => write!(f, "#<builtin {}>", v),
            Value::Primitive(NativeProcedure(ref v, _)) => write!(f, "#<procedure {}>", v),
            Value::Procedure(_) => write!(f, "#<procedure>"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::String(ref v) => write!(f, "\"{}\"", v),
            Value::List(ref v) => write!(f, "{:?}", v),
            _ => write!(f, "{}", self)
        }
    }
}

struct RuntimeError {
    msg: String,
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

macro_rules! rr {
    ($e:expr) => (
        std::rc::Rc::new(std::cell::RefCell::new($e))
    )
}

pub struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    values: HashMap<String, Rc<RefCell<Value>>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        let mut env = Env { outer: None, values: HashMap::new(), };
        // TODO: initialize
        rr!(env)
    }
    
    pub fn get(&self, key: &String) -> Option<Rc<RefCell<Value>>> {
        match self.values.get(key) {
            Some(v) => Some(v.clone()),
            None => {
                match self.outer {
                    Some(ref p) => p.borrow().get(key),
                    _ => None,
                }
            }
        }
    }
    
    // always define in current env
    // always re-define
    pub fn define(&mut self, key: String, val: Value) {
        self.values.insert(key, rr!(val));
    }
    
    pub fn set(&mut self, key: String, val: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, rr!(val));
            Ok(())
        } else {
            match self.outer {
                Some(ref p) => p.borrow_mut().set(key, val),
                _ => runtime_error!("set! undefined variable: {:?}", key),
            }
        }
    }
    
    pub fn root(&self) -> Rc<RefCell<Env>> {
        match self.outer {
            Some(ref p) => p.borrow().root(),
            _ => self.clone(),
        }
    }
}

pub struct Interpreter {
}

fn eval_node(node: &AstNode, env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
    match *node {
        AstNode::Boolean(v) => Ok(rr!(Value::Boolean(v))),
        AstNode::Character(v) => Ok(rr!(Value::Character(v))),
        AstNode::Integer(v) => Ok(rr!(Value::Integer(v))),
        AstNode::Float(v) => Ok(rr!(Value::Float(v))),
        AstNode::String(ref v) => Ok(rr!(Value::String(v.clone()))),
        AstNode::Symbol(ref k) => {
            match env.borrow().get(k) {
                Some(v) => Ok(v),
                _ => runtime_error!("unbound variable: {}", k),
            }
        },
        AstNode::List(ref v) => eval_expr(v, env.clone()),
        AstNode::Keyword(Keyword(ref k)) => {
            // keyword => native builtins, lookup from root env => Builtin
            let rootenv = env.borrow().root();
            match rootenv.borrow().get(k) {
                Some(v) => Ok(v),
                _ => runtime_error!("impossble w/o {}!", k),
            }
        },
    }
}

fn eval_expr(expr: &List<AstNode>, env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Value>>, RuntimeError> {
    match expr {
        &List::Cons(ref head, ref tail) => {
            let operator = eval_node(head, env.clone())?.borrow();
            match operator {
                Value::Builtin(native) =>,
                Value::Primitive(native) =>,
                Value::Procedure(f) =>,
                _ => runtime_error!("non standard form or applicable: {:?}", ),
            }
        },
        &List::Nil => Ok(rr!(List::Nil)),
    }
}

mod tests {
    use super::*;
    
    #[test]
    fn test_value_fmt() {
        println!("{}", Value::Character('c'));
    }
}