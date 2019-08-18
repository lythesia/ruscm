use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;

use crate::internals::List;
use crate::parser::{AstNode, Keyword};

pub enum Value {
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

pub struct NativeProcedure(
    String, // name
    fn(&List<Value>, Rc<RefCell<Env>>) -> Result<Option<Value>, RuntimeError>,
);
pub struct Procedure(
    List<String>, // params
    List<Value>,  // body
    Rc<RefCell<Env>>,
);

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::Boolean(v) => write!(f, "#{}", if v { "t" } else { "f" }),
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
            _ => write!(f, "{}", self),
        }
    }
}

pub struct RuntimeError {
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

//macro_rules! rr {
//    ($e:expr) => {
//        std::rc::Rc::new(std::cell::RefCell::new($e))
//    };
//}

pub struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    values: HashMap<String, Rc<Value>>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        let mut env = Env {
            outer: None,
            values: HashMap::new(),
        };
        // TODO: initialize
        Rc::new(RefCell::new(env))
    }

    pub fn get(&self, key: &String) -> Option<Rc<Value>> {
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
    pub fn define(&mut self, key: String, val: Value) {
        self.values.insert(key, Rc::new(val));
    }

    pub fn set(&mut self, key: String, val: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&key) {
            self.values.insert(key, Rc::new(val));
            Ok(())
        } else {
            match self.outer {
                Some(ref p) => p.borrow_mut().set(key, val),
                _ => runtime_error!("set! undefined variable: {:?}", key),
            }
        }
    }

    pub fn root(e: &Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        match e.borrow().outer {
            Some(ref p) => Self::root(p),
            _ => e.clone(),
        }
    }
}

pub struct Interpreter {}

fn eval_node(node: &AstNode, env: Rc<RefCell<Env>>) -> Result<Rc<Value>, RuntimeError> {
    match *node {
        AstNode::Boolean(v) => Ok(Rc::new(Value::Boolean(v))),
        AstNode::Character(v) => Ok(Rc::new(Value::Character(v))),
        AstNode::Integer(v) => Ok(Rc::new(Value::Integer(v))),
        AstNode::Float(v) => Ok(Rc::new(Value::Float(v))),
        AstNode::String(ref v) => Ok(Rc::new(Value::String(v.clone()))),
        AstNode::Symbol(ref k) => match env.borrow().get(k) {
            Some(v) => Ok(v),
            _ => runtime_error!("unbound variable: {}", k),
        },
        AstNode::List(ref v) => eval_expr(v, env.clone()),
        AstNode::Keyword(ref v) => {
            let k = v.to_string();
            // keyword => native builtins, lookup from root env => Builtin
            let rootenv = Env::root(&env);
            let r_ = rootenv.borrow();
            match r_.get(&k) {
                Some(v) => Ok(v),
                _ => runtime_error!("impossible w/o {}!", k),
            }
        }
    }
}

fn eval_expr(expr: &List<AstNode>, env: Rc<RefCell<Env>>) -> Result<Rc<Value>, RuntimeError> {
    //    match expr {
    //        &List::Cons(ref head, ref tail) => {
    //            let operator = eval_node(head, env.clone())?;
    //            let operands = tail.iter().map(|e|
    //                eval_node(e, env.clone())?
    //            ).collect::<Vec<_>>(); // TODO: vec or List? now list::fromiterator is implemented
    //            match *operator {
    //                // don't care name here
    //                // TODO
    //                Value::Builtin(NativeProcedure(_, ref f)) =>,
    //                Value::Primitive(NativeProcedure(_, ref f)) =>,
    //                Value::Procedure(Procedure(ref p, ref b, ref e)) =>,
    //                _ => runtime_error!("non standard form or applicable: {:?}", ),
    //            }
    //        },
    //        &List::Nil => Ok(Rc::new(Value::List(List::Nil))),
    //    }
    Ok(Rc::new(Value::List(List::Nil)))
}

mod tests {
    use super::*;
    use std::borrow::Borrow;

    #[test]
    fn test_value_fmt() {
        println!("{}", Value::Character('c'));
    }

    #[test]
    fn test_rc_refcell_match() {
        let s = Value::String("sym".to_string());
        let rs = Rc::new(s);
        match *rs {
            Value::String(ref v) => println!("str: {}", v),
            _ => (),
        }
        println!("{:?}", rs);
    }
}
