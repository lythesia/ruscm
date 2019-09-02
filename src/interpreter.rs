use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;
use std::mem;
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

// TODO: err to common lib?
pub struct RuntimeError {
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


#[derive(Clone)]
pub struct Cons(Value, Value);
impl Debug for Cons {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "({:?} . {:?})", self.0, self.1)
    }
}
impl Display for Cons {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "({} . {})", self.0, self.1)
    }
}

impl Cons {
    pub fn set_car(&mut self, v: Value) {
        self.0 = v;
    }

    pub fn set_cdr(&mut self, v: Value) {
        self.1 = v;
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
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

#[derive(Clone)]
pub struct SyntaxRule (
    Vec<String>,   // pattern: list of symbols
    List<Value>,    // body
);

#[derive(Clone)]
pub enum Value {
    Unspecified,
    Symbol(String),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    // Cons & List are both wrap boxes, inner is mutable
    // cons to list: (cons x '())
//    Nil,
    // TODO: think about this ptr, ok?
    Cons(Rc<RefCell<Cons>>), // RefCell to allow modify if we get cons from env, e.g: (define a-cons (car some-complex list)), then (set-car! a-cons xxx)
    List(List<Rc<RefCell<Value>>>), // special form of Cons
    SpecialForm(SpecialForm),
    // primitives (can be override => Procedure)
    Primitive(String),
    // defined lambda
    Procedure(
        Vec<String>,        // params: immutable
        List<Value>,  // body: clone of rc, ast tree struct, not-evaluated, not-(macro)-expanded, always list
        Rc<RefCell<Env>>,   // env
    ),
    Macro(
        String,             // name: for display only
        Vec<String>,        // keywords
        List<SyntaxRule>,   // match arms
        Rc<RefCell<Env>>,   // defining lexical scope(lenv)
    ),
    Continuation(Box<Continuation>),
    // NOTE: only used for constructing ast tree, not an scm object
    DatumList(List<Value>),
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
            Value::Unspecified => write!(f, "#<unspecified>"),
//            Value::Nil => write!(f, "()"),
            Value::Cons(ref v) => write!(f, "{}", v.borrow()),
            Value::List(ref v) => {
                let s = v.iter().map(|i| format!("{}", i.borrow())).collect::<Vec<_>>();
                write!(f, "({})", s.join(" "))
            },
            Value::DatumList(ref v) => write!(f, "#ast{}", v),
            Value::SpecialForm(ref v) => write!(f, "#<special form: {}>", v.to_string()),
            Value::Primitive(ref v) => write!(f, "#<primitive procedure: {}>", v),
            Value::Procedure(_, _, _) => write!(f, "#<procedure>"),
            Value::Macro(ref v, _, _, _) => write!(f, "#<macro: {}>", v),
            Value::Continuation(_) => write!(f, "#<continuation>"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match *self {
            Value::Character(v) => {
                let s = match v {
                    '\n' => "newline".to_string(),
                    '\t' => "tab".to_string(),
                    ' ' => "space".to_string(),
                    _ => v.to_string()
                };
                write!(f, "#\\{}", s)
            },
            Value::String(ref v) => write!(f, "\"{}\"", v),
            Value::Cons(ref v) => write!(f, "{:?}", v.borrow()),
            Value::List(ref v) => {
                let s = v.iter().map(|i| format!("{:?}", i.borrow())).collect::<Vec<_>>();
                write!(f, "({})", s.join(" "))
            },
            Value::DatumList(ref v) => write!(f, "#ast{:?}", v),
            _ => write!(f, "{}", self),
        }
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        match self {
            Value::List(ref v) => v.is_nil(),
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Value::Cons(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Value::List(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_numerical(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            Value::Character(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Value::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self {
            Value::Procedure(_, _, _) | Value::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn is_macro(&self) -> bool {
        match self {
            Value::Macro(_, _, _, _) => true,
            _ => false,
        }
    }

    pub fn to_integer(self) -> Result<i64, RuntimeError> {
        match self {
            Value::Integer(v) => Ok(v),
            _ => runtime_error!("value not integer: {:?}", self),
        }
    }

    pub fn to_float(self) -> Result<f64, RuntimeError> {
        match self {
            Value::Float(v) => Ok(v),
            _ => runtime_error!("value not float: {:?}", self),
        }
    }

    pub fn to_symbol(self) -> Result<String, RuntimeError> {
        match self {
            Value::Symbol(s) => Ok(s),
            _ => runtime_error!("value not symbol: {:?}", self),
        }
    }

    pub fn to_bool(self) -> Result<bool, RuntimeError> {
        match self {
            Value::Boolean(v) => Ok(v),
            _ => runtime_error!("value not boolean: {:?}", self),
        }
    }

    pub fn to_char(self) -> Result<char, RuntimeError> {
        match self {
            Value::Character(v) => Ok(v),
            _ => runtime_error!("value not character: {:?}", self),
        }
    }

    pub fn to_pair(self) -> Result<Rc<RefCell<Cons>>, RuntimeError> {
        match self {
            Value::Cons(v) => Ok(v),
            _ => runtime_error!("value not pair: {:?}", self),
        }
    }

    pub fn to_list(self) -> Result<List<Rc<RefCell<Value>>>, RuntimeError> {
        match self {
            Value::List(v) => Ok(v),
            _ => runtime_error!("value not list: {:?}", self),
        }
    }

    pub fn from_ast_list(self) -> Result<List<Value>, RuntimeError> {
        match self {
            Value::DatumList(l) => Ok(l),
            _ => runtime_error!("value not list: {:?}", self),
        }
    }

    // NOTE: only used for DatumList => List
    pub fn to_value(self) -> Self {
        match self {
            Value::DatumList(l) => {
                let x = l.into_iter().map(|v: Value| rr!(v.to_value())).collect::<List<Rc<RefCell<Value>>>>();
                Value::List(x)
            },
            _ => self,
        }
    }
}

pub fn from_ast_node(i: &AstNode) -> Value {
    match *i {
        AstNode::List(ref l) => Value::DatumList(from_ast_nodes(l)),
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
pub fn from_ast_nodes(l: &List<AstNode>) -> List<Value> {
    let nl = l.iter()
        .map(|x| from_ast_node(x))
        .collect::<List<Value>>();
    nl
}

pub struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    values: HashMap<String, Value>,
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

    // return Value here, but:
    // Cons&List: mutable via inner ptr, so ptr is cloned
    // Procedure: clone body
    // Macro: clone syntax-rule(aka. body)
    // Continuation: clone list of values
    pub fn get(&self, key: &String) -> Option<Value> {
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
    pub fn define(&mut self, key: String, val: Value) { // cons as ref, so rc-refcell here(e.g the value get from env)
        self.values.insert(key, val);
    }

    pub fn set(&mut self, key: String, val: Value) -> Result<(), RuntimeError> {
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

#[derive(Clone)]
pub enum Continuation {
    // top continuation
    Return,
    // eval seq of s-exp, return last val
    EvaluateExpressions(
        List<Value>, // rest exprs
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    ApplyLike(
        List<Value>, // rest exprs
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    EvaluateDefine(
        String, // var-name
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    EvaluateSet(
        String, // var-name
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    EvaluateIf(
        Value, // consequent,
        Value, // alternative,
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    EvaluateApply(
        Value, // proc
        List<Value>, // rest exprs
        List<Value>, // evaluated args
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
//    ContinueMacroExpand(
//        List<Value>, // rest expr
//        List<Value>, // expanded expr
//        Rc<RefCell<Env>>, // defining lexical scope(lenv)
//        Rc<RefCell<Env>>, // invoking lexical scope(renv)
//        Box<Continuation>,
//    ),
}

// utils
// (p1 p2 . p3)
fn verify_lambda_params(params: &Vec<String>) -> Result<(), RuntimeError> {
    let np = params.iter().filter(|&s| s == ".").count();
    if np == 0 {
        Ok(())
    } else if np == 1 {
        if let Some(p) = params.iter().position(|s| s == ".") {
            if p + 1 < params.len() {
                runtime_error!("dot parameter must be the last one")
            } else if p + 1 == params.len() {
                runtime_error!("bad dot parameter form: missing one parameter")
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    } else {
        runtime_error!("only one dot parameter allowed");
    }
}

fn make_lambda(params: Vec<String>, body: List<Value>, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
    verify_lambda_params(&params)?;
    Ok(Value::Procedure(params, body, env))
}

impl Continuation {
    pub fn run(self, val: Value) -> Result<Trampoline, RuntimeError> {
        match self {
            Continuation::EvaluateExpressions(rest, env, next) => {
                if rest.len() == 0 {
                    Ok(Trampoline::Value(val, *next))
                } else {
                    // eval next expressions, don't care val
                    eval_expressions(rest, env, next)
                }
            },
            Continuation::ApplyLike(operands, env, next) => {
                match val {
                    // special form
                    Value::SpecialForm(f) => {
                        match f {
                            // (define var val)
                            // (define (var param ..) body)
                            SpecialForm::Define => {
                                let (car, cdr) = shift_or_error!(operands, "bad define form: at least two arguments");
                                match car {
                                    Value::Symbol(var_name) => {
                                        let var_val = cdr.unpack1()?;
                                        // bounce val to eval
                                        Ok(Trampoline::Bounce(var_val, env.clone(), Continuation::EvaluateDefine(var_name, env, next)))
                                    },
                                    Value::DatumList(l) => {
                                        let (caar, cadr) = shift_or_error!(l, "bad define form: missing procedure name");
                                        let var_name = caar.to_symbol()?;
                                        let params = cadr.into_iter().map(|v: Value| v.to_symbol()).collect::<Result<Vec<String>, _>>()?;
                                        let body = cdr;
                                        // make lambda
                                        let proc = make_lambda(params, body, env.clone())?;
                                        Ok(Trampoline::Bounce(proc, env.clone(), Continuation::EvaluateDefine(var_name, env, next)))
                                    },
                                    _ => runtime_error!("bad define form: not variable name {:?}", car),
                                }
                            },
                            // (set! var val)
                            SpecialForm::Set => {
                                let (var_name, var_val) = operands.unpack2()?;
                                let name = var_name.to_symbol()?;
                                Ok(Trampoline::Bounce(var_val, env.clone(), Continuation::EvaluateSet(name, env, next)))
                            },
                            // (if predicate consequent [alternative])
                            SpecialForm::If => {
                                let (pred, cdr) = shift_or_error!(operands, "bad if form: missing predicate");
                                let (cons, cddr) = shift_or_error!(cdr, "bad if form: missing consequent");
                                if cddr.is_nil() {
                                    Ok(Trampoline::Bounce(pred, env.clone(), Continuation::EvaluateIf(cons, Value::Unspecified, env, next)))
                                } else {
                                    let alt = cddr.unpack1()?;
                                    Ok(Trampoline::Bounce(pred, env.clone(), Continuation::EvaluateIf(cons, alt, env, next)))
                                }
                            },
                            // (lambda params body..)
                            SpecialForm::Lambda => {
                                let (car, cdr) = shift_or_error!(operands, "bad lambda form: missing parameters");
                                if cdr.is_nil() {
                                    runtime_error!("bad lambda form: empty body");
                                }
                                let pl: List<Value> = car.from_ast_list()?;
                                let params = pl.into_iter().map(|v: Value| v.to_symbol()).collect::<Result<Vec<String>, _>>()?;
                                let body = cdr;
                                // make lambda
                                let proc = make_lambda(params, body, env.clone())?;
                                Ok(Trampoline::Value(proc, *next))
                            },
                            // (quote exp)
                            SpecialForm::Quote => {
                                // TODO
                                let quoted = operands.unpack1()?.to_value();
                                Ok(Trampoline::Value(quoted, *next))
                            },
                            // (begin exp ..)
                            SpecialForm::Begin => {
                                let (car, cdr) = shift_or_error!(operands, "bad begin form: at least one statement");
                                Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateExpressions(cdr, env, next)))
                            },
                            _ => Ok(Trampoline::Value(Value::Unspecified, *next))
                        }
                    },
                    // procedure
                    _ => {
                        match operands.shift() {
                            // with args
                            Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(),
                                                                      Continuation::EvaluateApply(val, cdr, List::Nil, env, next))),
                            // w/o args invoke
                            _ => apply(val, List::Nil, next)
                        }
                    }
                }
            },
            Continuation::EvaluateDefine(var_name, env, next) => {
                env.borrow_mut().define(var_name, val);
                Ok(Trampoline::Value(Value::Unspecified, *next))
            },
            Continuation::EvaluateSet(var_name, env, next) => {
                env.borrow_mut().set(var_name, val)?;
                Ok(Trampoline::Value(Value::Unspecified, *next))
            },
            Continuation::EvaluateIf(cons, alt, env, next) => {
                match val {
                    Value::Boolean(false) => Ok(Trampoline::Bounce(cons, env, *next)),
                    _ => Ok(Trampoline::Bounce(alt, env, *next)),
                }
            },
            Continuation::EvaluateApply(f, rest, args, env, next) => {
                let acc = args.unshift_r(val);
                match rest.shift() {
                    // more args
                    Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(),
                                                              Continuation::EvaluateApply(f, cdr, acc, env, next))),
                    // exhaust args
                    _ => apply(f, acc, next),
                }
            },
            Continuation::Return => Ok(Trampoline::Off(val))
        }
    }
}

fn apply(f: Value, args: List<Value>, next: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    match f {
//        Value::Procedure(ref params, ref body, ref env) => {
//        },
//        Value::Primitive() => {
//        },
        // TODO: what is arg for continuation
//        Value::Continuation(k) => Ok(Trampoline::Value(Value::List(??), *k)), // we already in Continuation::run, so we should go back to trampoline to preserve stackless
        _ => runtime_error!("how to apply: {:?}", f),
    }
}

fn call_primitive(f: &str, args: List<Value>) -> Result<Value, RuntimeError> {
    match f {
        "pair?" => call_primitive_predicate(args, f, |v| v.is_pair()),
        "null?" => call_primitive_predicate(args, f, |v| v.is_nil()),
        "list?" => call_primitive_predicate(args, f, |v| v.is_list()),
        "number?" => call_primitive_predicate(args, f, |v| v.is_numerical()),
        "integer?" => call_primitive_predicate(args, f, |v| v.is_integer()),
        "float?" => call_primitive_predicate(args, f, |v| v.is_float()), // non r5rs
        "char?" => call_primitive_predicate(args, f, |v| v.is_char()),
        "boolean?" => call_primitive_predicate(args, f, |v| v.is_boolean()),
        "string?" => call_primitive_predicate(args, f, |v| v.is_string()),
        "symbol?" => call_primitive_predicate(args, f, |v| v.is_symbol()),
        "procedure?" => call_primitive_predicate(args, f, |v| v.is_procedure()),
        "macro?" => call_primitive_predicate(args, f, |v| v.is_macro()),
        "cons" => primitive_cons(args),
        "car" => primitive_car(args),
        "cdr" => primitive_cdr(args),
        "list" => primitive_list(args),
        "set-car!" => primitive_set_car(args),
        "+" => call_primitive_arithmetic(args, f, primitive_plus_i, primitive_plus_f),
        "-" => call_primitive_arithmetic(args, f, primitive_minus_i, primitive_minus_f),
        "*" => call_primitive_arithmetic(args, f, primitive_mul_i, primitive_mul_f),
        "/" => call_primitive_arithmetic(args, f, primitive_div_i, primitive_div_f),
        "=" => call_primitive_arithmetic(args, f, primitive_equ_i, primitive_equ_f),
        "display" => primitive_display(args),
        "newline" => primitive_newline(args),
        "displayln" => primitive_displayln(args),
        "eqv?" => primitive_eqv(args),
        _ => runtime_error!("unknown primitive: `{}'", f),
    }
}

// each fn has its own type ..
fn call_primitive_arithmetic<F, G>(args: List<Value>, name: &str, fi: F, ff: G) -> Result<Value, RuntimeError>
where F: Fn(List<Value>) -> Result<Value, RuntimeError>,
      G: Fn(List<Value>) -> Result<Value, RuntimeError> {
    if args.iter().all(|v| v.is_integer()) {
        fi(args)
    } else if args.iter().all(|v| v.is_float()) {
        ff(args)
    } else {
        runtime_error!("wrong type of args to `{}': {:?}", name, args)
    }
}

fn call_primitive_predicate<F>(args: List<Value>, name: &str, pred: F) -> Result<Value, RuntimeError>
where F: Fn(&Value) -> bool {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `{}': 1 expected, {} got", name, args.len())
    }
    let x = args.unpack1()?;
    let ret = pred(&x);
    Ok(Value::Boolean(ret))
}

fn primitive_cons(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `cons': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    match y {
        Value::List(l) => {
            let ret = Value::List(l.unshift(rr!(x)));
            Ok(ret)
        },
        _ => {
            let ret = Cons(x, y);
            Ok(Value::Cons(rr!(ret)))
        },
    }
}

fn primitive_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `car': 1 expected, {} got", args.len())
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.borrow().0.clone()),
        Value::List(l) => {
            if l.is_nil() {
                runtime_error!("wrong type of arg to `car': {:?}", l)
            }
            let (car, _) = l.shift().unwrap();
            let r = car.borrow();
            Ok(r.clone())
        },
        _ => runtime_error!("wrong type of arg to `car': {:?}", x),
    }
}

fn primitive_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `cdr': 1 expected, {} got", args.len())
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.borrow().1.clone()),
        Value::List(l) => {
            if l.is_nil() {
                runtime_error!("wrong type of arg to `car': {:?}", l)
            }
            let (_, cdr) = l.shift().unwrap();
            Ok(Value::List(cdr))
        },
        _ => runtime_error!("wrong type of arg to `cdr': {:?}", x),
    }
}

fn primitive_set_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `set-car!': 1 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let x = x.to_pair()?;
        x.borrow_mut().set_car(y);
        Ok(Value::Unspecified)
    } else if x.is_list() && !x.is_nil() {
        let x = x.to_list()?;
        let (head, _) = x.shift().unwrap();
        head.as_ref().replace(y);
        // match x {
        //     List::Cons(head, _) => {
        //         (*head).as_ref().replace(y);
        //     },
        //     _ => (),
        // };
        Ok(Value::Unspecified)
    } else {
        runtime_error!("wrong type of 1st arg to `set-car!': {:?}", x)
    }
}

fn primitive_set_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `set-cdr!': 1 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let x = x.to_pair()?;
        x.borrow_mut().set_cdr(y);
        Ok(Value::Unspecified)
    } else if x.is_list() && !x.is_nil() {
        let x = x.to_list()?;
        let (_, tail) = x.shift().unwrap();
        if y.is_list() {
            tail.as_ref().replace(y);
        } else {
            // TODO: cannot transform into cons or list if it's ref-ed by others!
        }
        Ok(Value::Unspecified)
    } else {
        runtime_error!("wrong type of 1st arg to `set-car!': {:?}", x)
    }
}

fn primitive_list(args: List<Value>) -> Result<Value, RuntimeError> {
    let l = args.into_iter().map(|v: Value| rr!(v.to_value())).collect::<List<Rc<RefCell<Value>>>>();
    Ok(Value::List(l))
}

fn fold_values<T, F, H>(args: List<Value>, init: T, op: F, tr: H) -> Result<T, RuntimeError>
where F: Fn(T,T) -> T,
      H: Fn(Value) -> Result<T, RuntimeError> {
    args.into_iter().fold(Ok(init), |acc, x| match acc {
        Ok(s) => Ok(op(s, tr(x)?)),
        _ => acc,
    })
}
fn primitive_plus_i(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 0, |acc, x| acc + x, |x| x.to_integer())?;
    Ok(Value::Integer(r))
}
fn primitive_plus_f(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 0.0, |acc, x| acc + x, |x| x.to_float())?;
    Ok(Value::Float(r))
}
fn primitive_minus_i(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `-': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_integer()? - y.to_integer()?;
    Ok(Value::Integer(r))
}
fn primitive_minus_f(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `-': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_float()? - y.to_float()?;
    Ok(Value::Float(r))
}
fn primitive_mul_i(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 1, |acc, x| acc * x, |x| x.to_integer())?;
    Ok(Value::Integer(r))
}
fn primitive_mul_f(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 1.0, |acc, x| acc * x, |x| x.to_float())?;
    Ok(Value::Float(r))
}
fn primitive_div_i(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `/': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_integer()? / y.to_integer()?;
    Ok(Value::Integer(r))
}
fn primitive_div_f(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `/': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_float()? / y.to_float()?;
    Ok(Value::Float(r))
}
fn primitive_equ_i(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `=': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_integer()? == y.to_integer()?;
    Ok(Value::Boolean(r))
}
fn primitive_equ_f(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `=': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    let r = x.to_float()? == y.to_float()?;
    Ok(Value::Boolean(r))
}

fn primitive_display(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `display': 1 expected, {} got", args.len())
    }
    let x = args.unpack1()?;
    print!("{}", x);
    Ok(Value::Unspecified)
}
fn primitive_newline(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 0 {
        runtime_error!("wrong number of args to `newline': 0 expected, {} got", args.len())
    }
    println!();
    Ok(Value::Unspecified)
}
fn primitive_displayln(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `displayln': 1 expected, {} got", args.len())
    }
    let x = args.unpack1()?;
    println!("{}", x);
    Ok(Value::Unspecified)
}

macro_rules! all_of {
    ($args:ident, $pred:ident) => {
        $args.iter().all(|v| v.$pred())
    };
}

fn primitive_eqv(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `eqv?': 2 expected, {} got", args.len())
    }
    if all_of!(args, is_boolean) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.to_bool()? == y.to_bool()?))
    } else if all_of!(args, is_symbol) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.to_symbol()? == y.to_symbol()?))
    } else if all_of!(args, is_integer) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.to_integer()? == y.to_integer()?))
    } else if all_of!(args, is_char) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.to_char()? == y.to_char()?))
    } else if all_of!(args, is_nil) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(true))
    } else if all_of!(args, is_pair) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(Rc::ptr_eq(&x.to_pair()?, &y.to_pair()?)))
    } else if all_of!(args, is_list) {
        let (x, y) = args.unpack2()?;
        let xx = x.to_list()?;
        let yy = y.to_list()?;
        let r = xx.iter()
            .zip(yy.iter())
            .all(|(l, r)| Rc::ptr_eq(l, r));
        Ok(Value::Boolean(r))
        // TODO: procedures eqv? by define location
    } else {
        Ok(Value::Boolean(false))
    }
}

// TODO: or make it procedures?
pub enum Trampoline {
    // go forward
    Bounce(
        Value,
        Rc<RefCell<Env>>,
        Continuation,
    ),
    // run cont with value
    Value(
        Value,
        Continuation,
    ),
    // quasiquote mode
//    QuasiBounce(
//        Rc<RefCell<Value>>,
//        Rc<RefCell<Env>>,
//        Continuation,
//    ),
//    // macro mode
//    MacroBounce(
//        Rc<RefCell<Value>>,
//        Rc<RefCell<Env>>,
//        Continuation,
//    ),
    Off(Value), // only used by Continuation::Return
}

// eval list of s-exp: expr must be one *complete* s-exp, not an atom
fn eval_expressions(expr: List<Value>, env: Rc<RefCell<Env>>, k: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    match expr.shift() {
        Some((car, cdr)) => Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateExpressions(cdr, env, k))),
        _ => runtime_error!("cannot eval empty list"),
    }
}

// sexp either list or atom
fn process(expr: List<Value>, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
    if expr.len() == 0 {
        Ok(Value::Unspecified)
    } else {
        let mut b = eval_expressions(expr, env, Box::new(Continuation::Return))?;
        loop {
            match b {
                Trampoline::Bounce(next_v, next_e, next_k) => {
                    b = match next_v {
                        Value::DatumList(l) => {
                            match l.shift() {
                                Some((car, cdr)) => Trampoline::Bounce(car, next_e.clone(), Continuation::ApplyLike(cdr, next_e, Box::new(next_k))),
                                _ => runtime_error!("cannot apply empty list"),
                            }
                        },
                        Value::Symbol(ref s) => {
                            let v = match next_e.borrow().get(s) {
                                Some(v) => v,
                                _ => runtime_error!("unbound variable: {}", s),
                            };
                            next_k.run(v)?
                        },
                        _ => next_k.run(next_v.clone())?, // TODO: this arm is same with Trampoline::Value, but value can run with symbol
                    }
                },
                Trampoline::Value(next_v, next_k) => {
                    b = next_k.run(next_v)?;
                },
                Trampoline::Off(next_v) => {
                    return Ok(next_v);
                }
            }
        }
    }
}


mod tests {
    use super::*;

    #[test]
    fn test_value_from() {
        let n1 = Rc::new(AstNode::List(List::Nil));
        let v1 = from_ast_node(n1.as_ref());
        println!("{}", v1);
    }

    #[test]
    fn test_value_from_prog() {
        let prog = r#"(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))"#;
        let tokens = Lexer::tokenize(prog).unwrap();
        let tree = Parser::parse(&tokens).unwrap();
        let eval_ast = from_ast_nodes(&tree);
        println!("{:?}", eval_ast);
        let eval_ast_to_val = Value::DatumList(eval_ast).to_value();
        println!("{:?}", eval_ast_to_val);
    }

    #[test]
    fn test_value_plus() {
        let v: Vec<i64> = vec![1, 2, 3];
        let args = v.into_iter().map(|x| Value::Integer(x)).collect::<List<_>>();
        let ret = primitive_plus_i(args).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 6);
    }

    #[test]
    fn test_value_mul() {
        let v: Vec<i64> = vec![4, 2, 3];
        let args = v.into_iter().map(|x| Value::Integer(x)).collect::<List<_>>();
        let ret = call_primitive("*", args).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 24);
    }

    #[test]
    fn test_value_set_car() {
        let n1 = Value::Integer(1);
        let n2 = Value::Integer(2);
        let n3 = Value::Integer(3);
        let p = Value::Cons(rr!(Cons(n1.clone(), n2.clone())));
        println!("before p:{:?}", p);
        let args1 = vec![p.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret1 = call_primitive("set-car!", args1).unwrap();
        println!("after p:{:?}", p);

        let l = Value::List(vec![rr!(n1.clone()), rr!(n2.clone())].into_iter().collect::<List<_>>());
        println!("before l:{:?}", l);
        let args2 = vec![l.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret2 = call_primitive("set-car!", args2).unwrap();
        println!("after l:{:?}", l);
    }
}
