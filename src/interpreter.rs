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
        write!(f, "RUNTIME ERR: {}", self.msg)
    }
}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "RUNTIME ERR: {}", self.msg)
    }
}
macro_rules! runtime_error {
    ($($arg:tt)*) => (
        return Err(RuntimeError { msg: format!($($arg)*) })
    )
}

pub type RRV = Rc<RefCell<Value>>;

#[derive(Clone)]
pub struct Cons(RRV, RRV);

impl Cons {
    // co-recursive with Value::is_list
    pub fn is_list(&self) -> bool {
        self.1.borrow().is_list()
    }
    
    pub fn set_car(&mut self, v: Value) {
        self.0.replace(v);
    }

    pub fn set_cdr(&mut self, v: Value) {
        self.1.replace(v);
    }
}

impl PartialEq for Cons {
    fn eq(&self, other: &Cons) -> bool {
        Rc::ptr_eq(&self.0, &other.0) && Rc::ptr_eq(&self.1, &other.1)
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
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
    // TODO: And, Or
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
    // cons to list: (cons x '())
    Nil,
    Cons(Cons),
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

pub struct ConsIter(RRV);
impl Iterator for ConsIter {
    type Item = RRV;
    fn next(&mut self) -> Option<Self::Item> {
        let x = self.0.clone();
        let rx = x.borrow();
        match &*rx {
            Value::Cons(r) => {
                self.0 = r.1.clone();
                Some(r.0.clone())
            },
            _ => None,
        }
    }
    
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
            Value::Nil => write!(f, "()"),
            Value::Cons(ref v) => {
                if v.is_list() {
                    let s = self.iter().map(|v| format!("{}", v.borrow())).collect::<Vec<String>>();
                    write!(f, "({})", s.join(" "))
                } else {
                    write!(f, "({} . {})", v.0.borrow(), v.1.borrow())
                }
            },
            Value::DatumList(ref v) => write!(f, "#ast{}", v),
            Value::SpecialForm(ref v) => write!(f, "#<special form: {}>", v.to_string()),
            Value::Primitive(ref v) => write!(f, "#<primitive procedure: {}>", v),
            Value::Procedure(ref v, _, _) => write!(f, "#<procedure: [{}]>", v.join(", ")),
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
            Value::Cons(ref v) => {
                if v.is_list() {
                    let s = self.iter().map(|v| format!("{:?}", v.borrow())).collect::<Vec<String>>();
                    write!(f, "({})", s.join(" "))
                } else {
                    write!(f, "({:?} . {:?})", v.0.borrow(), v.1.borrow())
                }
            },
            Value::DatumList(ref v) => write!(f, "#ast{:?}", v),
            _ => write!(f, "{}", self),
        }
    }
}

impl Value {
    pub fn iter(&self) -> ConsIter {
        ConsIter(rr!(self.clone()))
    }
    
    pub fn is_unspecified(&self) -> bool {
        match self {
            Value::Unspecified => true,
            _ => false,
        }
    }
    
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

    // co-recursive with Cons::is_list
    pub fn is_list(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Cons(ref p) => p.is_list(),
            _ => false,
        }
    }
    
    pub fn list_len(&self) -> usize {
        match self {
            Value::Nil => 0,
            Value::Cons(ref p) => {
                if p.is_list() {
                    1 + p.1.borrow().list_len()
                } else {
                    0
                }
            },
            _ => 0,
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

    pub fn to_pair(self) -> Result<Cons, RuntimeError> {
        match self {
            Value::Cons(v) => Ok(v),
            _ => runtime_error!("value not pair: {:?}", self),
        }
    }

    // non-recursive, only transform 1st layer of DatumList to List
    pub fn from_ast_list(self) -> Result<List<Value>, RuntimeError> {
        match self {
            Value::DatumList(l) => Ok(l),
            _ => runtime_error!("value not datum list: {:?}", self),
        }
    }
    
    // recursive
    pub fn to_ast_list(self) -> Value {
        if !self.is_list() {
            self
        } else {
            let l = self.iter().map(|v| v.borrow().clone().to_ast_list()).collect::<List<Value>>();
            Value::DatumList(l)
        }
    }
    
    // non-recursive, only transform 1st layer of ConsList to List
    pub fn to_list(self) -> Result<List<Value>, RuntimeError> {
        if !self.is_list() {
            runtime_error!("value not cons list: {:?}", self);
        }
        let l = self.iter()
            .map(|v| v.borrow().clone())
            .collect::<List<Value>>();
        Ok(l)
    }
    
    // List to Cons struct
    // co-recursive with to_value
    pub fn from_list(lst: List<Value>) -> Value {
        match lst {
            List::Cons(head, tail) => {
                let x = head.to_value();
                let y = Value::from_list(*tail);
                let ret = Cons(rr!(x), rr!(y));
                Value::Cons(ret)
            },
            _ => Value::Nil,
        }
    }

    // co-recursive with from_list
    pub fn to_value(self) -> Self {
        match self {
            Value::DatumList(l) => {
                let v = Value::from_list(l);
                v
            },
            _ => {
                self
            },
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
                    "quote" => Value::SpecialForm(SpecialForm::Quote),
                    "quasiquote" => Value::SpecialForm(SpecialForm::Quasiquote),
                    "unquote" => Value::SpecialForm(SpecialForm::Unquote),
                    "unquote-splicing" => Value::SpecialForm(SpecialForm::UnquoteSplicing),
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
    pub fn root() -> Rc<RefCell<Env>> {
        let primitives = &[
            "pair?",
            "null?",
            "list?",
            "number?",
            "integer?",
            "float?",
            "char?",
            "boolean?",
            "string?",
            "symbol?",
            "procedure?",
            "macro?",
            "cons",
            "car",
            "cdr",
            "list",
            "set-car!",
            "set-cdr!",
            "+",
            "-",
            "*",
            "/",
            "=",
            "<",
            ">",
            "<=",
            ">=",
            "display",
            "newline",
            "displayln",
            "eqv?",
            "error",
        ];
        let map = primitives
            .iter()
            .map(|&v| (v.to_string(), Value::Primitive(v.to_string())))
            .into_iter().collect::<HashMap<_, _>>();
        let mut env = Env {
            outer: None,
            values: map,
        };
        rr!(env)
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
    
    pub fn derive(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        let env = Env {
            outer: Some(parent.clone()),
            values: HashMap::new()
        };
        rr!(env)
    }

    // TODO: any case we need define via invoke .root env? or we make it member proc
    pub fn root_of(e: &Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        match e.borrow().outer {
            Some(ref p) => Self::root_of(p),
            _ => e.clone(),
        }
    }
    
    pub fn values(&self) -> &HashMap<String, Value> {
        &self.values
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
    EvaluateApplyArgs(
        Value,
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    ExecuteApply(
        Value, // proc
        Box<Continuation>,
    ),
    ExecuteEval(
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    ContinueQuasiExpand(
        bool, // if skip (for unquote-splicing)
        List<Value>, // rest exprs
        List<Value>, // acc
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    ExecuteUnQuoteSplicing(
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
            if p + 1 == params.len() {
                runtime_error!("bad dot parameter form: missing last parameter")
            } else if p + 1 < params.len() - 1 {
                runtime_error!("dot parameter must be the last one")
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
    pub fn is_quasiquote_mode(&self) -> bool {
        match self {
            Continuation::ContinueQuasiExpand(_, _, _, _, _) => true,
            _ => false,
        }
    }
    
//    pub fn is_macro_mode(&self) -> bool {
//        match self {
//        }
//    }
    
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
                                        if cdr.len() != 1 {
                                            runtime_error!("bad define form: must supply exactly 1 value to define");
                                        }
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
                                if operands.len() != 1 {
                                    runtime_error!("bad quote form: 1 arguments expected, {} got", operands.len());
                                }
                                let quoted = operands.unpack1()?.to_value(); // to cons list
                                Ok(Trampoline::Value(quoted, *next))
                            },
                            // (begin exp ..)
                            SpecialForm::Begin => {
                                let (car, cdr) = shift_or_error!(operands, "bad begin form: at least one statement");
                                Ok(Trampoline::Bounce(car, env.clone(), Continuation::EvaluateExpressions(cdr, env, next)))
                            },
                            // (apply proc args-as-list)
                            SpecialForm::Apply => {
                                if operands.len() != 2 {
                                    runtime_error!("bad apply form: 2 arguments expected, {} got", operands.len());
                                }
                                let (op, args) = operands.unpack2()?;
                                // now we need to eval op
                                Ok(Trampoline::Bounce(op, env.clone(), Continuation::EvaluateApplyArgs(args, env, next)))
                            },
                            // (eval expr)
                            // for now env-as-arg is not supported, may make env as Value in the future
                            SpecialForm::Eval => {
                                if operands.len() != 1 {
                                    runtime_error!("bad eval form: 1 arguments expected, {} got", operands.len());
                                }
                                let expr = operands.unpack1()?;
                                // fist eval expr to an scm object(datum -> cons), then execute(cons -> datum)
                                Ok(Trampoline::Bounce(expr, env.clone(), Continuation::ExecuteEval(env, next)))
                            },
                            // (quasiquote expr)
                            // NOTE: unquote must be inside quasiquote
                            SpecialForm::Quasiquote => {
                                if operands.len() != 1 {
                                    runtime_error!("bad quasiquote form: 1 arguments expected, {} got", operands.len());
                                }
                                let expr = operands.unpack1()?;
                                match expr {
                                    Value::DatumList(l) => {
                                        match l.shift() {
                                            Some((car, cdr)) => Ok(Trampoline::QuasiBounce(car, env.clone(), Continuation::ContinueQuasiExpand(false, cdr, List::Nil, env, next))),
                                            _ => Ok(Trampoline::Value(Value::Nil, *next))
                                        }
                                    },
                                    _ => Ok(Trampoline::Value(expr, *next))
                                }
                            },
                            // (unquote expr)
                            // NOTE: we do not check if it's inside quasiquote, otherwise it's UB
                            SpecialForm::Unquote => {
                                if operands.len() != 1 {
                                    runtime_error!("bad unquote form: 1 arguments expected, {} got", operands.len());
                                }
                                let expr = operands.unpack1()?;
                                if !next.is_quasiquote_mode() {
                                    runtime_error!("bad unquote form: outside of quasiquote in form (unquote {})", expr);
                                }
                                Ok(Trampoline::Bounce(expr, env, *next))
                            },
                            // (unquote-splicing expr)
                            // NOTE: we do not check if it's inside quasiquote, otherwise it's UB
                            SpecialForm::UnquoteSplicing => {
                                if operands.len() != 1 {
                                    runtime_error!("bad unquote-splicing form: 1 arguments expected, {} got", operands.len());
                                }
                                let expr = operands.unpack1()?;
                                if !next.is_quasiquote_mode() {
                                    runtime_error!("bad unquote form: outside of quasiquote in form (unquote {})", expr);
                                }
                                Ok(Trampoline::Bounce(expr, env.clone(), Continuation::ExecuteUnQuoteSplicing(next)))
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
                    Value::Boolean(false) => Ok(Trampoline::Bounce(alt, env, *next)),
                    _ => Ok(Trampoline::Bounce(cons, env, *next)),
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
            Continuation::EvaluateApplyArgs(args, env, next) => {
                Ok(Trampoline::Bounce(args, env, Continuation::ExecuteApply(val, next)))
            },
            Continuation::ExecuteApply(f, next) => {
                apply(f, val.to_list()?, next)
            },
            Continuation::ExecuteEval(env, next) => {
                // TODO: which env? where the eval appears?
                Ok(Trampoline::Bounce(val.to_ast_list(), env, *next))
            },
            Continuation::ContinueQuasiExpand(skip, rest, acc, env, next) => {
                // cons quasi value
                let acc2 = if skip {acc} else {acc.unshift_r(val)};
                match rest.shift() {
                    // more quasi symbols
                    Some((car, cdr)) => Ok(Trampoline::QuasiBounce(car, env.clone(),
                                                                   Continuation::ContinueQuasiExpand(false, cdr, acc2, env, next))),
                    // exhaust symbols
                    _ => {
                        // list to cons list
                        let cl = Value::from_list(acc2);
                        Ok(Trampoline::Value(cl, *next))
                    },
                }
            },
            Continuation::ExecuteUnQuoteSplicing(next) => {
                if !val.is_list() {
                    runtime_error!("impossible! result of unquote-splicing must be a list");
                }
                let l = val.to_list()?;
                // val is cons list, we need to unpack it
                // next is ContinuaQuasiExpand
                match *next {
                    Continuation::ContinueQuasiExpand(_, rest, acc, env, next_k) => {
                        if l.is_nil() {
                            // skip
                            Ok(Trampoline::Value(Value::Unspecified, Continuation::ContinueQuasiExpand(true, rest, acc, env, next_k)))
                        } else {
                            // merge
                            let new_acc: List<Value> = l.into_iter().fold(acc, |acc2, v| acc2.unshift_r(v));
                            // utilize skip when finish merge
                            Ok(Trampoline::Value(Value::Unspecified, Continuation::ContinueQuasiExpand(true, rest, new_acc, env, next_k)))
                        }
                    },
                    _ => runtime_error!("impossible! unquote-splicing outside of quasiquote"),
                }
            },
            Continuation::Return => Ok(Trampoline::Off(val))
        }
    }
}

fn apply(f: Value, args: List<Value>, next: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    let ff = format!("{:?}", f);
    match f {
        Value::Procedure(params, body, env) => {
            let dot = ".".to_string();
            let new_env = Env::derive(env);
            // params verified
            if params.contains(&dot) {
                let mut it = params.split(|v| v == &dot);
                let norm = it.next().unwrap();
                let rest = &it.next().unwrap()[0];
                let mut i = norm.into_iter();
                let mut j = args.into_iter();
                loop {
                    match (i.next(), j.next()) {
                        (Some(x), Some(y)) => new_env.borrow_mut().define(x.clone(), y),
                        (Some(_), None) => runtime_error!("not enough arguments to procedure: {}", ff),
                        _ => break,
                    }
                }
                let rest_args = j.collect::<List<Value>>();
                new_env.borrow_mut().define(rest.clone(), Value::from_list(rest_args));
            } else {
                if params.len() != args.len() {
                    runtime_error!("must supply exactly {} arguments to procedure: {}", params.len(), ff);
                } else {
                    for (x, y) in params.into_iter().zip(args.into_iter()) {
                        new_env.borrow_mut().define(x, y);
                    }
                }
            }
            eval_expressions(body, new_env, next)
        },
        Value::Primitive(n) => {
            let ret = call_primitive(n.as_str(), args)?;
            Ok(Trampoline::Value(ret, *next))
        },
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
        "set-cdr!" => primitive_set_cdr(args),
        "+" => call_primitive_arithmetic(args, f, primitive_plus_i, primitive_plus_f),
        "-" => call_primitive_arithmetic(args, f, primitive_minus_i, primitive_minus_f),
        "*" => call_primitive_arithmetic(args, f, primitive_mul_i, primitive_mul_f),
        "/" => call_primitive_arithmetic(args, f, primitive_div_i, primitive_div_f),
        "=" => call_primitive_arithmetic(args, f, primitive_eq_i, primitive_eq_f),
        ">" => call_primitive_arithmetic(args, f, primitive_gt_i, primitive_gt_f),
        "<=" => call_primitive_arithmetic(args, f, primitive_le_i, primitive_le_f),
        "<" => call_primitive_arithmetic(args, f, primitive_lt_i, primitive_lt_f),
        ">=" => call_primitive_arithmetic(args, f, primitive_ge_i, primitive_ge_f),
        "display" => primitive_display(args),
        "newline" => primitive_newline(args),
        "displayln" => primitive_displayln(args),
        "error" => primitive_error(args),
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
    let ret = Cons(rr!(x), rr!(y));
    Ok(Value::Cons(ret))
}

fn primitive_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `car': 1 expected, {} got", args.len())
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.0.borrow().clone()),
        _ => runtime_error!("wrong type of arg to `car': {:?}", x),
    }
}

fn primitive_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("wrong number of args to `cdr': 1 expected, {} got", args.len())
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.1.borrow().clone()),
        _ => runtime_error!("wrong type of arg to `cdr': {:?}", x),
    }
}

fn primitive_set_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `set-car!': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let mut p = x.to_pair()?;
        p.set_car(y);
        Ok(Value::Unspecified)
    } else {
        runtime_error!("wrong type of 1st arg to `set-car!': {:?}", x)
    }
}

fn primitive_set_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `set-cdr!': 2 expected, {} got", args.len())
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let mut p = x.to_pair()?;
        p.set_cdr(y);
        Ok(Value::Unspecified)
    } else {
        runtime_error!("wrong type of 1st arg to `set-cdr!': {:?}", x)
    }
}

fn primitive_list(args: List<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::from_list(args))
}

fn fold_values<T, F, H>(args: List<Value>, init: T, op: F, tr: H) -> Result<T, RuntimeError>
where F: Fn(T, T) -> T,
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
fn compare_numeric<T, F, H>(args: List<Value>, f: &str, op: F, tr: H) -> Result<Value, RuntimeError>
where F: Fn(T, T) -> bool,
      H: Fn(Value) -> Result<T, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("wrong number of args to `{}': 2 expected, {} got", f, args.len())
    }
    let (x, y) = args.unpack2()?;
    let ret = op(tr(x)?, tr(y)?);
    Ok(Value::Boolean(ret))
}
fn primitive_eq_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "=", |x, y| x == y, |x| x.to_integer())
}
fn primitive_eq_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "=", |x, y| x == y, |x| x.to_float())
}
fn primitive_gt_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">", |x, y| x > y, |x| x.to_integer())
}
fn primitive_gt_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">", |x, y| x > y, |x| x.to_float())
}
fn primitive_le_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<=", |x, y| x <= y, |x| x.to_integer())
}
fn primitive_le_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<=", |x, y| x <= y, |x| x.to_float())
}
fn primitive_lt_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<", |x, y| x < y, |x| x.to_integer())
}
fn primitive_lt_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<", |x, y| x < y, |x| x.to_float())
}
fn primitive_ge_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">=", |x, y| x >= y, |x| x.to_float())
}
fn primitive_ge_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">=", |x, y| x >= y, |x| x.to_float())
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

fn primitive_error(args: List<Value>) -> Result<Value, RuntimeError> {
    let msg = args.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(" ");
    runtime_error!("{}", msg)
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
        Ok(Value::Boolean(x.to_pair()? == y.to_pair()?))
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
    QuasiBounce(
        Value,
        Rc<RefCell<Env>>,
        Continuation,
    ),
//    // macro mode
//    MacroBounce(
//        Value,
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
pub fn process(ast: &List<AstNode>, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
    if ast.len() == 0 {
        Ok(Value::Unspecified)
    } else {
        let expr = from_ast_nodes(ast);
        let mut b = eval_expressions(expr, env, Box::new(Continuation::Return))?;
        loop {
            match b {
                Trampoline::Bounce(next_v, next_e, next_k) => {
                    b = match next_v {
                        // expr, further eval
                        Value::DatumList(l) => {
                            match l.shift() {
                                Some((car, cdr)) => Trampoline::Bounce(car, next_e.clone(), Continuation::ApplyLike(cdr, next_e, Box::new(next_k))),
                                _ => runtime_error!("cannot apply empty list"),
                            }
                        },
                        // symbol, lookup
                        Value::Symbol(ref s) => {
                            let v = match next_e.borrow().get(s) {
                                Some(v) => v,
                                _ => runtime_error!("unbound variable: {}", s),
                            };
                            next_k.run(v)?
                        },
                        _ => next_k.run(next_v)?, // NOTE: this arm is same with Trampoline::Value, but ::Value can run with symbol
                    }
                },
                Trampoline::Value(next_v, next_k) => {
                    b = next_k.run(next_v)?;
                },
                Trampoline::QuasiBounce(next_v, next_e, next_k) => {
                    b = match next_v {
                        // 1. list: normal list or apply like: unquote, unquote-splicing
                        Value::DatumList(l) => {
                            match l.shift() {
                                Some((car, cdr)) => {
                                    match car {
                                        f @ Value::SpecialForm(SpecialForm::Unquote) | f @ Value::SpecialForm(SpecialForm::UnquoteSplicing) => {
                                            // unquote to val; unquote-splicing to list and then unpack
                                            // bounce to apply-like with form unquote
                                            Trampoline::Bounce(f, next_e.clone(), Continuation::ApplyLike(cdr, next_e, Box::new(next_k)))
                                        },
                                        _ => Trampoline::QuasiBounce(car, next_e.clone(),
                                                                     Continuation::ContinueQuasiExpand(false, cdr, List::Nil, next_e, Box::new(next_k))),
                                    }
                                },
                                _ => next_k.run(Value::Nil)?,
                            }
                        },
                        // 2. symbol: reflect it directly
                        _ => next_k.run(next_v)?,
                    };
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
    fn test_value_cons() {
        let s1 = Value::Symbol("s1".to_string());
        let s2 = Value::Symbol("s2".to_string());
        let s12 = Value::Cons(Cons(rr!(s1), rr!(s2)));
        let s12c = s12.clone();
        println!("s12: {:?}", s12);
        println!("s12c: {:?}", s12c);
        
        let nil = rr!(Value::Nil);
        let n1 = rr!(Value::Integer(1));
        let n2 = rr!(Value::String("int".to_string()));
        let p = Value::Cons(Cons(n1, nil));
        let p2 = Value::Cons(Cons(n2, rr!(p)));
        let p2c = p2.clone();
        println!("p2: {:?}", p2);
        println!("p2c: {:?}", p2c);
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
        let eval_ast_datum = Value::DatumList(eval_ast);
        println!("{:?}", eval_ast_datum);
        let eval_ast_to_val = eval_ast_datum.to_value();
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
    fn test_value_list() {
        let n1 = Value::Integer(1);
        let n2 = Value::Integer(2);
        let n3 = Value::Integer(3);
        let args = vec![n1.clone(), n2.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret = call_primitive("list", args).unwrap();
        println!("ret:{:?}", ret);
    }

    #[test]
    fn test_value_set_car() {
        let nil = Value::Nil;
        let n1 = Value::Integer(1);
        let n2 = Value::Integer(2);
        let n3 = Value::Integer(3);
        let p = Value::Cons(Cons(rr!(n1.clone()), rr!(n2.clone())));
        println!("before p:{:?}", p);
        let args1 = vec![p.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret1 = call_primitive("set-car!", args1).unwrap();
        println!("after p:{:?}", p);

        let l1 = Value::Cons(Cons(rr!(n2), rr!(nil)));
        let l = Value::Cons(Cons(rr!(n1), rr!(l1)));
        println!("before l:{:?}", l);
        let args2 = vec![l.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret2 = call_primitive("set-car!", args2).unwrap();
        println!("after l:{:?}", l);
    }
    
    #[test]
    fn test_value_set_cdr() {
        let nil = Value::Nil;
        let n1 = Value::Integer(1);
        let n2 = Value::Integer(2);
        let n3 = Value::Integer(3);
        let p = Value::Cons(Cons(rr!(n1.clone()), rr!(n2.clone())));
        println!("before p:{:?}", p);
        let args1 = vec![p.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret1 = call_primitive("set-cdr!", args1).unwrap();
        println!("after p:{:?}", p);
        
        let l1 = Value::Cons(Cons(rr!(n2), rr!(nil)));
        let l = Value::Cons(Cons(rr!(n1), rr!(l1)));
        println!("before l:{:?}", l);
        let args2 = vec![l.clone(), n3.clone()].into_iter().collect::<List<_>>();
        let ret2 = call_primitive("set-cdr!", args2).unwrap();
        println!("after l:{:?}", l);
    }
    
    fn exec(prog: &str) -> Result<Value, RuntimeError> {
        let tokens = Lexer::tokenize(prog).unwrap();
        let tree = Parser::parse(&tokens).unwrap();
        process(&tree, Env::root())
    }
    
    fn exec_ok(prog: &str) -> Value {
        exec(prog).unwrap()
    }
    
    #[test]
    fn test_if_1() {
        let prog = "(if (> 1 2) 3 4)";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 4);
    }
    
    #[test]
    fn test_if_2() {
        let prog = "(if ((if (> 5 4) > <) (+ 1 2) 2) (+ 5 7 8) (+ 9 10 11))";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 20);
    }
    
    #[test]
    fn test_if_3() {
        let prog = "(if 0 3 4)";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 3);
    }
    
    #[test]
    fn test_multiple_statements() {
        let prog = "(+ 1 2) (+ 3 4) ; => 7";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.to_integer().unwrap(), 7);
    }
    
    #[test]
    fn test_define() {
        let prog = "(define x 2) (+ x x) ; => 4";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 4);
    }
    
    #[test]
    fn test_set() {
        let prog = "(define x 2) (set! x 3) (+ x x) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 6);
    }
    
    #[test]
    fn test_lambda() {
        let prog = "((lambda (x) (+ x 2)) 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 5);
    }
    
    #[test]
    fn test_lambda_symbol() {
        let prog = "((λ (x) (+ x 2)) 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 5);
    }
    
    #[test]
    fn test_define_func() {
        let prog = "(define (f x) (+ x 2)) (f 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 5);
    }
    
    #[test]
    fn test_define_func2() {
        let prog =  "(define (noop) (+ 0 0)) (define (f x) (noop) (+ x 2)) ((lambda () (f 3))) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 5);
    }
    
    #[test]
    fn test_define_func3() {
        let prog = "(define (plus-or-mul op . args) (apply op args)) (plus-or-mul * 1 2 3) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 6);
    }
    
    #[test]
    fn test_quoting() {
        let prog = "(quote (1 2)) ; => (1 2)";
        let ret = exec_ok(prog);
        println!("{}", ret);
        let p = ret.to_pair().unwrap();
        assert!(p.is_list());
    }
    
    #[test]
    fn test_apply() {
        let prog = "(apply + (quote (1 2 3))) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 6);
    }
    
    #[test]
    fn test_begin() {
        let prog = "(define x 1) (begin (set! x 5) (set! x (+ x 2)) x) ; => 7";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 7);
    }
    
    #[test]
    fn test_eval_1() {
        let prog = "(eval (quote (+ 1 2))) ; => 3";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 3);
    }
    
    #[test]
    fn test_eval_2() {
        let prog = "(define (foo x) (eval (quote (+ 1 2))) x) (foo 5) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 5);
    }
    
    #[test]
    fn test_quasiquoting_1() {
        let prog = "(apply + (quasiquote (2 (unquote (+ 1 2)) 4))) ; => 9";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 9);
    }
    
    #[test]
    fn test_quasiquoting_2() {
        let prog = "(apply + `(2 ,(+ 1 2) ,@(list 1 2 3) 4)) ; => 15";
        let ret = exec_ok(prog);
        assert_eq!(ret.to_integer().unwrap(), 15);
    }
}
