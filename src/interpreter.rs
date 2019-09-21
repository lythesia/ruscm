use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::error::Error as StdError;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::fs;
use std::io::Read;
use std::iter;
use std::mem;
use std::rc::Rc;
use std::str::FromStr; // strum

use crate::internals::{List, RepValue};
use crate::lexer::Lexer;
use crate::parser::{AstNode, Parser};

// seems like we need a new List struct to hold ast=>value tree..
// and List itself should be part of value..

macro_rules! rr {
    ($e:expr) => {
        std::rc::Rc::new(std::cell::RefCell::new($e))
    };
}

macro_rules! all_of {
    ($args:ident, $pred:ident) => {
        $args.iter().all(|v| v.$pred())
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

#[derive(Clone, Debug, Display, PartialEq, EnumString, EnumIter)]
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
    #[strum(serialize = "syntax-rules")]
    SyntaxRules,
    #[strum(serialize = "macro-expand")]
    MacroExpand,
    #[strum(serialize = "call/cc")]
    CallCC,
    Load,
    // TODO: And, Or
}

#[derive(Clone, Debug, PartialEq)]
enum Match {
    Best,
    Normal,
    Not,
}
macro_rules! best_or_error {
    ($pred:expr) => {
        if $pred {
            Ok(Match::Best)
        } else {
            Err(Match::Not)
        }
    };
}
macro_rules! normal_or_error {
    ($pred:expr) => {
        if $pred {
            Ok(Match::Normal)
        } else {
            Err(Match::Not)
        }
    };
}

#[derive(Clone)]
pub struct SyntaxRule(
    List<Value>, // pattern
    List<Value>, // template (as ast)
);
#[derive(Clone)]
pub struct Macro {
    name: String, // TODO: need?
    keywords: Vec<String>,
    syntax_rules: Vec<SyntaxRule>,
    def_env: Rc<RefCell<Env>>,
}

type Matcher = HashMap<String, RepValue<Value>>;
impl Macro {
    // find pattern variables in:
    // 1. patterns/sub-patterns
    // 2. templates, while in template parsing, `keywords` should not take into account
    pub fn pattern_variables(
        pattern: &List<Value>,
        excludes: &Vec<String>,
        includes: &Vec<String>,
    ) -> HashSet<String> {
        let mut ret: HashSet<String> = HashSet::new();
        for v in pattern.iter() {
            match v {
                Value::Symbol(ref s) => {
                    if !excludes.contains(s) && includes.contains(s) {
                        ret.insert(s.clone());
                    }
                }
                Value::DatumList(ref l) => {
                    let r = Self::pattern_variables(l, excludes, includes);
                    ret.extend(r);
                }
                Value::VarValue(ref vv) => {
                    let vv = vv.clone();
                    match *vv {
                        Value::Symbol(s) => {
                            if !excludes.contains(&s) && includes.contains(&s) {
                                ret.insert(s);
                            }
                        }
                        _ => (),
                    }
                }
                Value::VarList(ref l) => {
                    let r = Self::pattern_variables(l, excludes, includes);
                    ret.extend(r);
                }
                _ => (),
            }
        }
        ret
    }

    fn match_value(
        &self,
        pattern: &Value,
        val: &Value,
        run_env: &Rc<RefCell<Env>>,
        depth: usize,
        matcher: &mut Matcher,
    ) -> Result<Match, Match> {
        match pattern {
            // literal
            Value::String(ref v) => best_or_error!(val.is_string_with(v)),
            Value::Character(v) => best_or_error!(val.is_char_with(*v)),
            Value::Boolean(v) => best_or_error!(val.is_boolean_with(*v)),
            Value::Integer(v) => best_or_error!(val.is_integer_with(*v)),
            Value::Float(v) => best_or_error!(val.is_float_with(*v)),
            // keyword | single symbol
            Value::Symbol(ref v) => {
                if self.keywords.contains(v) {
                    best_or_error!(val.is_symbol_with(v) && !run_env.borrow().is_defined(v))
                } else {
                    let o = val.clone();
                    // single
                    if depth == 0 {
                        matcher.insert(v.clone(), RepValue::Val(o));
                        Ok(Match::Best)
                    }
                    // variadic
                    else {
                        if let Some(r) = matcher.get_mut(v) {
                            r.push(o);
                        } else {
                            matcher.insert(v.clone(), RepValue::Repeated(vec![o]));
                        }
                        Ok(Match::Normal)
                    }
                }
            }
            // list (sub-pattern)
            Value::DatumList(ref p) => match val {
                Value::DatumList(ref i) => self.match_list(p, i, run_env, depth, matcher),
                _ => Err(Match::Not),
            },
            _ => Err(Match::Not),
        }
    }

    fn match_list(
        &self,
        pattern: &List<Value>,
        input: &List<Value>,
        run_env: &Rc<RefCell<Env>>,
        depth: usize,
        matcher: &mut Matcher,
    ) -> Result<Match, Match> {
        let np = pattern.len();
        let mut it = input.iter();
        let mut mat = Match::Best;

        for (i, x) in pattern.iter().enumerate() {
            match x {
                // variadic literal | symbol
                // e.g: 2 ... | a ...
                Value::VarValue(ref p) => {
                    if i + 1 != np {
                        return Err(Match::Not);
                    }
                    match p.as_ref() {
                        Value::Symbol(ref s) => {
                            matcher.insert(s.clone(), RepValue::Repeated(Vec::new()));
                        }
                        _ => (),
                    }
                    let rest: Vec<Match> = it
                        .map(|i| self.match_value(p.as_ref(), &i, run_env, depth + 1, matcher))
                        .collect::<Result<Vec<_>, _>>()?;
                    if rest.is_empty() || rest.contains(&Match::Normal) {
                        mat = Match::Normal;
                    }
                    return Ok(mat);
                }
                // variadic list
                // e.g: (xx) ...
                Value::VarList(ref p) => {
                    if i + 1 != np {
                        return Err(Match::Not);
                    }
                    let rest: Vec<Match> = it
                        .map(|i| match i {
                            Value::DatumList(ref l) => {
                                self.match_list(p, l, run_env, depth + 1, matcher)
                            }
                            _ => Err(Match::Not),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    if rest.contains(&Match::Normal) {
                        mat = Match::Normal;
                    }
                    return Ok(mat);
                }
                // literal | symbol
                _ => match it.next() {
                    Some(y) => {
                        if self.match_value(x, y, run_env, depth, matcher)? == Match::Normal {
                            mat = Match::Normal;
                        }
                        continue;
                    }
                    _ => return Err(Match::Not),
                },
            }
        }
        if it.next().is_some() {
            Err(Match::Not)
        } else {
            Ok(mat)
        }
    }

    // only one match allowed
    pub fn rule_matches(
        &self,
        input: &List<Value>,
        run_env: &Rc<RefCell<Env>>,
    ) -> Result<(SyntaxRule, Matcher), RuntimeError> {
        let (bests, normals): (Vec<_>, Vec<_>) = self
            .syntax_rules
            .iter()
            .map(|rule| {
                let mut matcher = Matcher::new();
                let ret = self
                    .match_list(&rule.0, input, run_env, 0, &mut matcher)
                    .map(|m| {
                        //print!("{:?}", m);
                        (rule.clone(), matcher, m)
                    });
                //println!(" {:?} => {:?}", rule.0, input);
                ret
            })
            .filter_map(Result::ok)
            .partition(|(_, _, m)| m == &Match::Best);
        if bests.is_empty() && normals.is_empty() {
            runtime_error!(
                "failed to match any pattern in #<macro: {}>: {:?}",
                self.name,
                input
            );
        }
        let (rule, matcher, _) = if !bests.is_empty() {
            bests.into_iter().next().unwrap()
        } else {
            normals.into_iter().next().unwrap()
        };
        Ok((rule, matcher))
    }

    pub fn rename(&mut self, name: String) {
        self.name = name;
    }
}

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
        Vec<String>,      // params: immutable
        List<Value>, // body: clone of rc, ast tree struct, not-evaluated, not-(macro)-expanded, always list
        Rc<RefCell<Env>>, // env
    ),
    Macro(Macro),
    Continuation(Box<Continuation>),
    // NOTE: only used for constructing ast tree, not an scm object
    DatumList(List<Value>),
    // for marco pattern matching
    VarValue(Box<Value>),
    VarList(List<Value>),
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
            }
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Value::Boolean(v) => write!(f, "#{}", if *v { "t" } else { "f" }),
            Value::Symbol(ref v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Character(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
            Value::Unspecified => write!(f, "#<unspecified>"),
            Value::Nil => write!(f, "()"),
            Value::Cons(ref v) => {
                if v.is_list() {
                    let s = self
                        .iter()
                        .map(|v| format!("{}", v.borrow()))
                        .collect::<Vec<String>>();
                    write!(f, "({})", s.join(" "))
                } else {
                    write!(f, "({} . {})", v.0.borrow(), v.1.borrow())
                }
            }
            Value::DatumList(ref v) => write!(f, "#ast{}", v),
            Value::SpecialForm(ref v) => write!(f, "#<special form: {}>", v.to_string()),
            Value::Primitive(ref v) => write!(f, "#<primitive procedure: {}>", v),
            Value::Procedure(ref v, _, _) => write!(f, "#<procedure: [{}]>", v.join(", ")),
            Value::Macro(ref v) => write!(f, "#<macro: {}>", v.name),
            Value::Continuation(_) => write!(f, "#<continuation>"),
            Value::VarValue(ref v) => write!(f, "v#{} ...", v),
            Value::VarList(ref v) => write!(f, "vl#{} ...", v),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Value::Character(v) => {
                let s = match v {
                    '\n' => "newline".to_string(),
                    '\t' => "tab".to_string(),
                    ' ' => "space".to_string(),
                    _ => v.to_string(),
                };
                write!(f, "#\\{}", s)
            }
            Value::String(ref v) => write!(f, "\"{}\"", v),
            Value::Cons(ref v) => {
                if v.is_list() {
                    let s = self
                        .iter()
                        .map(|v| format!("{:?}", v.borrow()))
                        .collect::<Vec<String>>();
                    write!(f, "({})", s.join(" "))
                } else {
                    write!(f, "({:?} . {:?})", v.0.borrow(), v.1.borrow())
                }
            }
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
            }
            _ => 0,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_integer_with(&self, i: i64) -> bool {
        match self {
            Value::Integer(v) => *v == i,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_float_with(&self, f: f64) -> bool {
        match self {
            Value::Float(v) => *v == f,
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

    pub fn is_string_with(&self, s: &str) -> bool {
        match self {
            Value::String(ref v) => v == s,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            Value::Character(_) => true,
            _ => false,
        }
    }

    pub fn is_char_with(&self, c: char) -> bool {
        match self {
            Value::Character(v) => *v == c,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Value::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol_with(&self, s: &str) -> bool {
        match self {
            Value::Symbol(ref v) => v == s,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean_with(&self, b: bool) -> bool {
        match self {
            Value::Boolean(v) => *v == b,
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
            Value::Macro(_) => true,
            _ => false,
        }
    }

    pub fn is_ast_list(&self) -> bool {
        match self {
            Value::DatumList(_) => true,
            _ => false,
        }
    }

    pub fn as_integer(self) -> Result<i64, RuntimeError> {
        match self {
            Value::Integer(v) => Ok(v),
            _ => runtime_error!("value not integer: {:?}", self),
        }
    }

    pub fn as_float(self) -> Result<f64, RuntimeError> {
        match self {
            Value::Float(v) => Ok(v),
            _ => runtime_error!("value not float: {:?}", self),
        }
    }

    pub fn as_symbol(self) -> Result<String, RuntimeError> {
        match self {
            Value::Symbol(s) => Ok(s),
            _ => runtime_error!("value not symbol: {:?}", self),
        }
    }

    pub fn as_bool(self) -> Result<bool, RuntimeError> {
        match self {
            Value::Boolean(v) => Ok(v),
            _ => runtime_error!("value not boolean: {:?}", self),
        }
    }

    pub fn as_char(self) -> Result<char, RuntimeError> {
        match self {
            Value::Character(v) => Ok(v),
            _ => runtime_error!("value not character: {:?}", self),
        }
    }

    pub fn as_pair(self) -> Result<Cons, RuntimeError> {
        match self {
            Value::Cons(v) => Ok(v),
            _ => runtime_error!("value not pair: {:?}", self),
        }
    }

    pub fn as_string(self) -> Result<String, RuntimeError> {
        match self {
            Value::String(v) => Ok(v),
            _ => runtime_error!("value not string: {:?}", self),
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
            let l = self
                .iter()
                .map(|v| v.borrow().clone().to_ast_list())
                .collect::<List<Value>>();
            Value::DatumList(l)
        }
    }

    // non-recursive, only transform 1st layer of ConsList to List
    pub fn to_list(self) -> Result<List<Value>, RuntimeError> {
        if !self.is_list() {
            runtime_error!("value not cons list: {:?}", self);
        }
        let l = self
            .iter()
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
            }
            _ => Value::Nil,
        }
    }

    // co-recursive with from_list
    pub fn to_value(self) -> Self {
        match self {
            Value::DatumList(l) => {
                let v = Value::from_list(l);
                v
            }
            _ => self,
        }
    }
}

pub fn from_ast_node(i: &AstNode) -> Value {
    match *i {
        AstNode::List(ref l) => Value::DatumList(from_ast_nodes(l)),
        AstNode::Symbol(ref v) => match SpecialForm::from_str(v.as_str()) {
            Ok(f) => Value::SpecialForm(f),
            _ => match v.as_str() {
                "λ" => Value::SpecialForm(SpecialForm::Lambda),
                "quote" => Value::SpecialForm(SpecialForm::Quote),
                "quasiquote" => Value::SpecialForm(SpecialForm::Quasiquote),
                "unquote" => Value::SpecialForm(SpecialForm::Unquote),
                "unquote-splicing" => Value::SpecialForm(SpecialForm::UnquoteSplicing),
                "call-with-current-continuation" => Value::SpecialForm(SpecialForm::CallCC),
                _ => Value::Symbol(v.clone()),
            },
        },
        AstNode::String(ref v) => Value::String(v.clone()),
        AstNode::Integer(v) => Value::Integer(v),
        AstNode::Float(v) => Value::Float(v),
        AstNode::Character(v) => Value::Character(v),
        AstNode::Boolean(v) => Value::Boolean(v),
    }
}
pub fn from_ast_nodes(l: &List<AstNode>) -> List<Value> {
    let mut prev: Option<Value> = None;
    let mut nl: Vec<Value> = Vec::new();
    for i in l.iter().map(|x| from_ast_node(x)) {
        let p = prev.replace(i.clone());
        if i.is_symbol_with("...") && p.is_some() {
            let x = p.unwrap();
            let var = match x {
                Value::DatumList(l) => Value::VarList(l.clone()),
                v => Value::VarValue(Box::new(v.clone())),
            };
            nl.pop();
            nl.push(var);
        } else {
            nl.push(i.clone());
        }
    }
    nl.into_iter().collect::<List<Value>>()
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
            "zero?",
            "display",
            "newline",
            "displayln",
            "eqv?",
            "error",
        ];
        let map = primitives
            .iter()
            .map(|&v| (v.to_string(), Value::Primitive(v.to_string())))
            .into_iter()
            .collect::<HashMap<_, _>>();
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

    pub fn is_defined(&self, key: &String) -> bool {
        if self.values.contains_key(key) {
            true
        } else {
            match self.outer {
                Some(ref p) => p.borrow().is_defined(key),
                _ => false,
            }
        }
    }

    // always define in current env
    // always re-define
    pub fn define(&mut self, key: String, val: Value) {
        // cons as ref, so rc-refcell here(e.g the value get from env)
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
            values: HashMap::new(),
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
        Value,       // proc
        List<Value>, // rest exprs
        List<Value>, // evaluated args
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    EvaluateApplyArgs(Value, Rc<RefCell<Env>>, Box<Continuation>),
    ExecuteApply(
        Value, // proc
        Box<Continuation>,
    ),
    ExecuteEval(Rc<RefCell<Env>>, Box<Continuation>),
    ContinueQuasiExpand(
        bool,        // if skip (for unquote-splicing)
        List<Value>, // rest exprs
        List<Value>, // acc
        Rc<RefCell<Env>>,
        Box<Continuation>,
    ),
    ExecuteUnQuoteSplicing(Box<Continuation>),
    EvaluateMacroDefine(
        String,           // macro-name
        Rc<RefCell<Env>>, // lenv
        Box<Continuation>,
    ),
    ExecuteMacroExpand(List<Value>, Rc<RefCell<Env>>, Box<Continuation>),
    ExecuteCallCC(Box<Continuation>),
}

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

fn make_lambda(
    params: Vec<String>,
    body: List<Value>,
    env: Rc<RefCell<Env>>,
) -> Result<Value, RuntimeError> {
    verify_lambda_params(&params)?;
    Ok(Value::Procedure(params, body, env))
}

fn verify_macro_syntax(pattern: &List<Value>, depth: usize) -> Result<(), RuntimeError> {
    for p in pattern.iter() {
        match p {
            Value::VarValue(_) | Value::VarList(_) => {
                if depth > 0 {
                    runtime_error!(
                        "nested ellipsis(...) not supported in macro pattern: {:?}",
                        pattern
                    );
                }
            }
            _ => (),
        }
    }
    Ok(())
}
// end utils

impl Continuation {
    pub fn is_quasiquote_mode(&self) -> bool {
        match self {
            Continuation::ContinueQuasiExpand(_, _, _, _, _) => true,
            _ => false,
        }
    }

    pub fn is_macrodef_mode(&self) -> bool {
        match self {
            Continuation::EvaluateMacroDefine(_, _, _) => true,
            _ => false,
        }
    }

    pub fn run(self, val: Value) -> Result<Trampoline, RuntimeError> {
        match self {
            Continuation::EvaluateExpressions(rest, env, next) => {
                if rest.len() == 0 {
                    Ok(Trampoline::Value(val, *next))
                } else {
                    // eval next expressions, don't care val
                    eval_expressions(rest, env, next)
                }
            }
            Continuation::ApplyLike(operands, env, next) => {
                match val {
                    // special form
                    Value::SpecialForm(f) => {
                        match f {
                            // (define var val)
                            // (define (var param ..) body)
                            SpecialForm::Define => {
                                let (car, cdr) = shift_or_error!(
                                    operands,
                                    "bad define form: at least two arguments"
                                );
                                match car {
                                    Value::Symbol(var_name) => {
                                        if cdr.len() != 1 {
                                            runtime_error!("bad define form: must supply exactly 1 value to define");
                                        }
                                        let var_val = cdr.unpack1()?;
                                        // bounce val to eval
                                        Ok(Trampoline::Bounce(
                                            var_val,
                                            env.clone(),
                                            Continuation::EvaluateDefine(var_name, env, next),
                                        ))
                                    }
                                    Value::DatumList(l) => {
                                        let (caar, cadr) = shift_or_error!(
                                            l,
                                            "bad define form: missing procedure name"
                                        );
                                        let var_name = caar.as_symbol()?;
                                        let params = cadr
                                            .into_iter()
                                            .map(|v: Value| v.as_symbol())
                                            .collect::<Result<Vec<String>, _>>()?;
                                        let body = cdr;
                                        // make lambda
                                        let proc = make_lambda(params, body, env.clone())?;
                                        Ok(Trampoline::Bounce(
                                            proc,
                                            env.clone(),
                                            Continuation::EvaluateDefine(var_name, env, next),
                                        ))
                                    }
                                    _ => runtime_error!(
                                        "bad define form: not variable name {:?}",
                                        car
                                    ),
                                }
                            }
                            // (set! var val)
                            SpecialForm::Set => {
                                if operands.len() != 2 {
                                    runtime_error!(
                                        "bad set! form: 2 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let (var_name, var_val) = operands.unpack2()?;
                                let name = var_name.as_symbol()?;
                                Ok(Trampoline::Bounce(
                                    var_val,
                                    env.clone(),
                                    Continuation::EvaluateSet(name, env, next),
                                ))
                            }
                            // (if predicate consequent [alternative])
                            SpecialForm::If => {
                                let (pred, cdr) =
                                    shift_or_error!(operands, "bad if form: missing predicate");
                                let (cons, cddr) =
                                    shift_or_error!(cdr, "bad if form: missing consequent");
                                if cddr.is_nil() {
                                    Ok(Trampoline::Bounce(
                                        pred,
                                        env.clone(),
                                        Continuation::EvaluateIf(
                                            cons,
                                            Value::Unspecified,
                                            env,
                                            next,
                                        ),
                                    ))
                                } else {
                                    let alt = cddr.unpack1()?;
                                    Ok(Trampoline::Bounce(
                                        pred,
                                        env.clone(),
                                        Continuation::EvaluateIf(cons, alt, env, next),
                                    ))
                                }
                            }
                            // (lambda params body..)
                            SpecialForm::Lambda => {
                                let (car, cdr) = shift_or_error!(
                                    operands,
                                    "bad lambda form: missing parameters"
                                );
                                if cdr.is_nil() {
                                    runtime_error!("bad lambda form: empty body");
                                }
                                let pl: List<Value> = car.from_ast_list()?;
                                let params = pl
                                    .into_iter()
                                    .map(|v: Value| v.as_symbol())
                                    .collect::<Result<Vec<String>, _>>()?;
                                let body = cdr;
                                // make lambda
                                let proc = make_lambda(params, body, env.clone())?;
                                Ok(Trampoline::Value(proc, *next))
                            }
                            // (quote exp)
                            SpecialForm::Quote => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad quote form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let quoted = operands.unpack1()?.to_value(); // to cons list
                                Ok(Trampoline::Value(quoted, *next))
                            }
                            // (begin exp ..)
                            SpecialForm::Begin => {
                                let (car, cdr) = shift_or_error!(
                                    operands,
                                    "bad begin form: at least one statement"
                                );
                                Ok(Trampoline::Bounce(
                                    car,
                                    env.clone(),
                                    Continuation::EvaluateExpressions(cdr, env, next),
                                ))
                            }
                            // (apply proc args-as-list)
                            SpecialForm::Apply => {
                                if operands.len() != 2 {
                                    runtime_error!(
                                        "bad apply form: 2 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let (op, args) = operands.unpack2()?;
                                // now we need to eval op
                                Ok(Trampoline::Bounce(
                                    op,
                                    env.clone(),
                                    Continuation::EvaluateApplyArgs(args, env, next),
                                ))
                            }
                            // (eval expr)
                            // for now env-as-arg is not supported, may make env as Value in the future
                            SpecialForm::Eval => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad eval form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let expr = operands.unpack1()?;
                                // fist eval expr to an scm object(datum -> cons), then execute(cons -> datum)
                                Ok(Trampoline::Bounce(
                                    expr,
                                    env.clone(),
                                    Continuation::ExecuteEval(env, next),
                                ))
                            }
                            // (quasiquote expr)
                            // NOTE: unquote must be inside quasiquote
                            SpecialForm::Quasiquote => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad quasiquote form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let expr = operands.unpack1()?;
                                match expr {
                                    Value::DatumList(l) => match l.shift() {
                                        Some((car, cdr)) => Ok(Trampoline::QuasiBounce(
                                            car,
                                            env.clone(),
                                            Continuation::ContinueQuasiExpand(
                                                false,
                                                cdr,
                                                List::Nil,
                                                env,
                                                next,
                                            ),
                                        )),
                                        _ => Ok(Trampoline::Value(Value::Nil, *next)),
                                    },
                                    _ => Ok(Trampoline::Value(expr, *next)),
                                }
                            }
                            // (unquote expr)
                            // NOTE: we do not check if it's inside quasiquote, otherwise it's UB
                            SpecialForm::Unquote => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad unquote form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let expr = operands.unpack1()?;
                                if !next.is_quasiquote_mode() {
                                    runtime_error!("bad unquote form: outside of quasiquote in form (unquote {})", expr);
                                }
                                Ok(Trampoline::Bounce(expr, env, *next))
                            }
                            // (unquote-splicing expr)
                            // NOTE: we do not check if it's inside quasiquote, otherwise it's UB
                            SpecialForm::UnquoteSplicing => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad unquote-splicing form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let expr = operands.unpack1()?;
                                if !next.is_quasiquote_mode() {
                                    runtime_error!("bad unquote form: outside of quasiquote in form (unquote {})", expr);
                                }
                                Ok(Trampoline::Bounce(
                                    expr,
                                    env.clone(),
                                    Continuation::ExecuteUnQuoteSplicing(next),
                                ))
                            }
                            // (define-syntax macro
                            //   ..
                            // )
                            SpecialForm::DefineSyntax => {
                                if operands.len() != 2 {
                                    runtime_error!(
                                        "bad define-syntax form: 2 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let (fst, snd) = operands.unpack2()?;
                                let mut bad_macro_msg =
                                    format!("bad define-syntax form: invalid macro body {:?}", snd);
                                match fst {
                                    // snd must be (syntax-rules () ..)
                                    Value::Symbol(macro_name) => {
                                        let macro_body = match snd.from_ast_list() {
                                            Ok(v) => v,
                                            Err(_) => runtime_error!("{}", bad_macro_msg),
                                        };
                                        if macro_body.len() < 2 {
                                            runtime_error!("{}", bad_macro_msg);
                                        }
                                        // (sytanx-rules (keywords ..) ..)
                                        let (car, cdr) = macro_body.shift().unwrap();
                                        match car {
                                            f @ Value::SpecialForm(SpecialForm::SyntaxRules) => {
                                                let reform = Value::DatumList(cdr.unshift(f));
                                                Ok(Trampoline::Bounce(reform, env.clone(),
                                                                      Continuation::EvaluateMacroDefine(macro_name, env, next)))
                                            },
                                            _ => runtime_error!("bad define-syntax form: must be followed by syntax-rules"),
                                        }
                                    }
                                    _ => runtime_error!(
                                        "bad define-syntax form: not variable name {:?}",
                                        fst
                                    ),
                                }
                            }
                            // (syntax-rules (keywords ..)
                            //   (pattern template)
                            //   ..
                            // )
                            // ; pattern = (pat ...) | (pat pat ... pat) | (pat ... pat ellipsis)
                            // ; template = (elem ...) | (elem elem ... tmpl) |
                            // 1. pat cannot be ellipsis; 2. ellipsis must be the last of enclosing list; 3. dot not allowed
                            // 4. e ... match 0 or more; 5. nested ... not supported
                            SpecialForm::SyntaxRules => {
                                if !next.is_macrodef_mode() {
                                    runtime_error!(
                                        "bad syntax-rules form: outside of define-syntax form {:?}",
                                        operands
                                    );
                                }
                                let (car, cdr) = shift_or_error!(
                                    operands,
                                    "bad syntax-rules form: at least 1 arguments expected, 0 got"
                                );
                                // 1. keywords
                                let mut bad_macro_msg = format!("{:?}", car);
                                let keywords: Vec<String> = match car.from_ast_list() {
                                    Ok(v) => {
                                        if all_of!(v, is_symbol) {
                                            v.into_iter().map(|v| v.as_symbol()).collect::<Result<
                                                Vec<String>,
                                                _,
                                            >>(
                                            )?
                                        } else {
                                            runtime_error!("bad syntax-rules form: keywords must be all symbols {}", bad_macro_msg)
                                        }
                                    }
                                    Err(_) => runtime_error!(
                                        "bad syntax-rules form: keywords must be list {}",
                                        bad_macro_msg
                                    ),
                                };
                                let ellipsis = String::from("...");
                                if keywords.contains(&ellipsis) {
                                    runtime_error!(
                                        "bad syntax-rules form: ellipsis not allowed in keywords"
                                    );
                                }
                                // 2. rules
                                // note: when expansion, we should first eval to ast, then eval it
                                let mut rules: Vec<SyntaxRule> = Vec::new();
                                for v in cdr.into_iter() {
                                    let msg = format!("{:?}", v);
                                    match v.from_ast_list() {
                                        Ok(l) => {
                                            if l.len() != 2 {
                                                runtime_error!("bad syntax-rules form: rule must be list(# = 2) {}", msg);
                                            } else {
                                                let (pat, tmpl) = l.shift().unwrap(); // make tmpl as expressions
                                                let pat: List<Value> = pat.from_ast_list()?; // to_list
                                                                                             // _ placeholder for macro name, omit it
                                                let (_, pat) = shift_or_error!(pat, "bad syntax-rule pattern: must supply at least 1 pattern argument(aka. macro itself)");
                                                // verify
                                                verify_macro_syntax(&pat, 0)?;
                                                verify_macro_syntax(&tmpl, 0)?;
                                                //                                                println!("rule pattern: {:?}", pat);
                                                //                                                println!("rule template: {:?}", tmpl);
                                                rules.push(SyntaxRule(pat, tmpl))
                                            }
                                        }
                                        Err(_) => runtime_error!(
                                            "bad syntax-rules form: rule must be list(# = 2) {}",
                                            msg
                                        ),
                                    }
                                }
                                // temporary macro object
                                let makro = Macro {
                                    name: String::from("#macro"),
                                    keywords,
                                    syntax_rules: rules,
                                    def_env: env,
                                };
                                Ok(Trampoline::Value(Value::Macro(makro), *next))
                            }
                            // (macro-expand macro ...)
                            SpecialForm::MacroExpand => {
                                let (m, input) = shift_or_error!(
                                    operands,
                                    "bad macro-expand form: at least 1 argument expect"
                                );
                                Ok(Trampoline::Bounce(
                                    m,
                                    env.clone(),
                                    Continuation::ExecuteMacroExpand(input, env, next),
                                ))
                            }
                            // (call/cc proc)
                            // where proc == (lambda (k) ...)
                            SpecialForm::CallCC => {
                                if operands.len() != 1 {
                                    runtime_error!(
                                        "bad call/cc form: 1 arguments expected, {} got",
                                        operands.len()
                                    );
                                }
                                let f = operands.unpack1()?;
                                Ok(Trampoline::Bounce(
                                    f,
                                    env.clone(),
                                    Continuation::ExecuteCallCC(next),
                                ))
                            }
                            // (load "xx" ...)
                            SpecialForm::Load => {
                                if operands.len() == 0 {
                                    runtime_error!(
                                        "wrong number of operands to `load`: at least 1 argument"
                                    );
                                }
                                if !all_of!(operands, is_string) {
                                    runtime_error!(
                                        "wrong type of operands to `load`(must be string): {:?}",
                                        operands
                                    );
                                }
                                for i in operands.iter() {
                                    let path = i.clone().as_string()?;
                                    match fs::OpenOptions::new().read(true).open(&path) {
                                        Ok(mut file) => {
                                            let mut src = String::new();
                                            file.read_to_string(&mut src);
                                            let prog = src.as_str();
                                            let tokens = Lexer::tokenize(prog).unwrap();
                                            let tree = Parser::parse(&tokens).unwrap();
                                            if process(&tree, env.clone()).is_err() {
                                                runtime_error!("error loading {}", path);
                                                break;
                                            }
                                        }
                                        Err(e) => runtime_error!("open `{}' failed: {}", path, e),
                                    }
                                }
                                Ok(Trampoline::Value(Value::Unspecified, *next))
                            }
                            _ => Ok(Trampoline::Value(Value::Unspecified, *next)),
                        }
                    }
                    // macro
                    Value::Macro(makro) => {
                        let expr = macro_expand(&makro, &operands, &env)?;
                        eval_expressions(expr, env, next)
                    }
                    // procedure
                    _ => {
                        match operands.shift() {
                            // with args
                            Some((car, cdr)) => Ok(Trampoline::Bounce(
                                car,
                                env.clone(),
                                Continuation::EvaluateApply(val, cdr, List::Nil, env, next),
                            )),
                            // w/o args invoke
                            _ => apply(val, List::Nil, next),
                        }
                    }
                }
            }
            Continuation::EvaluateDefine(var_name, env, next) => {
                env.borrow_mut().define(var_name, val);
                Ok(Trampoline::Value(Value::Unspecified, *next))
            }
            Continuation::EvaluateSet(var_name, env, next) => {
                if val.is_macro() {
                    runtime_error!("macro cannot be set: {:?}", val);
                }
                env.borrow_mut().set(var_name, val)?;
                Ok(Trampoline::Value(Value::Unspecified, *next))
            }
            Continuation::EvaluateIf(cons, alt, env, next) => match val {
                Value::Boolean(false) => Ok(Trampoline::Bounce(alt, env, *next)),
                _ => Ok(Trampoline::Bounce(cons, env, *next)),
            },
            Continuation::EvaluateApply(f, rest, args, env, next) => {
                let acc = args.unshift_r(val);
                match rest.shift() {
                    // more args
                    Some((car, cdr)) => Ok(Trampoline::Bounce(
                        car,
                        env.clone(),
                        Continuation::EvaluateApply(f, cdr, acc, env, next),
                    )),
                    // exhaust args
                    _ => apply(f, acc, next),
                }
            }
            Continuation::EvaluateApplyArgs(args, env, next) => Ok(Trampoline::Bounce(
                args,
                env,
                Continuation::ExecuteApply(val, next),
            )),
            Continuation::ExecuteApply(f, next) => apply(f, val.to_list()?, next),
            Continuation::ExecuteEval(env, next) => {
                // TODO: which env? where the eval appears?
                Ok(Trampoline::Bounce(val.to_ast_list(), env, *next))
            }
            Continuation::ContinueQuasiExpand(skip, rest, acc, env, next) => {
                // cons quasi value
                let acc2 = if skip { acc } else { acc.unshift_r(val) };
                match rest.shift() {
                    // more quasi symbols
                    Some((car, cdr)) => Ok(Trampoline::QuasiBounce(
                        car,
                        env.clone(),
                        Continuation::ContinueQuasiExpand(false, cdr, acc2, env, next),
                    )),
                    // exhaust symbols
                    _ => {
                        // list to cons list
                        let cl = Value::from_list(acc2);
                        Ok(Trampoline::Value(cl, *next))
                    }
                }
            }
            Continuation::ExecuteUnQuoteSplicing(next) => {
                if !val.is_list() {
                    runtime_error!("impossible! result of unquote-splicing must be a list");
                }
                let l = val.to_list()?;
                // val is cons list, we need to unpack it
                // next is ContinueQuasiExpand
                match *next {
                    Continuation::ContinueQuasiExpand(_, rest, acc, env, next_k) => {
                        if l.is_nil() {
                            // skip
                            Ok(Trampoline::Value(
                                Value::Unspecified,
                                Continuation::ContinueQuasiExpand(true, rest, acc, env, next_k),
                            ))
                        } else {
                            // merge
                            let new_acc: List<Value> =
                                l.into_iter().fold(acc, |acc2, v| acc2.unshift_r(v));
                            // utilize skip when finish merge
                            Ok(Trampoline::Value(
                                Value::Unspecified,
                                Continuation::ContinueQuasiExpand(true, rest, new_acc, env, next_k),
                            ))
                        }
                    }
                    _ => runtime_error!("impossible! unquote-splicing outside of quasiquote"),
                }
            }
            Continuation::EvaluateMacroDefine(name, env, next) => match val {
                Value::Macro(mut makro) => {
                    makro.rename(name.clone());
                    let val = Value::Macro(makro);
                    env.borrow_mut().define(name, val);
                    Ok(Trampoline::Value(Value::Unspecified, *next))
                }
                _ => runtime_error!("value not macro: {:?}", val),
            },
            Continuation::ExecuteMacroExpand(exprs, env, next) => match val {
                Value::Macro(makro) => {
                    let expr = macro_expand(&makro, &exprs, &env)?;
                    Ok(Trampoline::Value(Value::DatumList(expr), *next))
                }
                _ => runtime_error!("value not macro: {:?}", val),
            },
            Continuation::ExecuteCallCC(next) => {
                let k = Value::Continuation(next.clone());
                // val is lambda
                apply(val, list!(k), next)
            }
            Continuation::Return => Ok(Trampoline::Off(val)),
        }
    }
}

fn transform_repeated(
    expr: &Value,
    replace: &HashMap<String, Vec<Value>>,
    n: usize,
    lenv: &Rc<RefCell<Env>>,
    renv: &Rc<RefCell<Env>>,
    renamed: &mut HashMap<String, String>,
) -> Result<Vec<Value>, RuntimeError> {
    // expr can only be: symbol | literal | list
    match expr {
        Value::Symbol(ref s) => {
            match replace.get(s) {
                Some(v) => {
                    if v.len() == n {
                        Ok(v.clone())
                    } else {
                        runtime_error!("failed to expand macro: # of {:?} must be {}", expr, n);
                    }
                }
                _ => {
                    let v = lenv
                        .borrow()
                        .get(s)
                        .or_else(|| {
                            // rename, theoretically it should not appear in variadic pattern
                            renv.borrow().get(s).map(|_| {
                                let val = match renamed.get(s) {
                                    Some(ss) => Value::Symbol(ss.clone()),
                                    _ => {
                                        let ss = gensym(s);
                                        renamed.insert(s.clone(), ss.clone());
                                        Value::Symbol(ss)
                                    }
                                };
                                val
                            })
                        })
                        .unwrap_or(expr.clone());
                    let vec = iter::repeat(v).take(n).collect::<Vec<_>>();
                    Ok(vec)
                }
            }
        }
        Value::DatumList(ref l) => {
            let mut vv = Vec::new();
            for var in l.iter() {
                let val = transform_repeated(var, replace, n, lenv, renv, renamed)?;
                let it = val.into_iter();
                vv.push(it);
            }
            let mut vec: Vec<Value> = Vec::new();
            for _ in 1..=n {
                let mut l = List::Nil;
                for i in vv.iter_mut() {
                    l = l.unshift_r(i.next().unwrap());
                }
                vec.push(Value::DatumList(l));
            }
            Ok(vec)
        }
        Value::VarValue(_) | Value::VarList(_) => runtime_error!(
            "failed to expand macro: nested ellipsis(...) not supported in expansion {:?}",
            expr
        ),
        _ => {
            let vec = iter::repeat(expr.clone()).take(n).collect::<Vec<Value>>();
            Ok(vec)
        }
    }
}

// TODO: for now, #: is not allowed as start of identifier, so ..
fn gensym(s: &String) -> String {
    format!("#:{}", s)
}

fn transform_template(
    template: List<Value>,
    matcher: &Matcher,
    lenv: &Rc<RefCell<Env>>,
    renv: &Rc<RefCell<Env>>,
    renamed: &mut HashMap<String, String>,
) -> Result<List<Value>, RuntimeError> {
    let mut ret: List<Value> = List::Nil;
    //println!("template {:?} >>", template);
    for var in template {
        match var {
            // symbols:
            // 1. match in pattern table: substitute
            // 2. bind in lenv, get it from lenv and replace
            // 3. bind in renv, rename it(gensym)
            // 4. free, keep it as it is
            Value::Symbol(s) => {
                if matcher.contains_key(&s) {
                    let v = matcher.get(&s).unwrap();
                    match v {
                        RepValue::Val(ref val) => {
                            ret = ret.unshift_r(val.clone());
                        }
                        _ => (),
                    }
                } else if lenv.borrow().is_defined(&s) {
                    let v = lenv.borrow().get(&s).unwrap();
                    ret = ret.unshift_r(v);
                } else if renv.borrow().is_defined(&s) {
                    let v = match renamed.get(&s) {
                        Some(v) => Value::Symbol(v.clone()),
                        _ => {
                            let ss = gensym(&s);
                            renamed.insert(s.clone(), ss.clone());
                            Value::Symbol(ss)
                        }
                    };
                    ret = ret.unshift_r(v);
                } else {
                    ret = ret.unshift_r(Value::Symbol(s));
                }
            }
            // a ...
            Value::VarValue(s) => {
                if !s.is_symbol() {
                    runtime_error!(
                        "failed to expand macro: cannot expand variadic literals {:?}",
                        s
                    );
                }
                let sym = s.as_symbol()?;
                if !matcher.contains_key(&sym) {
                    runtime_error!("failed to expand macro: unknown pattern {:?}", sym);
                }
                let v = matcher.get(&sym).unwrap();
                match v {
                    RepValue::Repeated(ref l) => {
                        ret = l.iter().fold(ret, |acc, i| acc.unshift_r(i.clone()));
                    }
                    _ => runtime_error!(
                        "failed to expand macro: variadic value must be repeated {:?}",
                        sym
                    ), // impossible
                }
            }
            // (a b) ...
            Value::VarList(l) => {
                let ks = matcher.keys().cloned().collect::<Vec<_>>();
                let pattern_vars = Macro::pattern_variables(&l, &Vec::new(), &ks);
                let sz = pattern_vars
                    .iter()
                    .map(|s| match matcher.get(s) {
                        Some(RepValue::Repeated(ref v)) => v.len(),
                        _ => 0,
                    })
                    .collect::<HashSet<usize>>();
                if sz.is_empty() {
                    runtime_error!(
                        "failed to expand macro: non repeated values for variadic pattern {:?}",
                        l
                    );
                } else if sz.len() > 1 {
                    runtime_error!(
                        "failed to expand macro: inconsistent repeat # of pattern variables {:?}",
                        l
                    );
                }
                let n = sz.into_iter().next().unwrap();
                // substitute
                let mut replace: HashMap<String, Vec<Value>> = HashMap::new();
                for k in pattern_vars.iter() {
                    match matcher.get(k) {
                        Some(RepValue::Repeated(ref var)) => {
                            replace.insert(k.clone(), var.clone());
                        }
                        _ => (),
                    }
                }
                let mut vv = Vec::new();
                // each expr repeat n times
                for i in l.iter() {
                    let x = transform_repeated(i, &replace, n, lenv, renv, renamed)?;
                    vv.push(x.into_iter());
                }
                let mut vec: Vec<Value> = Vec::new();
                // zip into list
                for _ in 1..=n {
                    let mut subl = List::Nil;
                    for i in vv.iter_mut() {
                        subl = subl.unshift_r(i.next().unwrap());
                    }
                    let vl = Value::DatumList(subl);
                    //println!("Varlist {:?}: add {:?}", l, vl);
                    ret = ret.unshift_r(vl);
                }
            }
            Value::DatumList(l) => {
                let v = transform_template(l, matcher, lenv, renv, renamed)?;
                ret = ret.unshift_r(Value::DatumList(v));
            }
            _ => {
                ret = ret.unshift_r(var);
            }
        }
    }
    //println!("transformed: {:?}", ret);
    Ok(ret)
}

fn macro_expand(
    makro: &Macro,
    operands: &List<Value>,
    env: &Rc<RefCell<Env>>,
) -> Result<List<Value>, RuntimeError> {
    // 1. match
    let (SyntaxRule(_, template), matcher) = makro.rule_matches(operands, env)?;
    // 2. expand
    let mut renamed: HashMap<String, String> = HashMap::new();
    transform_template(template, &matcher, &makro.def_env, &env, &mut renamed)
}

fn apply(f: Value, args: List<Value>, next: Box<Continuation>) -> Result<Trampoline, RuntimeError> {
    let ff = format!("{:?}", f);
    match f {
        Value::Procedure(params, body, env) => {
            //println!("apply proc: {:?} with {:?}", params, args);
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
                        (Some(_), None) => {
                            runtime_error!("not enough arguments to procedure: {}", ff)
                        }
                        _ => break,
                    }
                }
                let rest_args = j.collect::<List<Value>>();
                new_env
                    .borrow_mut()
                    .define(rest.clone(), Value::from_list(rest_args));
            } else {
                if params.len() != args.len() {
                    runtime_error!(
                        "must supply exactly {} arguments to procedure: {}",
                        params.len(),
                        ff
                    );
                } else {
                    for (x, y) in params.into_iter().zip(args.into_iter()) {
                        new_env.borrow_mut().define(x, y);
                    }
                }
            }
            eval_expressions(body, new_env, next)
        }
        Value::Primitive(n) => {
            let ret = call_primitive(n.as_str(), args)?;
            Ok(Trampoline::Value(ret, *next))
        }
        Value::Continuation(k) => {
            let arg = match args.shift() {
                Some((car, _)) => car,
                _ => Value::Unspecified,
            };
            Ok(Trampoline::Value(arg, *k))
        }
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
        "zero?" => {
            call_primitive_predicate(args, f, |v| v.is_integer_with(0) || v.is_float_with(0.0))
        }
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
fn call_primitive_arithmetic<F, G>(
    args: List<Value>,
    name: &str,
    fi: F,
    ff: G,
) -> Result<Value, RuntimeError>
where
    F: Fn(List<Value>) -> Result<Value, RuntimeError>,
    G: Fn(List<Value>) -> Result<Value, RuntimeError>,
{
    if args.iter().all(|v| v.is_integer()) {
        fi(args)
    } else if args.iter().all(|v| v.is_float()) {
        ff(args)
    } else {
        runtime_error!("wrong type of args to `{}': {:?}", name, args)
    }
}

fn call_primitive_predicate<F>(
    args: List<Value>,
    name: &str,
    pred: F,
) -> Result<Value, RuntimeError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        runtime_error!(
            "wrong number of args to `{}': 1 expected, {} got",
            name,
            args.len()
        )
    }
    let x = args.unpack1()?;
    let ret = pred(&x);
    Ok(Value::Boolean(ret))
}

fn primitive_cons(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `cons': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let ret = Cons(rr!(x), rr!(y));
    Ok(Value::Cons(ret))
}

fn primitive_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!(
            "wrong number of args to `car': 1 expected, {} got",
            args.len()
        )
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.0.borrow().clone()),
        _ => runtime_error!("wrong type of arg to `car': {:?}", x),
    }
}

fn primitive_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!(
            "wrong number of args to `cdr': 1 expected, {} got",
            args.len()
        )
    }

    let x = args.unpack1()?;
    match x {
        Value::Cons(r) => Ok(r.1.borrow().clone()),
        _ => runtime_error!("wrong type of arg to `cdr': {:?}", x),
    }
}

fn primitive_set_car(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `set-car!': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let mut p = x.as_pair()?;
        p.set_car(y);
        Ok(Value::Unspecified)
    } else {
        runtime_error!("wrong type of 1st arg to `set-car!': {:?}", x)
    }
}

fn primitive_set_cdr(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `set-cdr!': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    if x.is_pair() {
        let mut p = x.as_pair()?;
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
where
    F: Fn(T, T) -> T,
    H: Fn(Value) -> Result<T, RuntimeError>,
{
    args.into_iter().fold(Ok(init), |acc, x| match acc {
        Ok(s) => Ok(op(s, tr(x)?)),
        _ => acc,
    })
}
fn primitive_plus_i(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 0, |acc, x| acc + x, |x| x.as_integer())?;
    Ok(Value::Integer(r))
}
fn primitive_plus_f(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 0.0, |acc, x| acc + x, |x| x.as_float())?;
    Ok(Value::Float(r))
}
fn primitive_minus_i(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `-': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let r = x.as_integer()? - y.as_integer()?;
    Ok(Value::Integer(r))
}
fn primitive_minus_f(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `-': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let r = x.as_float()? - y.as_float()?;
    Ok(Value::Float(r))
}
fn primitive_mul_i(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 1, |acc, x| acc * x, |x| x.as_integer())?;
    Ok(Value::Integer(r))
}
fn primitive_mul_f(args: List<Value>) -> Result<Value, RuntimeError> {
    let r = fold_values(args, 1.0, |acc, x| acc * x, |x| x.as_float())?;
    Ok(Value::Float(r))
}
fn primitive_div_i(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `/': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let r = x.as_integer()? / y.as_integer()?;
    Ok(Value::Integer(r))
}
fn primitive_div_f(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `/': 2 expected, {} got",
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let r = x.as_float()? / y.as_float()?;
    Ok(Value::Float(r))
}
fn compare_numeric<T, F, H>(args: List<Value>, f: &str, op: F, tr: H) -> Result<Value, RuntimeError>
where
    F: Fn(T, T) -> bool,
    H: Fn(Value) -> Result<T, RuntimeError>,
{
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `{}': 2 expected, {} got",
            f,
            args.len()
        )
    }
    let (x, y) = args.unpack2()?;
    let ret = op(tr(x)?, tr(y)?);
    Ok(Value::Boolean(ret))
}
fn primitive_eq_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "=", |x, y| x == y, |x| x.as_integer())
}
fn primitive_eq_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "=", |x, y| x == y, |x| x.as_float())
}
fn primitive_gt_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">", |x, y| x > y, |x| x.as_integer())
}
fn primitive_gt_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">", |x, y| x > y, |x| x.as_float())
}
fn primitive_le_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<=", |x, y| x <= y, |x| x.as_integer())
}
fn primitive_le_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<=", |x, y| x <= y, |x| x.as_float())
}
fn primitive_lt_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<", |x, y| x < y, |x| x.as_integer())
}
fn primitive_lt_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, "<", |x, y| x < y, |x| x.as_float())
}
fn primitive_ge_i(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">=", |x, y| x >= y, |x| x.as_float())
}
fn primitive_ge_f(args: List<Value>) -> Result<Value, RuntimeError> {
    compare_numeric(args, ">=", |x, y| x >= y, |x| x.as_float())
}

fn primitive_display(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!(
            "wrong number of args to `display': 1 expected, {} got",
            args.len()
        )
    }
    let x = args.unpack1()?;
    print!("{}", x);
    Ok(Value::Unspecified)
}
fn primitive_newline(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 0 {
        runtime_error!(
            "wrong number of args to `newline': 0 expected, {} got",
            args.len()
        )
    }
    println!();
    Ok(Value::Unspecified)
}
fn primitive_displayln(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!(
            "wrong number of args to `displayln': 1 expected, {} got",
            args.len()
        )
    }
    let x = args.unpack1()?;
    println!("{}", x);
    Ok(Value::Unspecified)
}

fn primitive_error(args: List<Value>) -> Result<Value, RuntimeError> {
    let msg = args
        .iter()
        .map(|v| format!("{:?}", v))
        .collect::<Vec<_>>()
        .join(" ");
    runtime_error!("{}", msg)
}

fn primitive_eqv(args: List<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!(
            "wrong number of args to `eqv?': 2 expected, {} got",
            args.len()
        )
    }
    if all_of!(args, is_boolean) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_bool()? == y.as_bool()?))
    } else if all_of!(args, is_symbol) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_symbol()? == y.as_symbol()?))
    } else if all_of!(args, is_integer) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_integer()? == y.as_integer()?))
    } else if all_of!(args, is_char) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_char()? == y.as_char()?))
    } else if all_of!(args, is_nil) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(true))
    } else if all_of!(args, is_string) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_string()? == y.as_string()?))
    } else if all_of!(args, is_pair) {
        let (x, y) = args.unpack2()?;
        Ok(Value::Boolean(x.as_pair()? == y.as_pair()?))
    // TODO: procedures eqv? by define location
    } else {
        Ok(Value::Boolean(false))
    }
}

// TODO: or make it procedures?
pub enum Trampoline {
    // go forward
    Bounce(Value, Rc<RefCell<Env>>, Continuation),
    // run cont with value
    Value(Value, Continuation),
    // quasiquote mode
    QuasiBounce(Value, Rc<RefCell<Env>>, Continuation),
    //    // macro mode
    //    MacroBounce(
    //        Value,
    //        Rc<RefCell<Env>>,
    //        Continuation,
    //    ),
    Off(Value), // only used by Continuation::Return
}

// eval list of s-exp: expr must be one *complete* s-exp, not an atom
fn eval_expressions(
    expr: List<Value>,
    env: Rc<RefCell<Env>>,
    k: Box<Continuation>,
) -> Result<Trampoline, RuntimeError> {
    match expr.shift() {
        Some((car, cdr)) => Ok(Trampoline::Bounce(
            car,
            env.clone(),
            Continuation::EvaluateExpressions(cdr, env, k),
        )),
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
                        Value::DatumList(l) => match l.shift() {
                            Some((car, cdr)) => Trampoline::Bounce(
                                car,
                                next_e.clone(),
                                Continuation::ApplyLike(cdr, next_e, Box::new(next_k)),
                            ),
                            _ => runtime_error!("cannot apply empty list"),
                        },
                        // symbol, lookup
                        Value::Symbol(ref s) => {
                            let v = match next_e.borrow().get(s) {
                                Some(v) => v,
                                _ => runtime_error!("unbound variable: {}", s),
                            };
                            next_k.run(v)?
                        }
                        _ => next_k.run(next_v)?, // NOTE: this arm is same with Trampoline::Value, but ::Value can run with symbol
                    }
                }
                Trampoline::Value(next_v, next_k) => {
                    b = next_k.run(next_v)?;
                }
                Trampoline::QuasiBounce(next_v, next_e, next_k) => {
                    b = match next_v {
                        // 1. list: normal list or apply like: unquote, unquote-splicing
                        Value::DatumList(l) => {
                            match l.shift() {
                                Some((car, cdr)) => {
                                    match car {
                                        f @ Value::SpecialForm(SpecialForm::Unquote)
                                        | f @ Value::SpecialForm(SpecialForm::UnquoteSplicing) => {
                                            // unquote to val; unquote-splicing to list and then unpack
                                            // bounce to apply-like with form unquote
                                            Trampoline::Bounce(
                                                f,
                                                next_e.clone(),
                                                Continuation::ApplyLike(
                                                    cdr,
                                                    next_e,
                                                    Box::new(next_k),
                                                ),
                                            )
                                        }
                                        _ => Trampoline::QuasiBounce(
                                            car,
                                            next_e.clone(),
                                            Continuation::ContinueQuasiExpand(
                                                false,
                                                cdr,
                                                List::Nil,
                                                next_e,
                                                Box::new(next_k),
                                            ),
                                        ),
                                    }
                                }
                                _ => next_k.run(Value::Nil)?,
                            }
                        }
                        // 2. symbol: reflect it directly
                        _ => next_k.run(next_v)?,
                    };
                }
                Trampoline::Off(next_v) => {
                    return Ok(next_v);
                }
            }
        }
    }
}

fn exec(prog: &str) -> Result<Value, RuntimeError> {
    let tokens = Lexer::tokenize(prog).unwrap();
    let tree = Parser::parse(&tokens).unwrap();
    process(&tree, Env::root())
}

fn exec_ok(prog: &str) -> Value {
    exec(prog).unwrap()
}

pub fn prologue(prog: &str, env: Rc<RefCell<Env>>) -> Result<Value, RuntimeError> {
    let tokens = Lexer::tokenize(prog).unwrap();
    let tree = Parser::parse(&tokens).unwrap();
    process(&tree, env)
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
        let args = v
            .into_iter()
            .map(|x| Value::Integer(x))
            .collect::<List<_>>();
        let ret = primitive_plus_i(args).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 6);
    }

    #[test]
    fn test_value_mul() {
        let v: Vec<i64> = vec![4, 2, 3];
        let args = v
            .into_iter()
            .map(|x| Value::Integer(x))
            .collect::<List<_>>();
        let ret = call_primitive("*", args).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 24);
    }

    #[test]
    fn test_value_list() {
        let n1 = Value::Integer(1);
        let n2 = Value::Integer(2);
        let n3 = Value::Integer(3);
        let args = vec![n1.clone(), n2.clone(), n3.clone()]
            .into_iter()
            .collect::<List<_>>();
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

    #[test]
    fn test_if_1() {
        let prog = "(if (> 1 2) 3 4)";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 4);
    }

    #[test]
    fn test_if_2() {
        let prog = "(if ((if (> 5 4) > <) (+ 1 2) 2) (+ 5 7 8) (+ 9 10 11))";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 20);
    }

    #[test]
    fn test_if_3() {
        let prog = "(if 0 3 4)";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 3);
    }

    #[test]
    fn test_multiple_statements() {
        let prog = "(+ 1 2) (+ 3 4) ; => 7";
        let ret = exec(prog).unwrap();
        assert_eq!(ret.as_integer().unwrap(), 7);
    }

    #[test]
    fn test_define() {
        let prog = "(define x 2) (+ x x) ; => 4";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 4);
    }

    #[test]
    fn test_set() {
        let prog = "(define x 2) (set! x 3) (+ x x) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 6);
    }

    #[test]
    fn test_lambda() {
        let prog = "((lambda (x) (+ x 2)) 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 5);
    }

    #[test]
    fn test_lambda_symbol() {
        let prog = "((λ (x) (+ x 2)) 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 5);
    }

    #[test]
    fn test_define_func() {
        let prog = "(define (f x) (+ x 2)) (f 3) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 5);
    }

    #[test]
    fn test_define_func2() {
        let prog =
            "(define (noop) (+ 0 0)) (define (f x) (noop) (+ x 2)) ((lambda () (f 3))) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 5);
    }

    #[test]
    fn test_define_func3() {
        let prog = "(define (plus-or-mul op . args) (apply op args)) (plus-or-mul * 1 2 3) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 6);
    }

    #[test]
    fn test_quoting() {
        let prog = "(quote (1 2)) ; => (1 2)";
        let ret = exec_ok(prog);
        println!("{}", ret);
        let p = ret.as_pair().unwrap();
        assert!(p.is_list());
    }

    #[test]
    fn test_apply() {
        let prog = "(apply + (quote (1 2 3))) ; => 6";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 6);
    }

    #[test]
    fn test_begin() {
        let prog = "(define x 1) (begin (set! x 5) (set! x (+ x 2)) x) ; => 7";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 7);
    }

    #[test]
    fn test_eval_1() {
        let prog = "(eval (quote (+ 1 2))) ; => 3";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 3);
    }

    #[test]
    fn test_eval_2() {
        let prog = "(define (foo x) (eval (quote (+ 1 2))) x) (foo 5) ; => 5";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 5);
    }

    #[test]
    fn test_quasiquoting_1() {
        let prog = "(apply + (quasiquote (2 (unquote (+ 1 2)) 4))) ; => 9";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 9);
    }

    #[test]
    fn test_quasiquoting_2() {
        let prog = "(apply + `(2 ,(+ 1 2) ,@(list 1 2 3) 4)) ; => 15";
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 15);
    }

    #[test]
    fn test_macro_1() {
        let prog = "(define-syntax lst (syntax-rules () ((_ xs ...) (list xs ...)))) (lst) ; => ()";
        let ret = exec_ok(prog);
        assert!(ret.is_list());
        assert_eq!(ret.list_len(), 0 as usize);
    }

    #[test]
    fn test_macro_2() {
        let prog = "(define-syntax lst (syntax-rules () ((_ xs ...) (list xs ...)))) (lst 1 2 3) ; => (1 2 3)";
        let ret = exec_ok(prog);
        assert!(ret.is_list());
        assert_eq!(ret.list_len(), 3 as usize);
    }

    #[test]
    fn test_macro_3() {
        let prog = r#"(define-syntax define-matcher-macro
  (syntax-rules ()
    ((_ name lit)
     (define-syntax name
       (syntax-rules ()
        ((_ lit) #t)
        ((_ else) #f))))))
  (define-matcher-macro is-lit-foo? "foo")
  (is-lit-foo? "foo")"#;
        let ret = exec_ok(prog);
        assert!(ret.as_bool().unwrap());
    }

    #[test]
    fn test_macro_4() {
        let prog = r#"(define-syntax define-matcher-macro
  (syntax-rules ()
    ((_ name lit)
     (define-syntax name
       (syntax-rules ()
        ((_ lit) #t)
        ((_ else) #f))))))
  (define-matcher-macro is-lit-foo? "foo")
  (is-lit-foo? "bar")"#;
        let ret = exec_ok(prog);
        assert!(!ret.as_bool().unwrap());
    }

    #[test]
    fn test_macro_5() {
        let prog = r#"(define-syntax define-matcher-macro
  (syntax-rules ()
    ((_ name lit)
     (define-syntax name
       (syntax-rules ()
        ((_ lit) #t)
        ((_ else) #f))))))
  (define-matcher-macro is-lit-foo? "foo")
  (define foo "foo")
  (is-lit-foo? foo)"#;
        let ret = exec_ok(prog);
        assert!(!ret.as_bool().unwrap());
    }

    #[test]
    fn test_callcc_1() {
        let prog = r#"
       (define x 0)
       (define (+x n) (set! x (+ x n)))
       (define (foo k) (+x 2) (k) (+x 4))
       ((lambda ()
          (+x 1)
          (call/cc foo)
          (+x 8)))
       x ; => 11"#;
        let ret = exec_ok(prog);
        assert_eq!(ret.as_integer().unwrap(), 11);
    }
}
