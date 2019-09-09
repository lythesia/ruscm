use std::error::Error as StdError;
use std::rc::Rc;
use std::cell::RefCell;
use std::mem;

use rustyline::{
    Editor,
    error::ReadlineError,
};

use crate::internals::List;
use crate::lexer::{Token, Position, Lexer};
use crate::parser::{Parser, AstNode};
use crate::interpreter::{Env, process};

enum State {
    Stop,
    Read(Vec<(Token, Position)>),
    Parse(Vec<(Token, Position)>),
    Eval(List<AstNode>),
    Err(Box<dyn StdError>),
}

pub struct Repl {
    env: Rc<RefCell<Env>>,
    state: State,
}

// TODO: completer: 1. keywords; 2. symbol of root-env
impl Repl {
    fn tokens_count_with<F>(tokens: &Vec<(Token, Position)>, f: F) -> usize
    where F: Fn(&Token) -> bool {
        tokens.iter().fold(0, |acc, (i, _)| if f(i) {acc + 1} else {acc})
    }
    
    fn is_complete_sexp(tokens: &Vec<(Token, Position)>) -> bool {
        let ls = Self::tokens_count_with(tokens, |t| t == &Token::LPAREN);
        let rs = Self::tokens_count_with(tokens, |t| t == &Token::RPAREN);
        ls <= rs
    }
    
    pub fn new() -> Repl {
        Repl { env: Env::root(), state: State::Read(Vec::new()) }
    }
    
    pub fn run(&mut self) {
        let mut rl = Editor::<()>::new();
        let hist_path = dirs::home_dir().unwrap().join(".ruscm-repl.history");
        if rl.load_history(&hist_path).is_err() {
            println!("No previous history.");
        }
        let mut lineno = 0usize;
        loop {
            let curr = mem::replace(&mut self.state, State::Stop);
            let next = match curr {
                State::Read(mut tokens) => {
                    let prompt = if tokens.is_empty() {">> "} else {".. "};
                    match rl.readline(prompt) {
                        Ok(line) => {
                            lineno += 1;
                            let s = line.as_str();
                            rl.add_history_entry(s);
                            match Lexer::tokenize_line(s, lineno) {
                                Ok(input) => {
                                    tokens.extend(input.into_iter());
                                    if Self::is_complete_sexp(&tokens) {
                                        State::Parse(tokens)
                                    } else {
                                        State::Read(tokens)
                                    }
                                },
                                Err(err) => State::Err(Box::new(err)),
                            }
                        },
                        Err(ReadlineError::Interrupted) => {
                            println!("Interrupt while reading");
                            State::Read(Vec::new())
                        },
                        Err(ReadlineError::Eof) => {
                            println!("Bye");
                            State::Stop
                        },
                        Err(err) => {
                            println!("REPL ERR: {:?}", err);
                            println!("exiting ..");
                            State::Stop
                        },
                    }
                },
                State::Parse(tokens) => {
                    match Parser::parse(&tokens) {
                        Ok(tree) => State::Eval(tree),
                        Err(err) => State::Err(Box::new(err)),
                    }
                },
                State::Eval(tree) => {
                    match process(&tree, self.env.clone()) {
                        Ok(val) => {
                            if !val.is_unspecified() {
                                println!("{}", val);
                            }
                        },
                        Err(err) => println!("{}", err),
                    };
                    State::Read(Vec::new())
                },
                State::Err(e) => {
                    println!("{}", e);
                    State::Read(Vec::new())
                },
                _ => State::Stop,
            };
            match next {
                State::Stop => break,
                _ => {
                    mem::replace(&mut self.state, next);
                },
            }
        }
        rl.save_history(&hist_path).unwrap();
    }
}