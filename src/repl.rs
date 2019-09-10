use std::error::Error as StdError;
use std::rc::Rc;
use std::cell::RefCell;
use std::mem;
use std::borrow::Cow::{self, Borrowed, Owned};

use rustyline::{
    Editor,
    error::ReadlineError,
    Context,
    Helper,
    completion::Completer,
    highlight::{Highlighter, MatchingBracketHighlighter},
    hint::{Hinter, HistoryHinter},
    KeyPress,
    Cmd,
};

use crate::internals::List;
use crate::lexer::{Token, Position, Lexer};
use crate::parser::{Parser, AstNode};
use crate::interpreter::{Env, process};
use crate::completer::ReplCompleter;

enum State {
    Stop,
    Read(Vec<(Token, Position)>),
    Parse(Vec<(Token, Position)>),
    Eval(List<AstNode>),
    Err(Box<dyn StdError>),
}

struct ReplHelper {
    completer: ReplCompleter,
    highlighter: MatchingBracketHighlighter,
    hinter: HistoryHinter,
}

impl Helper for ReplHelper {}

impl Completer for ReplHelper {
    type Candidate = String;
    
    fn complete(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Result<(usize, Vec<Self::Candidate>), ReadlineError> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for ReplHelper {
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }
    
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        // gray
        Owned("\x1b[38;2;96;98;96m".to_owned() + hint + "\x1b[0m")
    }
    
    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

pub struct Repl {
    env: Rc<RefCell<Env>>,
    state: State,
}

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
        let mut rl = Editor::<ReplHelper>::new();
        let hist_path = dirs::home_dir().unwrap().join(".ruscm-repl.history");
        if rl.load_history(&hist_path).is_err() {
            println!("No previous history.");
        }
        let helper = ReplHelper {
            completer: ReplCompleter::new(self.env.clone()),
            highlighter: MatchingBracketHighlighter::new(),
            hinter: HistoryHinter {},
        };
        rl.set_helper(Some(helper));
        
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