use std::rc::Rc;
use std::cell::RefCell;

use rustyline::{
    line_buffer::LineBuffer,
    Context,
    Result,
    completion::Completer,
};
use memchr::memchr;
use strum::IntoEnumIterator; // strum iter

use crate::interpreter::{SpecialForm, Env};

pub struct ReplCompleter {
    break_chars: &'static [u8],
    env: Rc<RefCell<Env>>, // root env
}

const DEFAULT_BREAK_CHARS: [u8; 17] = [
    b' ', b'\t', b'\n', b'"', b'\\', b'\'', b'`', b'@', b',', b';',
    b'{', b'}', b'[', b']', b'(', b')', b'\0',
];

impl ReplCompleter {
    pub fn new(env: Rc<RefCell<Env>>) -> ReplCompleter {
        ReplCompleter { break_chars: &DEFAULT_BREAK_CHARS, env }
    }
}

#[derive(PartialEq)]
enum ScanMode {
    DoubleQuote,
    Escape,
    EscapeInDoubleQuote,
    Normal,
}

fn unclosing_quote(s: &str) -> bool {
    let char_indices = s.char_indices();
    let mut mode = ScanMode::Normal;
    let mut quote_index = 0;
    for (index, char) in char_indices {
        match mode {
            ScanMode::Normal => {
                if char == '"' {
                    mode = ScanMode::DoubleQuote;
                    quote_index = index;
                } else if char == '\\' {
                    mode = ScanMode::Escape;
                }
            }
            ScanMode::DoubleQuote => {
                if char == '"' {
                    mode = ScanMode::Normal;
                } else if char == '\\' {
                    mode = ScanMode::EscapeInDoubleQuote;
                }
            }
            ScanMode::Escape => {
                mode = ScanMode::Normal;
            }
            ScanMode::EscapeInDoubleQuote => {
                mode = ScanMode::DoubleQuote;
            }
        };
    }
    
    match mode {
        ScanMode::DoubleQuote | ScanMode::EscapeInDoubleQuote => true,
        _ => false,
    }
}

fn extract_symbol<'l>(
    line: &'l str,
    pos: usize,
    break_chars: &[u8],
) -> (usize, &'l str) {
    let line = &line[..pos];
    if line.is_empty() {
        return (0, line);
    }
    
    let mut start = None;
    for (i, c) in line.char_indices().rev() {
        if memchr(c as u8, break_chars).is_some() {
            start = Some(i + c.len_utf8());
            break;
        }
    }
    
    match start {
        Some(start) => (start, &line[start..]),
        None => (0, line),
    }
}

impl Completer for ReplCompleter {
    type Candidate = String;
    
    fn complete(&self, line: &str, pos: usize, _: &Context<'_>) -> Result<(usize, Vec<Self::Candidate>)> {
        if unclosing_quote(line) {
            Ok((0, Vec::with_capacity(0)))
        } else {
            let (start, symbol) = extract_symbol(line, pos, &self.break_chars);
            let mut specials: Vec<String> = SpecialForm::iter().map(|v| v.to_string()).collect();
            specials.push("call-with-current-continuation".to_string());
            let env_symbols = self.env.borrow()
                .values()
                .keys()
                .chain(specials.iter())
                .filter(|&v| v.starts_with(symbol))
                .map(|v| String::from(v))
                .collect::<Vec<_>>();
            Ok((start, env_symbols))
        }
    }
}