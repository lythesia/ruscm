extern crate ruscm;

use ruscm::{interpreter, repl::Repl};
use std::fs;
use std::io::Read;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "ruscm")]
struct Opt {
    /// Evaluate EXPR as a Scheme expression
    #[structopt(short = "c")]
    expr: Option<String>,
    /// Load source from SCRIPT
    #[structopt(parse(from_os_str))]
    script: Option<PathBuf>,
    /// ARGs for SCRIPT
    args: Vec<String>,
}

fn main() {
    let opt: Opt = Opt::from_args();
    match opt.expr {
        Some(expr) => {
            interpreter::exec(expr.as_str()).map_err(|e| eprintln!("{}", e));
        }
        _ => match opt.script {
            Some(script) => match fs::OpenOptions::new().read(true).open(&script) {
                Ok(mut file) => {
                    let mut src = String::new();
                    file.read_to_string(&mut src);
                    interpreter::exec_args(src.as_str(), opt.args).map_err(|e| eprintln!("{}", e));
                }
                Err(e) => eprintln!("open `{}' failed: {}", script.display(), e),
            },
            _ => {
                let mut repl = Repl::new();
                repl.run();
            }
        },
    }
}
