extern crate ruscm;

use ruscm::repl::Repl;

fn main() {
    let mut repl = Repl::new();
    repl.run();
}