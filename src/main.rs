extern crate ruscm;

use ruscm::repl::Repl;

// TODO: 1. opt-parse; 2. scripting
fn main() {
    let mut repl = Repl::new();
    repl.run();
}