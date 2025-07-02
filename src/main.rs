use std::process;

use anyhow::Result;

use crate::lox::Lox;

mod lox;
mod scanner;

fn main() -> Result<()> {
    let args = std::env::args();
    let mut lox = Lox::new();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        lox.run_file(&args.skip(1).next().unwrap())
    } else {
        lox.run_prompt()
    }
}
