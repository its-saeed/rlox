use std::process;

use anyhow::Result;
use rustyline::DefaultEditor;

fn main() -> Result<()> {
    let args = std::env::args();
    if args.len() > 2 {
        println!("Usage: rlox [script]");
        process::exit(64);
    } else if args.len() == 2 {
        run_file(&args.skip(1).next().unwrap())
    } else {
        run_prompt()
    }
}

fn run_file(filename: &str) -> Result<()> {
    let content = std::fs::read_to_string(filename)?;
    run(content)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let line = rl.readline(">> ")?;
        run(line)?;
    }
}

fn run(source: String) -> Result<()> {
    println!("Running: {source}");
    Ok(())
}
