use crate::scanner::Scanner;
use anyhow::Result;
use rustyline::DefaultEditor;

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn run_file(&mut self, filename: &str) -> Result<()> {
        let content = std::fs::read_to_string(filename)?;
        self.run(content)
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        let mut rl = DefaultEditor::new()?;
        loop {
            let line = rl.readline(">> ")?;
            self.run(line)?;
        }
    }

    fn run(&mut self, source: String) -> Result<()> {
        println!("Running: {source}");
        let mut scanner = Scanner::new(&source);
        scanner.scan_tokens();
        Ok(())
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message)
    }

    fn report(&mut self, line: usize, loc: &str, message: &str) {
        println!("[line {line}] Error {loc}: {message}");
        self.had_error = true;
    }
}
