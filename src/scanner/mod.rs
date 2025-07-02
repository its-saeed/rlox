#[derive(Debug, PartialEq, PartialOrd)]
enum TokenType {
    // Single Character Tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Literal<'a> {
    String(&'a str),
    Number(f64),
}

#[derive(Debug, PartialEq, PartialOrd)]
struct Token<'a> {
    _type: TokenType,
    lexeme: &'a str,
    line: usize,
    literal: Option<Literal<'a>>,
}

impl<'a> Token<'a> {
    fn new(_type: TokenType, lexeme: &'a str, line: usize) -> Self {
        Self {
            _type,
            lexeme,
            line,
            literal: None,
        }
    }

    fn new_with_literal(
        _type: TokenType,
        lexeme: &'a str,
        line: usize,
        literal: Option<Literal<'a>>,
    ) -> Self {
        Self {
            _type,
            lexeme,
            line,
            literal,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    chars: std::str::CharIndices<'a>,
    tokens: Vec<Token<'a>>,

    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::Eof, "", self.line))
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            Some('(') => self.add_token(TokenType::LeftParen),
            Some(')') => self.add_token(TokenType::RightParen),
            Some('{') => self.add_token(TokenType::LeftBrace),
            Some('}') => self.add_token(TokenType::RightBrace),
            Some(',') => self.add_token(TokenType::Comma),
            Some('.') => self.add_token(TokenType::Dot),
            Some('-') => self.add_token(TokenType::Minus),
            Some('+') => self.add_token(TokenType::Plus),
            Some(';') => self.add_token(TokenType::Semicolon),
            Some('*') => self.add_token(TokenType::Star),
            Some('!') => {
                let next_token = if self.is_match('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(next_token);
            }
            Some('=') => {
                let next_token = if self.is_match('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(next_token);
            }
            Some('<') => {
                let next_token = if self.is_match('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(next_token);
            }
            Some('>') => {
                let next_token = if self.is_match('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(next_token);
            }
            Some('/') => {
                if self.is_match('/') {
                    while self.peek().is_some_and(|ch| ch != '\n') {
                        self.advance();
                    }
                } else if self.is_match('*') {
                    loop {
                        match self.advance() {
                            Some(c) => {
                                if c == '\n' {
                                    self.line += 1;
                                }
                                if c == '*' && self.is_match('/') {
                                    break;
                                }
                            }
                            None => {
                                eprint!("Unmatched comment.");
                                break;
                            }
                        }
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            Some(' ') | Some('\r') | Some('\t') => (),
            Some('\n') => self.line += 1,
            Some('\"') => self.string(),
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            Some(unexpected) => eprintln!("Unexpected token: {unexpected}!"),
            None => (),
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some((index, ch)) => {
                self.current = index + 1;
                Some(ch)
            }
            None => None,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn add_token(&mut self, _type: TokenType) {
        self.tokens.push(Token::new(
            _type,
            &self.source[self.start..self.current],
            self.line,
        ));
    }

    fn add_token_with_literal(&mut self, _type: TokenType, literal: Literal<'a>) {
        self.tokens.push(Token::new_with_literal(
            _type,
            &self.source[self.start..self.current],
            self.line,
            Some(literal),
        ));
    }

    fn is_match(&mut self, expected: char) -> bool {
        matches!(self.chars.clone().next(), Some((_, ch)) if ch == expected) && {
            self.consumed();
            true
        }
    }

    fn consumed(&mut self) {
        self.current += 1;
        self.chars.next();
    }

    fn string(&mut self) {
        while self.peek().is_some_and(|ch| ch != '\"') {
            if let Some(ch) = self.peek() {
                if ch == '\n' {
                    self.line += 1;
                }
            }

            self.advance();
        }

        if self.is_at_end() {
            eprintln!("Unterminated string");
            return;
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token_with_literal(TokenType::String, Literal::String(value));
    }

    fn number(&mut self) {
        while self.peek().is_some_and(|ch| ch.is_ascii_digit()) {
            self.advance();
        }
        if self.peek().is_some_and(|v| v == '.')
            && self.peek_next().is_some_and(|v| v.is_ascii_digit())
        {
            self.advance();
            while self.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                self.advance();
            }
        }

        self.add_token_with_literal(
            TokenType::Number,
            Literal::Number(self.source[self.start..self.current].parse().unwrap()),
        );
    }

    fn identifier(&mut self) {
        while self
            .peek()
            .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_')
        {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = match text {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        self.add_token(token_type);
    }
}

#[cfg(test)]
mod tests {
    use super::{Literal, Scanner, Token, TokenType};
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("(", TokenType::LeftParen ; "Left Parenthesis")]
    #[test_case(")", TokenType::RightParen ; "Right Parenthesis")]
    #[test_case("{", TokenType::LeftBrace ; "Left Brace")]
    #[test_case("}", TokenType::RightBrace ; "Right Brace")]
    #[test_case(",", TokenType::Comma; "Comma")]
    #[test_case(".", TokenType::Dot; "Dot")]
    #[test_case("-", TokenType::Minus; "Minus")]
    #[test_case("+", TokenType::Plus; "Plus")]
    #[test_case(";", TokenType::Semicolon; "Semicolon")]
    #[test_case("/", TokenType::Slash; "Slash")]
    #[test_case("*", TokenType::Star; "Star")]
    fn single_character_tokens(input: &str, out: TokenType) {
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![Token::new(out, input, 1), Token::new(TokenType::Eof, "", 1)]
        )
    }

    #[test_case("!", TokenType::Bang; "Bang")]
    #[test_case("!=", TokenType::BangEqual; "Bang Equal")]
    #[test_case("=", TokenType::Equal; "Equal")]
    #[test_case("==", TokenType::EqualEqual; "Equal Equal")]
    #[test_case(">", TokenType::Greater; "Greater")]
    #[test_case(">=", TokenType::GreaterEqual; "Greater Equal")]
    #[test_case("<", TokenType::Less; "Less")]
    #[test_case("<=", TokenType::LessEqual; "Less Equal")]
    fn one_or_two_character_tokens(input: &str, out: TokenType) {
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![Token::new(out, input, 1), Token::new(TokenType::Eof, "", 1)]
        )
    }

    #[test_case("and", TokenType::And; "And keyword")]
    #[test_case("class", TokenType::Class; "Class keyword")]
    #[test_case("else", TokenType::Else; "Else keyword")]
    #[test_case("false", TokenType::False; "False keyword")]
    #[test_case("fun", TokenType::Fun; "Fun keyword")]
    #[test_case("for", TokenType::For; "For keyword")]
    #[test_case("if", TokenType::If; "If keyword")]
    #[test_case("nil", TokenType::Nil; "Nil keyword")]
    #[test_case("or", TokenType::Or; "Or keyword")]
    #[test_case("print", TokenType::Print; "Print keyword")]
    #[test_case("return", TokenType::Return; "Return keyword")]
    #[test_case("super", TokenType::Super; "Super keyword")]
    #[test_case("this", TokenType::This; "This keyword")]
    #[test_case("true", TokenType::True; "True keyword")]
    #[test_case("var", TokenType::Var; "Var keyword")]
    #[test_case("while", TokenType::While; "While keyword")]
    fn keywords(input: &str, out: TokenType) {
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![Token::new(out, input, 1), Token::new(TokenType::Eof, "", 1)]
        )
    }

    #[test_case("\"salam\"", TokenType::String, Literal::String("salam"); "String Literal")]
    #[test_case("1.23", TokenType::Number, Literal::Number(1.23); "Numeric Literal")]
    #[test_case("134", TokenType::Number, Literal::Number(134.0); "Numeric Literal Integers")]
    fn literals<'a>(input: &'a str, out: TokenType, literal: Literal<'a>) {
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![
                Token::new_with_literal(out, input, 1, Some(literal)),
                Token::new(TokenType::Eof, "", 1)
            ]
        )
    }

    #[test_case("new_var", TokenType::Identifier; "Identifier")]
    fn identifier(input: &str, out: TokenType) {
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![Token::new(out, input, 1), Token::new(TokenType::Eof, "", 1)]
        )
    }

    #[test]
    fn var_definition() {
        let input = "var input = 12";
        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![
                Token::new(TokenType::Var, "var", 1),
                Token::new(TokenType::Identifier, "input", 1),
                Token::new(TokenType::Equal, "=", 1),
                Token::new_with_literal(TokenType::Number, "12", 1, Some(Literal::Number(12.0))),
                Token::new(TokenType::Eof, "", 1)
            ]
        )
    }

    #[test]
    fn multiline_comment() {
        let input = r#"/*khar
gav
*/
var input = 12"#;

        let mut scanner = Scanner::new(&input);
        scanner.scan_tokens();
        assert_eq!(
            scanner.tokens,
            vec![
                Token::new(TokenType::Var, "var", 4),
                Token::new(TokenType::Identifier, "input", 4),
                Token::new(TokenType::Equal, "=", 4),
                Token::new_with_literal(TokenType::Number, "12", 4, Some(Literal::Number(12.0))),
                Token::new(TokenType::Eof, "", 4)
            ]
        )
    }
}
