use std::fmt;

#[derive(Debug)]
pub struct Token {
    ty: TokenType,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub enum TokenType {
    Indent { level: usize },
    Comment { text: String },
    Identifier(String),
    String(String),
    Integer(i64),

    If,
    Elif,
    Else,
    For,
    While,
    Match,
    When,
    Break,
    Continue,
    Pass,
    Return,
    Class,
    ClassName,
    Extends,
    Is,
    In,
    As,
    Self_,
    Super,
    Signal,
    Func,
    Static,
    Const,
    Enum,
    Var,
    Breakpoint,
    Preload,
    Await,
    Yield,
    Assert,
    Void,

    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    Bang,
    BangEq,
    Ampersand,
    AmpersandEq,
    Pipe,
    PipeEq,
    Carot,
    CarotEq,
    LChevron,
    LChevronEq,
    RChevron,
    RChevronEq,
    DoubleLChevron,
    DoubleLChevronEq,
    DoubleRChevron,
    DoubleRChevronEq,
    Equal,
    EqualEqual,
    At,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Semicolon,
    Dot,
    Tilde,
}

enum Indent {
    Space,
    Tab,
}

impl Indent {
    pub fn char(&self) -> char {
        match self {
            Indent::Space => ' ',
            Indent::Tab => '\t',
        }
    }
}

struct Tokenizer {
    source: Vec<char>,
    read_index: usize,
    detected_indent: Option<Indent>,
    beginning_new_line: bool,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub struct TokenizerError {
    ty: TokenizerErrorType,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub enum TokenizerErrorType {
    Unexpected { character: char },
    MixedIndent,
    UnterminatedString { delim: char },
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match self.ty {
            TokenizerErrorType::Unexpected { character } => format!("unexpected '{}'", character),
            TokenizerErrorType::MixedIndent => format!("mixed indentation"),
            TokenizerErrorType::UnterminatedString { delim } => {
                format!("string not terminated with {}", delim)
            }
        };

        write!(f, "{}:{}: {}", self.line, self.column, message)
    }
}

impl Tokenizer {
    pub fn new(contents: &str) -> Tokenizer {
        Tokenizer {
            source: contents.chars().collect(),
            read_index: 0,
            detected_indent: None,
            beginning_new_line: true,
            line: 1,
            column: 0,
        }
    }

    pub fn next(&mut self) -> Option<Result<Token, TokenizerError>> {
        Some(loop {
            let c = self.next_char()?;
            match c {
                ' ' | '\t' if self.beginning_new_line => {
                    break self.indent(c);
                }

                ' ' | '\t' => self.skip_line_whitespace(),
                '\n' => {}

                '#' => break Ok(self.comment()),

                '+' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::PlusEq));
                    }
                    break Ok(self.mk_token(TokenType::Plus));
                }

                '-' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::MinusEq));
                    }
                    break Ok(self.mk_token(TokenType::Minus));
                }

                '*' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::StarEq));
                    }
                    break Ok(self.mk_token(TokenType::Star));
                }

                '/' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::SlashEq));
                    }
                    break Ok(self.mk_token(TokenType::Slash));
                }

                '%' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::PercentEq));
                    }
                    break Ok(self.mk_token(TokenType::Percent));
                }

                '!' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::BangEq));
                    }
                    break Ok(self.mk_token(TokenType::Bang));
                }

                '&' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::AmpersandEq));
                    }
                    break Ok(self.mk_token(TokenType::Ampersand));
                }

                '|' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::PipeEq));
                    }
                    break Ok(self.mk_token(TokenType::Pipe));
                }

                '^' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::CarotEq));
                    }
                    break Ok(self.mk_token(TokenType::Carot));
                }

                '<' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::LChevronEq));
                    }
                    Some('<') => {
                        self.next_char();
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            break Ok(self.mk_token(TokenType::DoubleLChevronEq));
                        }
                        break Ok(self.mk_token(TokenType::DoubleLChevron));
                    }
                    _ => break Ok(self.mk_token(TokenType::LChevron)),
                },

                '>' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::RChevronEq));
                    }
                    Some('>') => {
                        self.next_char();
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            break Ok(self.mk_token(TokenType::DoubleRChevronEq));
                        }
                        break Ok(self.mk_token(TokenType::DoubleRChevron));
                    }
                    _ => break Ok(self.mk_token(TokenType::RChevron)),
                },

                '=' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        break Ok(self.mk_token(TokenType::EqualEqual));
                    }
                    break Ok(self.mk_token(TokenType::Equal));
                }

                '(' => break Ok(self.mk_token(TokenType::LParen)),
                ')' => break Ok(self.mk_token(TokenType::RParen)),
                '{' => break Ok(self.mk_token(TokenType::LBrace)),
                '}' => break Ok(self.mk_token(TokenType::RBrace)),
                '[' => break Ok(self.mk_token(TokenType::LBracket)),
                ']' => break Ok(self.mk_token(TokenType::RBracket)),
                ':' => break Ok(self.mk_token(TokenType::Colon)),
                ',' => break Ok(self.mk_token(TokenType::Comma)),
                ';' => break Ok(self.mk_token(TokenType::Semicolon)),
                '.' => break Ok(self.mk_token(TokenType::Dot)),
                '~' => break Ok(self.mk_token(TokenType::Tilde)),
                '@' => break Ok(self.mk_token(TokenType::At)),

                '"' | '\'' => break self.string(c),

                other if other.is_ascii_alphabetic() || other == '_' => {
                    break Ok(self.identifier(other))
                }

                other if other.is_ascii_digit() => break Ok(self.number(other)),

                other => {
                    break Err(self.mk_error(TokenizerErrorType::Unexpected { character: other }))
                }
            }
        })
    }

    fn indent(&mut self, first: char) -> Result<Token, TokenizerError> {
        let detected_indent = self.detected_indent.get_or_insert_with(|| match first {
            ' ' => Indent::Space,
            '\t' => Indent::Tab,
            _ => unreachable!(),
        });

        if first != detected_indent.char() {
            return Err(self.mk_error(TokenizerErrorType::MixedIndent));
        }

        let mut num_indents = 1;

        loop {
            match self.peek_char() {
                Some(c) if c == first => {
                    num_indents += 1;
                    self.next_char();
                }
                Some(' ') | Some('\t') => {
                    return Err(self.mk_error(TokenizerErrorType::MixedIndent));
                }
                _ => break,
            };
        }

        Ok(self.mk_token(TokenType::Indent { level: num_indents }))
    }

    fn skip_line_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ') | Some('\t') => {
                    self.next_char();
                }
                _ => break,
            }
        }
    }

    fn comment(&mut self) -> Token {
        let mut text = String::with_capacity(16);

        while let Some(c) = self.peek_char() {
            if c == '\n' {
                break;
            }

            text.push(c);
            self.next_char();
        }

        self.mk_token(TokenType::Comment { text })
    }

    fn identifier(&mut self, first: char) -> Token {
        let mut ident = String::with_capacity(8);
        ident.push(first);

        while let Some(c) = self.peek_char() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        let token_type = match ident.as_str() {
            "if" => TokenType::If,
            "elif" => TokenType::Elif,
            "else" => TokenType::Else,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "match" => TokenType::Match,
            "when" => TokenType::When,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "pass" => TokenType::Pass,
            "return" => TokenType::Return,
            "class" => TokenType::Class,
            "class_name" => TokenType::ClassName,
            "extends" => TokenType::Extends,
            "is" => TokenType::Is,
            "in" => TokenType::In,
            "as" => TokenType::As,
            "self" => TokenType::Self_,
            "super" => TokenType::Super,
            "signal" => TokenType::Signal,
            "func" => TokenType::Func,
            "static" => TokenType::Static,
            "const" => TokenType::Const,
            "enum" => TokenType::Enum,
            "var" => TokenType::Var,
            "breakpoint" => TokenType::Breakpoint,
            "preload" => TokenType::Preload,
            "await" => TokenType::Await,
            "yield" => TokenType::Yield,
            "assert" => TokenType::Assert,
            "void" => TokenType::Void,
            _ => TokenType::Identifier(ident),
        };

        self.mk_token(token_type)
    }

    fn string(&mut self, delim: char) -> Result<Token, TokenizerError> {
        let mut text = String::with_capacity(8);
        text.push(delim);

        while let Some(c) = self.peek_char() {
            if c == delim {
                self.next_char();
                return Ok(self.mk_token(TokenType::String(text)));
            }

            text.push(c);
            self.next_char();
        }

        Err(self.mk_error(TokenizerErrorType::UnterminatedString { delim }))
    }

    fn number(&mut self, first: char) -> Token {
        let mut text = String::with_capacity(8);
        text.push(first);

        while let Some(c) = self.peek_char() {
            if c.is_digit(10) {
                text.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        let int = text.parse::<i64>().unwrap();
        self.mk_token(TokenType::Integer(int))
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.read_index).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        if self.read_index >= self.source.len() {
            return None;
        }

        let c = self.source[self.read_index];

        if c == '\n' {
            self.line += 1;
            self.column = 0;
            self.beginning_new_line = true;
        } else if c != ' ' && c != '\t' {
            self.beginning_new_line = false;
        }

        self.column += 1;
        self.read_index += 1;
        Some(c)
    }

    fn mk_token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            line: self.line,
            column: self.column,
        }
    }

    fn mk_error(&self, ty: TokenizerErrorType) -> TokenizerError {
        TokenizerError {
            ty,
            line: self.line,
            column: self.column,
        }
    }
}

pub fn tokenize(contents: &str) -> Result<Vec<Token>, TokenizerError> {
    let mut tokenizer = Tokenizer::new(contents);

    let mut result = Vec::new();

    while let Some(token) = tokenizer.next() {
        result.push(token?);
    }

    Ok(result)
}
