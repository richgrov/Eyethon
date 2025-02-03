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
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match self.ty {
            TokenizerErrorType::Unexpected { character } => format!("unexpected '{}'", character),
            TokenizerErrorType::MixedIndent => format!("mixed indentation"),
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
                    self.beginning_new_line = false;
                    break self.indent(c);
                }

                ' ' | '\t' => self.skip_line_whitespace(),

                '\n' => {
                    self.beginning_new_line = true;
                }

                '#' => break Ok(self.comment()),

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
