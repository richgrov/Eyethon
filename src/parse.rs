use std::fmt;

use crate::tokenize::{Token, TokenType};

#[derive(Debug)]
pub struct Statement {
    pub ty: StatementType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum StatementType {}

pub enum ParseError {
    UnexpectedToken(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(tok) => {
                write!(f, "{}:{}: unexpected token {:?}", tok.line, tok.column, tok)
            }
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    read_index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            read_index: 0,
        }
    }

    fn next(&mut self) -> Option<Result<Statement, ParseError>> {
        Some(loop {
            let token = self.next_token()?;

            match token.ty {
                TokenType::Comment { .. } => {}
                _ => break Err(ParseError::UnexpectedToken(token.clone())),
            }
        })
    }

    fn peek_tok(&self) -> Option<&Token> {
        self.tokens.get(self.read_index)
    }

    fn next_token(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.read_index)?;
        self.read_index += 1;
        Some(tok)
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut parser = Parser::new(tokens);
    let mut result = Vec::new();

    while let Some(statement) = parser.next() {
        result.push(statement?);
    }

    Ok(result)
}
