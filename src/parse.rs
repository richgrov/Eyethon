use std::fmt;

use crate::tokenize::Token;

#[derive(Debug)]
pub struct Statement {
    pub ty: StatementType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum StatementType {}

pub struct ParseError {
    ty: ParseErrorType,
    line: usize,
    column: usize,
}

pub enum ParseErrorType {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Statement>, ParseError> {
    Ok(vec![])
}
