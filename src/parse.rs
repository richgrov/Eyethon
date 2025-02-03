use std::fmt;

use crate::tokenize::{Token, TokenType};

#[derive(Debug)]
pub struct Statement {
    pub ty: StatementType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum StatementType {
    Annotation {
        annotation: Box<Expression>,
        target: Box<Statement>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub ty: ExpressionType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum ExpressionType {
    Identifier(String),
    FunctionCall(FunctionCallExpression),
}

#[derive(Debug)]
pub struct FunctionCallExpression {}

pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(tok) => {
                write!(f, "{}:{}: unexpected token {:?}", tok.line, tok.column, tok)
            }
            ParseError::UnexpectedEof => {
                write!(f, "{}:{}: unexpected end of file", 0, 0)
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
        let without_comment = tokens
            .into_iter()
            .filter(|tok| {
                std::mem::discriminant(&tok.ty)
                    != std::mem::discriminant(&TokenType::Comment {
                        text: "".to_owned(),
                    })
            })
            .collect();

        Self {
            tokens: without_comment,
            read_index: 0,
        }
    }

    fn next(&mut self) -> Option<Result<Statement, ParseError>> {
        if let None = self.peek_tok() {
            return None;
        }

        Some(self.annotation())
    }

    fn annotation(&mut self) -> Result<Statement, ParseError> {
        let first_tok = match self.next_token() {
            Some(tok) => tok.clone(),
            None => return Err(ParseError::UnexpectedEof),
        };

        let TokenType::At = first_tok.ty else {
            return Err(ParseError::UnexpectedToken(first_tok));
        };

        let annotation = self.expression()?;
        let target = self.annotation()?;

        Ok(Statement {
            ty: StatementType::Annotation {
                annotation: Box::new(annotation),
                target: Box::new(target),
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        let token = match self.next_token() {
            Some(token) => token.clone(),
            None => return Err(ParseError::UnexpectedEof),
        };

        match token.ty {
            TokenType::Identifier(name) => Ok(Expression {
                ty: ExpressionType::Identifier(name),
                line: token.line,
                column: token.column,
            }),
            _ => Err(ParseError::UnexpectedToken(token)),
        }
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
