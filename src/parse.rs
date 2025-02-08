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
    ClassName(String),
    Extends(String),
    Var {
        identifier: String,
        value: Box<Expression>,
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
    String(String),
    Integer(i64),
    Array(Vec<Expression>),
    FunctionCall(FunctionCallExpression),
}

#[derive(Debug)]
pub struct FunctionCallExpression {
    callee: Box<Expression>,
    args: Vec<Expression>,
}

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
        let first_tok = self.expect_token()?.clone();
        match first_tok.ty {
            TokenType::At => {}
            TokenType::ClassName => return self.class_name(first_tok),
            TokenType::Extends => return self.extends(first_tok),
            TokenType::Var => return self.var(first_tok),
            _ => return Err(ParseError::UnexpectedToken(first_tok)),
        }

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

    fn class_name(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        Ok(Statement {
            ty: StatementType::ClassName(name),
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn extends(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        Ok(Statement {
            ty: StatementType::Extends(name),
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn var(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let identifier = self.expect_identifier()?;

        match self.expect_token()? {
            Token {
                ty: TokenType::Equal,
                ..
            } => {}
            other => return Err(ParseError::UnexpectedToken(other.clone())),
        }

        let expr = self.expression()?;

        Ok(Statement {
            ty: StatementType::Var {
                identifier,
                value: Box::new(expr),
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.expect_token()?.clone();
        let mut expr = self.literal(&token)?;

        loop {
            let Some(token) = self.peek_tok() else { break };
            let token = token.clone();

            match token.ty {
                TokenType::LParen => {
                    self.next_token();
                    let args = self.parse_function_args()?;

                    expr = Expression {
                        ty: ExpressionType::FunctionCall(FunctionCallExpression {
                            callee: Box::new(expr),
                            args,
                        }),
                        line: token.line,
                        column: token.column,
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn literal(&mut self, first_tok: &Token) -> Result<Expression, ParseError> {
        match first_tok.ty {
            TokenType::Identifier(ref name) => Ok(Expression {
                ty: ExpressionType::Identifier(name.clone()),
                line: first_tok.line,
                column: first_tok.column,
            }),
            TokenType::String(ref string) => Ok(Expression {
                ty: ExpressionType::String(string.clone()),
                line: first_tok.line,
                column: first_tok.column,
            }),
            TokenType::Integer(integer) => Ok(Expression {
                ty: ExpressionType::Integer(integer),
                line: first_tok.line,
                column: first_tok.column,
            }),
            TokenType::LBracket => Ok(Expression {
                ty: ExpressionType::Array(self.parse_array_literal()?),
                line: first_tok.line,
                column: first_tok.column,
            }),
            _ => Err(ParseError::UnexpectedToken(first_tok.clone())),
        }
    }

    fn parse_array_literal(&mut self) -> Result<Vec<Expression>, ParseError> {
        if let Some(Token {
            ty: TokenType::RBracket,
            ..
        }) = self.peek_tok()
        {
            self.next_token();
            return Ok(Vec::new());
        }

        let mut expressions = Vec::new();

        loop {
            expressions.push(self.expression()?);

            match self.expect_token()? {
                Token {
                    ty: TokenType::RBracket,
                    ..
                } => {
                    break;
                }
                Token {
                    ty: TokenType::Comma,
                    ..
                } => {}
                other => return Err(ParseError::UnexpectedToken(other.clone())),
            }
        }

        Ok(expressions)
    }

    fn parse_function_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        if let Some(Token {
            ty: TokenType::RParen,
            ..
        }) = self.peek_tok()
        {
            self.next_token();
            return Ok(Vec::new());
        }

        let mut args = Vec::new();

        loop {
            args.push(self.expression()?);

            let tok = self.expect_token()?;
            match tok.ty {
                TokenType::RParen => {
                    break;
                }
                TokenType::Comma => {}
                _ => return Err(ParseError::UnexpectedToken(tok.clone())),
            }
        }

        Ok(args)
    }

    fn peek_tok(&self) -> Option<&Token> {
        self.tokens.get(self.read_index)
    }

    fn next_token(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.read_index)?;
        self.read_index += 1;
        Some(tok)
    }

    fn expect_token(&mut self) -> Result<&Token, ParseError> {
        self.next_token().ok_or(ParseError::UnexpectedEof)
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.expect_token()? {
            Token {
                ty: TokenType::Identifier(name),
                ..
            } => Ok(name.to_owned()),
            other => Err(ParseError::UnexpectedToken(other.clone())),
        }
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
