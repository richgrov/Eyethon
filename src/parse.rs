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
        annotation: Expression,
        target: Box<Statement>,
    },
    ClassName(String),
    Extends(String),
    Var {
        konst: bool,
        identifier: String,
        value: Expression,
    },
    DefaultVar {
        konst: bool,
        identifier: String,
        ty: String,
    },
    StrictVar {
        konst: bool,
        identifier: String,
        value: Expression,
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
    Dictionary { kv_pairs: Vec<(String, Expression)> },
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
    ExpectedEol { line: usize },
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
            ParseError::ExpectedEol { line } => {
                write!(f, "{}:{}: expected end of line", line, 0)
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
        while self.next_if(TokenType::Eol) {}

        let first_tok = self.expect_token()?.clone();
        match first_tok.ty {
            TokenType::At => {}
            TokenType::ClassName => return self.class_name(first_tok),
            TokenType::Extends => return self.extends(first_tok),
            TokenType::Var | TokenType::Const => return self.var_or_const(first_tok),
            _ => return Err(ParseError::UnexpectedToken(first_tok)),
        }

        let annotation = self.expression()?;
        let target = self.annotation()?;

        Ok(Statement {
            ty: StatementType::Annotation {
                annotation,
                target: Box::new(target),
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn class_name(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        match self.peek_tok() {
            Some(tok) => match tok {
                Token {
                    ty: TokenType::Eol, ..
                }
                | Token {
                    ty: TokenType::Extends,
                    ..
                } => {}
                other => return Err(ParseError::UnexpectedToken(other.clone())),
            },
            None => {}
        }

        Ok(Statement {
            ty: StatementType::ClassName(name),
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn extends(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_eol()?;

        Ok(Statement {
            ty: StatementType::Extends(name),
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn var_or_const(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let konst = first_tok.ty == TokenType::Const;
        let identifier = self.expect_identifier()?;

        if self.next_if(TokenType::Colon) {
            let ty = self.expect_identifier()?;
            self.expect_eol()?;

            return Ok(Statement {
                ty: StatementType::DefaultVar {
                    konst,
                    identifier,
                    ty,
                },
                line: first_tok.line,
                column: first_tok.column,
            });
        }

        if self.next_if(TokenType::ColonEqual) {
            let value = self.expression()?;
            self.expect_eol()?;

            return Ok(Statement {
                ty: StatementType::StrictVar {
                    konst,
                    identifier,
                    value,
                },
                line: first_tok.line,
                column: first_tok.column,
            });
        }

        match self.expect_token()? {
            Token {
                ty: TokenType::Equal,
                ..
            } => {}
            other => return Err(ParseError::UnexpectedToken(other.clone())),
        }

        let expr = self.expression()?;
        self.expect_eol()?;

        Ok(Statement {
            ty: StatementType::Var {
                konst,
                identifier,
                value: expr,
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
        Ok(Expression {
            ty: match first_tok.ty {
                TokenType::Identifier(ref name) => ExpressionType::Identifier(name.clone()),
                TokenType::String(ref string) => ExpressionType::String(string.clone()),
                TokenType::Integer(integer) => ExpressionType::Integer(integer),
                TokenType::LBracket => ExpressionType::Array(self.parse_array_literal()?),
                TokenType::LBrace => ExpressionType::Dictionary {
                    kv_pairs: self.parse_dict_literal()?,
                },
                _ => return Err(ParseError::UnexpectedToken(first_tok.clone())),
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_array_literal(&mut self) -> Result<Vec<Expression>, ParseError> {
        if self.next_if(TokenType::RBracket) {
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

    fn parse_dict_literal(&mut self) -> Result<Vec<(String, Expression)>, ParseError> {
        if self.next_if(TokenType::RBrace) {
            return Ok(Vec::new());
        }

        let mut kv = Vec::new();

        loop {
            let key_token = self.expect_token()?.clone();

            let key = match self.expect_token()? {
                Token {
                    ty: TokenType::Colon,
                    ..
                } => match key_token.ty {
                    TokenType::String(key) => key,
                    TokenType::Integer(int) => int.to_string(),
                    _ => return Err(ParseError::UnexpectedToken(key_token.clone())),
                },
                Token {
                    ty: TokenType::Equal,
                    ..
                } => match key_token.ty {
                    TokenType::Identifier(key) => key,
                    _ => return Err(ParseError::UnexpectedToken(key_token.clone())),
                },
                other => return Err(ParseError::UnexpectedToken(other.clone())),
            };

            let value = self.expression()?;
            kv.push((key, value));

            match self.expect_token()? {
                Token {
                    ty: TokenType::RBrace,
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

        Ok(kv)
    }

    fn parse_function_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        if self.next_if(TokenType::RParen) {
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

    fn next_if(&mut self, ty: TokenType) -> bool {
        match self.peek_tok() {
            Some(tok) if tok.ty == ty => {
                self.next_token();
                true
            }
            _ => false,
        }
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

    fn expect_eol(&mut self) -> Result<(), ParseError> {
        match self.next_token() {
            Some(Token {
                ty: TokenType::Eol, ..
            })
            | None => Ok(()),
            Some(other) => Err(ParseError::ExpectedEol { line: other.line }),
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
