use std::fmt;

use crate::tokenize::{Token, TokenType};

#[derive(Debug)]
pub struct Statement {
    pub ty: StatementType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub enum ReassignmentOperator {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

impl TryFrom<TokenType> for ReassignmentOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Equal => Ok(ReassignmentOperator::Assign),
            TokenType::PlusEq => Ok(ReassignmentOperator::AddAssign),
            TokenType::MinusEq => Ok(ReassignmentOperator::SubtractAssign),
            TokenType::StarEq => Ok(ReassignmentOperator::MultiplyAssign),
            TokenType::SlashEq => Ok(ReassignmentOperator::DivideAssign),
            TokenType::PercentEq => Ok(ReassignmentOperator::ModuloAssign),
            TokenType::AmpersandEq => Ok(ReassignmentOperator::BitwiseAndAssign),
            TokenType::PipeEq => Ok(ReassignmentOperator::BitwiseOrAssign),
            TokenType::CarotEq => Ok(ReassignmentOperator::BitwiseXorAssign),
            TokenType::DoubleLChevronEq => Ok(ReassignmentOperator::LeftShiftAssign),
            TokenType::DoubleRChevronEq => Ok(ReassignmentOperator::RightShiftAssign),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Pattern {
    Literal(ExpressionType),
    Variable(String),
    Dictionary {
        pairs: Vec<(String, Pattern)>,
        rest: bool,
    },
    Array {
        elements: Vec<Pattern>,
        rest: bool,
    },
}

#[derive(Debug)]
pub enum StatementType {
    Annotation {
        annotation: Expression,
        target: Box<Statement>,
    },
    Return(Expression),
    Match {
        expression: Expression,
        arms: Vec<(Vec<Pattern>, Vec<Statement>)>,
    },
    Reassignment {
        key: Expression,
        operator: ReassignmentOperator,
        value: Expression,
    },
    ClassName(String),
    Extends(String),
    If {
        condition: Expression,
        when_true: Vec<Statement>,
        elifs: Vec<(Expression, Vec<Statement>)>,
        when_false: Vec<Statement>,
    },
    While {
        condition: Expression,
        statements: Vec<Statement>,
    },
    For {
        variable: String,
        iterator: Expression,
        statements: Vec<Statement>,
    },
    Expression(Expression),
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
    Enum {
        name: Option<String>,
        values: Vec<(String, Option<i64>)>,
    },
    Function {
        name: String,
        args: Vec<String>,
        statements: Vec<Statement>,
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
    Float(f64),
    Array(Vec<Expression>),
    Dictionary {
        kv_pairs: Vec<(String, Expression)>,
    },
    FunctionCall(FunctionCallExpression),
    Negate(Box<Expression>),
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        operator: BinaryOperator,
    },
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

impl TryFrom<TokenType> for BinaryOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Plus => Ok(BinaryOperator::Add),
            TokenType::Minus => Ok(BinaryOperator::Subtract),
            TokenType::Star => Ok(BinaryOperator::Multiply),
            TokenType::Slash => Ok(BinaryOperator::Divide),
            TokenType::Percent => Ok(BinaryOperator::Modulo),
            TokenType::LChevron => Ok(BinaryOperator::GreaterThan),
            TokenType::RChevron => Ok(BinaryOperator::LessThan),
            TokenType::LChevronEq => Ok(BinaryOperator::GreaterThanOrEqual),
            TokenType::RChevronEq => Ok(BinaryOperator::LessThanOrEqual),
            TokenType::EqualEqual => Ok(BinaryOperator::Equal),
            TokenType::BangEq => Ok(BinaryOperator::NotEqual),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct FunctionCallExpression {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<TokenType>,
        actual: Token,
    },
    UnexpectedEof,
    InvalidIndent {
        expected: usize,
        actual: usize,
        line: usize,
        column: usize,
    },
    ExpectedIndent {
        line: usize,
        column: usize,
    },
    InvalidExpression(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, actual } => {
                let expected_formatted = expected
                    .into_iter()
                    .map(|ty| format!("'{}'", ty.generic_name()))
                    .collect::<Vec<_>>()
                    .join(" or ");

                write!(
                    f,
                    "{}:{}: expected {} but found '{}'",
                    actual.line,
                    actual.column,
                    expected_formatted,
                    actual.ty.generic_name()
                )
            }
            ParseError::UnexpectedEof => {
                write!(f, "{}:{}: unexpected end of file", 0, 0)
            }
            ParseError::ExpectedIndent { line, column } => {
                write!(f, "{}:{}: expected indentation", line, column)
            }
            ParseError::InvalidIndent {
                expected,
                actual,
                line,
                column,
            } => {
                write!(
                    f,
                    "{}:{}: expected indentation of {} but found {}",
                    line, column, expected, actual
                )
            }
            ParseError::InvalidExpression(tok) => {
                write!(
                    f,
                    "{}:{}: invalid expression- found '{}'",
                    tok.line,
                    tok.column,
                    tok.ty.generic_name()
                )
            }
        }
    }
}

struct IndentInfo {
    level: usize,
    line: usize,
    column: usize,
}

struct Parser {
    tokens: Vec<Token>,
    read_index: usize,
    indent_stack: Vec<usize>,
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
            indent_stack: Vec::new(),
        }
    }

    fn next(&mut self) -> Option<Result<Statement, ParseError>> {
        let indent_info = self.consume_until_nonempty_line()?;

        if let Token {
            ty: TokenType::Indent { .. },
            ..
        } = self.peek_tok()?
        {
            self.consume_token();
        };

        let epected_indent = self.indent_stack.last().copied().unwrap_or(0);
        if indent_info.level != epected_indent {
            return Some(Err(ParseError::InvalidIndent {
                expected: epected_indent,
                actual: indent_info.level,
                line: indent_info.line,
                column: indent_info.column,
            }));
        }

        Some(self.annotation())
    }

    fn annotation(&mut self) -> Result<Statement, ParseError> {
        let Some(first_tok) = self.peek_tok().cloned() else {
            return Err(ParseError::UnexpectedEof);
        };

        match first_tok.ty {
            TokenType::At => {
                self.consume_token();
            }
            TokenType::ClassName => {
                self.consume_token();
                return self.class_name(first_tok);
            }
            TokenType::Extends => {
                self.consume_token();
                return self.extends(first_tok);
            }
            TokenType::If => {
                self.consume_token();
                return self.if_statement(first_tok);
            }
            TokenType::While => {
                self.consume_token();
                return self.while_statement(first_tok);
            }
            TokenType::For => {
                self.consume_token();
                return self.for_statement(first_tok);
            }
            TokenType::Var | TokenType::Const => {
                self.consume_token();
                return self.var_or_const(first_tok);
            }
            TokenType::Enum => {
                self.consume_token();
                return self.parse_enum(first_tok);
            }
            TokenType::Func => {
                self.consume_token();
                return self.parse_function(first_tok);
            }
            TokenType::Match => {
                self.consume_token();
                return self.parse_match(first_tok);
            }
            TokenType::Return => {
                self.consume_token();
                let expr = self.expression()?;
                self.expect(TokenType::Eol)?;
                return Ok(Statement {
                    ty: StatementType::Return(expr),
                    line: first_tok.line,
                    column: first_tok.column,
                });
            }
            _ => return self.reassignment_statement(),
        };

        let annotation = self.expression()?;
        self.expect(TokenType::Eol)?;
        let indent_info = self
            .consume_until_nonempty_line()
            .ok_or(ParseError::UnexpectedEof)?;

        let expected_indent = self.indent_stack.last().copied().unwrap_or(0);

        if indent_info.level != expected_indent {
            return Err(ParseError::InvalidIndent {
                expected: expected_indent,
                actual: indent_info.level,
                line: indent_info.line,
                column: indent_info.column,
            });
        }

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

    fn for_statement(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let variable = self.expect_identifier()?;
        self.expect(TokenType::In)?;
        let iterator = self.expression()?;
        self.expect(TokenType::Colon)?;

        let statements = self.parse_block_scope()?;

        Ok(Statement {
            ty: StatementType::For {
                variable,
                iterator,
                statements,
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn while_statement(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let condition = self.expression()?;
        self.expect(TokenType::Colon)?;
        self.expect(TokenType::Eol)?;

        let statements = self.parse_block_scope()?;

        Ok(Statement {
            ty: StatementType::While {
                condition,
                statements,
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
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::Eol, TokenType::Extends],
                        actual: other.clone(),
                    })
                }
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
        self.expect(TokenType::Eol)?;

        Ok(Statement {
            ty: StatementType::Extends(name),
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn if_statement(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let condition = self.expression()?;

        self.expect(TokenType::Colon)?;
        self.expect(TokenType::Eol)?;
        let when_true = self.parse_block_scope()?;

        let mut elifs = Vec::new();
        loop {
            let Some(indent_info) = self.consume_until_nonempty_line() else {
                break;
            };

            let current_indent = self.indent_stack.last().copied().unwrap_or(0);
            if indent_info.level != current_indent {
                break;
            }

            let Some(Token {
                ty: TokenType::Elif,
                ..
            }) = self.peek_n(2)
            else {
                break;
            };

            self.consume_token();
            self.consume_token();

            let condition = self.expression()?;

            self.expect(TokenType::Colon)?;
            self.expect(TokenType::Eol)?;

            let statements = self.parse_block_scope()?;
            elifs.push((condition, statements));
        }

        let when_false = 'when_false: {
            let Some(indent_info) = self.consume_until_nonempty_line() else {
                break 'when_false vec![];
            };

            let current_indent = self.indent_stack.last().copied().unwrap_or(0);
            if indent_info.level != current_indent {
                break 'when_false vec![];
            }

            let Some(Token {
                ty: TokenType::Else,
                ..
            }) = self.peek_n(2)
            else {
                break 'when_false vec![];
            };

            self.consume_token();
            self.consume_token();

            self.expect(TokenType::Colon)?;
            self.expect(TokenType::Eol)?;

            self.parse_block_scope()?
        };

        Ok(Statement {
            ty: StatementType::If {
                condition,
                when_true,
                elifs,
                when_false,
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn var_or_const(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let konst = first_tok.ty == TokenType::Const;
        let identifier = self.expect_identifier()?;

        if self.consume_if(TokenType::Colon) {
            let ty = self.expect_identifier()?;
            self.expect(TokenType::Eol)?;

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

        if self.consume_if(TokenType::ColonEqual) {
            let value = self.expression()?;
            self.expect(TokenType::Eol)?;

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

        self.expect(TokenType::Equal)?;
        let expr = self.expression()?;
        self.expect(TokenType::Eol)?;

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

    fn parse_enum(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name_tok = self.peek_tok().cloned();
        let name = match name_tok {
            Some(Token {
                ty: TokenType::Identifier(name),
                ..
            }) => {
                self.consume_token();
                Some(name.clone())
            }
            _ => None,
        };

        self.expect(TokenType::LBrace)?;

        let mut values = Vec::new();

        loop {
            let name = self.expect_identifier()?;

            let ordinal = if self.consume_if(TokenType::Equal) {
                let is_negative = self.consume_if(TokenType::Minus);
                match self.expect_token()? {
                    Token {
                        ty: TokenType::Integer(value),
                        ..
                    } => Some(if is_negative { -value } else { *value }),
                    other => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![TokenType::Integer(0)],
                            actual: other.clone(),
                        })
                    }
                }
            } else {
                None
            };

            values.push((name, ordinal));

            match self.expect_token()? {
                Token {
                    ty: TokenType::RBrace,
                    ..
                } => break,
                Token {
                    ty: TokenType::Comma,
                    ..
                } => {}
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::RBrace, TokenType::Comma],
                        actual: other.clone(),
                    })
                }
            }
        }

        self.expect(TokenType::Eol)?;

        Ok(Statement {
            ty: StatementType::Enum { name, values },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let token = self.expect_token()?.clone();

        match &token.ty {
            TokenType::Var => {
                let name = self.expect_identifier()?;
                Ok(Pattern::Variable(name))
            }
            TokenType::LBrace => {
                let mut pairs = Vec::new();
                let mut rest = false;

                if self.consume_if(TokenType::RBrace) {
                    return Ok(Pattern::Dictionary { pairs, rest: false });
                }

                loop {
                    if self.consume_if(TokenType::DotDot) {
                        rest = true;
                        self.expect(TokenType::RBrace)?;
                        break;
                    }

                    let key = self.expect_identifier()?;
                    self.expect(TokenType::Colon)?;
                    let value = self.parse_pattern()?;
                    pairs.push((key, value));

                    match self.expect_token()?.ty {
                        TokenType::RBrace => break,
                        TokenType::Comma => continue,
                        _ => {
                            return Err(ParseError::UnexpectedToken {
                                expected: vec![TokenType::RBrace, TokenType::Comma],
                                actual: token,
                            })
                        }
                    }
                }

                Ok(Pattern::Dictionary { pairs, rest })
            }
            TokenType::LBracket => {
                let mut elements = Vec::new();
                let mut rest = false;

                if self.consume_if(TokenType::RBracket) {
                    return Ok(Pattern::Array {
                        elements,
                        rest: false,
                    });
                }

                loop {
                    if self.consume_if(TokenType::DotDot) {
                        rest = true;
                        self.expect(TokenType::RBracket)?;
                        break;
                    }

                    elements.push(self.parse_pattern()?);

                    match self.expect_token()?.ty {
                        TokenType::RBracket => break,
                        TokenType::Comma => continue,
                        _ => {
                            return Err(ParseError::UnexpectedToken {
                                expected: vec![TokenType::RBracket, TokenType::Comma],
                                actual: token,
                            })
                        }
                    }
                }

                Ok(Pattern::Array { elements, rest })
            }
            _ => {
                let expr = self.literal(&token)?;
                Ok(Pattern::Literal(expr.ty))
            }
        }
    }

    fn parse_match(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let expression = self.expression()?;
        self.expect(TokenType::Colon)?;
        self.expect(TokenType::Eol)?;

        let mut arms = Vec::new();
        let indent_info = self
            .consume_until_nonempty_line()
            .ok_or(ParseError::UnexpectedEof)?;

        let prev_indent = self.indent_stack.last().copied().unwrap_or(0);
        if indent_info.level <= prev_indent {
            return Err(ParseError::ExpectedIndent {
                line: indent_info.line,
                column: indent_info.column,
            });
        }

        self.indent_stack.push(indent_info.level);

        loop {
            let Some(line_indent) = self.consume_until_nonempty_line() else {
                break;
            };

            if line_indent.level != indent_info.level {
                break;
            }

            self.consume_token();

            let mut patterns = Vec::new();
            loop {
                patterns.push(self.parse_pattern()?);
                if !self.consume_if(TokenType::Comma) {
                    break;
                }
            }

            self.expect(TokenType::Colon)?;
            self.expect(TokenType::Eol)?;

            let statements = self.parse_block_scope()?;
            arms.push((patterns, statements));
        }

        self.indent_stack.pop();

        Ok(Statement {
            ty: StatementType::Match { expression, arms },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_function(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        self.expect(TokenType::LParen)?;

        if self.consume_if(TokenType::RParen) {
            return Ok(Statement {
                ty: StatementType::Function {
                    name,
                    args: Vec::new(),
                    statements: self.parse_block_scope()?,
                },
                line: first_tok.line,
                column: first_tok.column,
            });
        }

        let mut args = Vec::new();

        loop {
            args.push(self.expect_identifier()?);

            match self.expect_token()? {
                Token {
                    ty: TokenType::RParen,
                    ..
                } => break,
                Token {
                    ty: TokenType::Comma,
                    ..
                } => {}
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::RParen, TokenType::Comma],
                        actual: other.clone(),
                    })
                }
            }
        }

        self.expect(TokenType::Colon)?;
        self.expect(TokenType::Eol)?;

        Ok(Statement {
            ty: StatementType::Function {
                name,
                args,
                statements: self.parse_block_scope()?,
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_block_scope(&mut self) -> Result<Vec<Statement>, ParseError> {
        let indent_info = self
            .consume_until_nonempty_line()
            .ok_or(ParseError::UnexpectedEof)?;

        let mut statements = Vec::new();
        let prev_indent = self.indent_stack.last().copied().unwrap_or(0);

        if indent_info.level <= prev_indent {
            return Err(ParseError::ExpectedIndent {
                line: indent_info.line,
                column: indent_info.column,
            });
        }

        self.indent_stack.push(indent_info.level);

        loop {
            let Some(line_indent_info) = self.consume_until_nonempty_line() else {
                break;
            };

            if line_indent_info.level != indent_info.level {
                break;
            }

            self.consume_token();

            statements.push(self.annotation()?);
        }

        self.indent_stack.pop();

        Ok(statements)
    }

    fn reassignment_statement(&mut self) -> Result<Statement, ParseError> {
        let key = self.expression()?;

        let Some(reassignment) = self
            .peek_tok()
            .and_then(|tok| ReassignmentOperator::try_from(tok.ty.clone()).ok())
        else {
            return Ok(Statement {
                line: key.line,
                column: key.column,
                ty: StatementType::Expression(key),
            });
        };

        self.consume_token();
        let value = self.expression()?;
        self.expect(TokenType::Eol)?;

        Ok(Statement {
            line: key.line,
            column: key.column,
            ty: StatementType::Reassignment {
                key,
                operator: reassignment,
                value,
            },
        })
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.add_sub()
    }

    fn add_sub(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.mul_div()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Plus | TokenType::Minus => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.mul_div()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn mul_div(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Star | TokenType::Slash | TokenType::Percent => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.comparison()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expression = self.negate()?;

        loop {
            let Some(operator) = self.peek_tok() else {
                break;
            };

            match BinaryOperator::try_from(operator.ty.clone()) {
                Ok(op) => {
                    self.consume_token();
                    expression = Expression {
                        line: expression.line,
                        column: expression.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expression),
                            rhs: Box::new(self.comparison()?),
                            operator: op,
                        },
                    }
                }
                Err(_) => break,
            }
        }

        Ok(expression)
    }

    fn negate(&mut self) -> Result<Expression, ParseError> {
        let Some(Token {
            ty: TokenType::Minus,
            line,
            column,
        }) = self.peek_tok()
        else {
            return self.attribute_access();
        };

        let line = *line;
        let column = *column;
        self.next();

        let target = self.negate()?;

        Ok(Expression {
            ty: ExpressionType::Negate(Box::new(target)),
            line,
            column,
        })
    }

    fn attribute_access(&mut self) -> Result<Expression, ParseError> {
        let token = self.expect_token()?.clone();
        let mut expr = self.literal(&token)?;

        loop {
            let Some(token) = self.peek_tok() else { break };
            let token = token.clone();

            match token.ty {
                TokenType::LParen => {
                    self.consume_token();
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
                TokenType::Float(float) => ExpressionType::Float(float),
                TokenType::LBracket => ExpressionType::Array(self.parse_array_literal()?),
                TokenType::LBrace => ExpressionType::Dictionary {
                    kv_pairs: self.parse_dict_literal()?,
                },
                _ => return Err(ParseError::InvalidExpression(first_tok.clone())),
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_array_literal(&mut self) -> Result<Vec<Expression>, ParseError> {
        if self.consume_if(TokenType::RBracket) {
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
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::RBracket, TokenType::Comma],
                        actual: other.clone(),
                    })
                }
            }
        }

        Ok(expressions)
    }

    fn parse_dict_literal(&mut self) -> Result<Vec<(String, Expression)>, ParseError> {
        if self.consume_if(TokenType::RBrace) {
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
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![TokenType::String("".to_owned()), TokenType::Integer(0)],
                            actual: key_token.clone(),
                        })
                    }
                },
                Token {
                    ty: TokenType::Equal,
                    ..
                } => match key_token.ty {
                    TokenType::Identifier(key) => key,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![TokenType::Identifier("".to_owned())],
                            actual: key_token.clone(),
                        })
                    }
                },
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::Colon, TokenType::Equal],
                        actual: other.clone(),
                    })
                }
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
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::RBrace, TokenType::Comma],
                        actual: other.clone(),
                    })
                }
            }
        }

        Ok(kv)
    }

    fn parse_function_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        if self.consume_if(TokenType::RParen) {
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
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![TokenType::RParen, TokenType::Comma],
                        actual: tok.clone(),
                    })
                }
            }
        }

        Ok(args)
    }

    fn peek_tok(&self) -> Option<&Token> {
        self.peek_n(1)
    }

    fn peek_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.read_index + n - 1)
    }

    fn consume_token(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.read_index)?;
        self.read_index += 1;
        Some(tok)
    }

    fn consume_if(&mut self, ty: TokenType) -> bool {
        match self.peek_tok() {
            Some(tok) if tok.ty == ty => {
                self.consume_token();
                true
            }
            _ => false,
        }
    }

    fn expect_token(&mut self) -> Result<&Token, ParseError> {
        self.consume_token().ok_or(ParseError::UnexpectedEof)
    }

    fn expect(&mut self, ty: TokenType) -> Result<(), ParseError> {
        let tok = self.expect_token()?;
        if std::mem::discriminant(&ty) == std::mem::discriminant(&tok.ty) {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![ty],
                actual: tok.clone(),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.expect_token()? {
            Token {
                ty: TokenType::Identifier(name),
                ..
            } => Ok(name.to_owned()),
            other => Err(ParseError::UnexpectedToken {
                expected: vec![TokenType::Identifier("".to_owned())],
                actual: other.clone(),
            }),
        }
    }

    fn consume_until_nonempty_line(&mut self) -> Option<IndentInfo> {
        loop {
            while self.consume_if(TokenType::Eol) {}

            let indent_info = match self.peek_tok()? {
                Token {
                    ty: TokenType::Indent { level },
                    line,
                    column,
                } => IndentInfo {
                    level: *level,
                    line: *line,
                    column: *column,
                },
                other => {
                    return Some(IndentInfo {
                        level: 0,
                        line: other.line,
                        column: other.column,
                    })
                }
            };

            match self.peek_n(2) {
                Some(Token {
                    ty: TokenType::Eol, ..
                }) => {
                    self.consume_token();
                    self.consume_token();
                }
                Some(_) | None => return Some(indent_info),
            }
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
