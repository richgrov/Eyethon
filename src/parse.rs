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
        name: String,
        args: Vec<Expression>,
        target: Box<Statement>,
    },
    Class {
        name: String,
        extends: Option<String>,
        statements: Vec<Statement>,
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
    ClassName {
        class_name: String,
        extends: Option<String>,
    },
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
        ty: VariableType,
        value: Option<Expression>,
    },
    Enum {
        name: Option<String>,
        values: Vec<(String, Option<i64>)>,
    },
    Pass,
}

#[derive(Debug)]
pub enum VariableType {
    Dynamic,
    Inferred,
    Static(String),
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
    Super,
    Parenthesis(Box<Expression>),
    Dictionary {
        kv_pairs: Vec<(String, Expression)>,
    },
    AttributeAccess {
        expr: Box<Expression>,
        attribute: String,
    },
    Function {
        name: String,
        args: Vec<String>,
        statements: Vec<Statement>,
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
    And,
    Or,
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
            TokenType::And | TokenType::DoubleAmpersand => Ok(BinaryOperator::And),
            TokenType::Or | TokenType::DoublePipe => Ok(BinaryOperator::Or),
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
    ExpectedEnd {
        actual: Token,
    },
    MultilineAnnotationNotAllowed {
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
            ParseError::ExpectedEnd { actual } => {
                write!(
                    f,
                    "{}:{}: expected end of line or semicolon, found '{}'",
                    actual.line,
                    actual.column,
                    actual.ty.generic_name()
                )
            }
            ParseError::MultilineAnnotationNotAllowed { line, column } => {
                write!(
                    f,
                    "{}:{}: multiline annotation not allowed here",
                    line, column
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
    indent_aware_stack: Vec<bool>,
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
            indent_aware_stack: Vec::new(),
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

        Some(self.annotation(true))
    }

    fn annotation(&mut self, allow_multiline: bool) -> Result<Statement, ParseError> {
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
            TokenType::Class => {
                self.consume_token();
                return self.parse_class(first_tok);
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
            TokenType::Match => {
                self.consume_token();
                return self.parse_match(first_tok);
            }
            TokenType::Return => {
                self.consume_token();
                let expr = self.expression()?;
                return Ok(Statement {
                    ty: StatementType::Return(expr),
                    line: first_tok.line,
                    column: first_tok.column,
                });
            }
            TokenType::Pass => {
                self.consume_token();
                return Ok(Statement {
                    ty: StatementType::Pass,
                    line: first_tok.line,
                    column: first_tok.column,
                });
            }
            _ => return self.reassignment_statement(),
        };

        let name = self.expect_identifier()?;
        let mut args = Vec::new();

        if self.consume_if(TokenType::LParen) {
            loop {
                if self.consume_if(TokenType::RParen) {
                    break;
                }

                args.push(self.expression()?);
                if !self.consume_if(TokenType::Comma) {
                    self.expect(TokenType::RParen)?;
                    break;
                }
            }
        }

        let eol = self.peek_tok();
        if let Some(Token {
            ty: TokenType::Eol,
            line,
            column,
        }) = eol
        {
            if !allow_multiline {
                return Err(ParseError::MultilineAnnotationNotAllowed {
                    line: *line,
                    column: *column,
                });
            }

            self.consume_token();

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

            self.consume_token();
        }

        let target = self.annotation(allow_multiline)?;

        Ok(Statement {
            ty: StatementType::Annotation {
                name,
                args,
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
        let class_name = self.expect_identifier()?;

        let extends = if self.consume_if(TokenType::Extends) {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        Ok(Statement {
            ty: StatementType::ClassName {
                class_name,
                extends,
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_class(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        let extends = if self.consume_if(TokenType::Extends) {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        self.expect(TokenType::Colon)?;
        let statements = if self.consume_if(TokenType::Eol) {
            self.parse_block_scope()?
        } else {
            self.parse_single_line_scope()?
        };

        Ok(Statement {
            ty: StatementType::Class {
                name,
                extends,
                statements,
            },
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

        let (ty, value) = if self.consume_if(TokenType::Colon) {
            let ty = self.expect_identifier()?;

            let value = if self.consume_if(TokenType::Equal) {
                Some(self.expression()?)
            } else {
                None
            };

            (VariableType::Static(ty), value)
        } else if self.consume_if(TokenType::ColonEqual) {
            let value = self.expression()?;
            (VariableType::Inferred, Some(value))
        } else {
            let value = if self.consume_if(TokenType::Equal) {
                Some(self.expression()?)
            } else {
                None
            };
            (VariableType::Dynamic, value)
        };

        Ok(Statement {
            ty: StatementType::Var {
                konst,
                identifier,
                ty,
                value,
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
                match self.expect_token()? {
                    Token {
                        ty: TokenType::Integer(value),
                        ..
                    } => Some(*value),
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

            statements.push(self.annotation(true)?);

            while self.consume_if(TokenType::Semicolon) {
                match self.peek_tok() {
                    Some(Token {
                        ty: TokenType::Eol, ..
                    })
                    | None => {
                        self.consume_token();
                        break;
                    }
                    _ => {}
                }
                statements.push(self.annotation(true)?);
            }
        }

        self.indent_stack.pop();

        Ok(statements)
    }

    fn parse_single_line_scope(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();

        loop {
            statements.push(self.annotation(false)?);

            if !self.consume_if(TokenType::Semicolon) {
                self.expect_end()?;
                break;
            }

            match self.peek_tok() {
                Some(Token {
                    ty: TokenType::Eol, ..
                })
                | None => {
                    self.consume_token();
                    break;
                }
                _ => {}
            }
        }

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
        self.logical()
    }

    fn logical(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Or
                | TokenType::DoublePipe
                | TokenType::And
                | TokenType::DoubleAmpersand => {
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
        let mut expression = self.add_sub()?;

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
                            rhs: Box::new(self.add_sub()?),
                            operator: op,
                        },
                    }
                }
                Err(_) => break,
            }
        }

        Ok(expression)
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
        let mut expr = self.negate()?;

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
                            rhs: Box::new(self.negate()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
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

        let target = self.attribute_access()?;

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
                TokenType::Dot => {
                    self.consume_token();
                    let attribute = self.expect_identifier()?;

                    expr = Expression {
                        ty: ExpressionType::AttributeAccess {
                            expr: Box::new(expr),
                            attribute,
                        },
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
                TokenType::Super => ExpressionType::Super,
                TokenType::Func => self.parse_func()?,
                TokenType::LParen => {
                    self.indent_aware_stack.push(false);
                    let expr = self.expression()?;
                    self.expect(TokenType::RParen)?;
                    self.indent_aware_stack.pop();
                    ExpressionType::Parenthesis(Box::new(expr))
                }
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

    fn parse_func(&mut self) -> Result<ExpressionType, ParseError> {
        let name = self.expect_identifier()?;

        self.expect(TokenType::LParen)?;

        let mut args = Vec::new();

        loop {
            if self.consume_if(TokenType::RParen) {
                break;
            }

            args.push(self.expect_identifier()?);

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RParen)?;
                break;
            }
        }

        self.expect(TokenType::Colon)?;

        let statements = if self.consume_if(TokenType::Eol) {
            self.parse_block_scope()?
        } else {
            self.parse_single_line_scope()?
        };

        Ok(ExpressionType::Function {
            name,
            args,
            statements,
        })
    }

    fn parse_array_literal(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut expressions = Vec::new();

        loop {
            if self.consume_if(TokenType::RBracket) {
                break;
            }

            expressions.push(self.expression()?);

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RBracket)?;
                break;
            }
        }

        Ok(expressions)
    }

    fn parse_dict_literal(&mut self) -> Result<Vec<(String, Expression)>, ParseError> {
        let mut kv = Vec::new();
        self.indent_aware_stack.push(false);

        loop {
            if self.consume_if(TokenType::RBrace) {
                break;
            }

            let key_token = self.expect_token()?.clone();
            match key_token.ty {
                TokenType::String(_) | TokenType::Integer(_) | TokenType::Identifier(_) => {}
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec![
                            TokenType::String("".to_owned()),
                            TokenType::Integer(0),
                            TokenType::Identifier("".to_owned()),
                        ],
                        actual: key_token,
                    })
                }
            };

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
                    TokenType::String(key) => key,
                    TokenType::Identifier(key) => key,
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![
                                TokenType::String("".to_owned()),
                                TokenType::Identifier("".to_owned()),
                            ],
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

            self.indent_aware_stack.push(true);
            let value = self.expression()?;
            self.indent_aware_stack.pop();
            kv.push((key, value));

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RBrace)?;
                break;
            }
        }

        self.indent_aware_stack.pop();
        Ok(kv)
    }

    fn parse_function_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();

        loop {
            if self.consume_if(TokenType::RParen) {
                break;
            }

            args.push(self.expression()?);

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RParen)?;
                break;
            }
        }

        Ok(args)
    }

    fn peek_tok(&mut self) -> Option<&Token> {
        self.peek_n(1)
    }

    fn peek_n(&mut self, n: usize) -> Option<&Token> {
        let indent_aware = self.indent_aware_stack.last().copied().unwrap_or(true);
        if !indent_aware {
            loop {
                match self.tokens.get(self.read_index) {
                    Some(Token {
                        ty: TokenType::Eol, ..
                    })
                    | Some(Token {
                        ty: TokenType::Indent { .. },
                        ..
                    }) => {
                        self.read_index += 1;
                    }
                    _ => break,
                }
            }
        }

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

    fn expect_end(&mut self) -> Result<(), ParseError> {
        match self.consume_token() {
            None => Ok(()),
            Some(Token {
                ty: TokenType::Eol, ..
            }) => Ok(()),
            Some(other) => Err(ParseError::UnexpectedToken {
                expected: vec![TokenType::Eol],
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

    fn consume_whitespace(&mut self) {
        if let Some(indent_info) = self.consume_until_nonempty_line() {
            if indent_info.level > 0 {
                self.consume_token();
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
