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
    IntegerDivideAssign,
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
            TokenType::SlashSlashEq => Ok(ReassignmentOperator::IntegerDivideAssign),
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
    Assert {
        condition: Expression,
    },
    Annotation {
        name: String,
        args: Vec<Expression>,
        target: Box<Statement>,
    },
    Class {
        name: String,
        extends: Vec<Expression>,
        statements: Vec<Statement>,
    },
    Signal {
        name: String,
        parameters: Vec<String>,
    },
    Return(Option<Expression>),
    Match {
        expression: Expression,
        arms: Vec<(Vec<Pattern>, Vec<Statement>)>,
    },
    Reassignment {
        key: Expression,
        operator: ReassignmentOperator,
        value: Expression,
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
        var_type: Option<Type>,
        iterator: Expression,
        statements: Vec<Statement>,
    },
    Expression(Expression),
    Var {
        konst: bool,
        static_: bool,
        identifier: String,
        ty: VariableType,
        value: Option<Expression>,
        getter: Option<Vec<Statement>>,
        setter: Option<(String, Vec<Statement>)>,
    },
    Import {
        from: Option<String>,
        imports: Vec<(String, Option<String>)>,
    },
    Pass,
}

#[derive(Debug)]
pub struct Type {
    pub ty: String,
    pub generic_args: Vec<String>,
}

#[derive(Debug)]
pub enum VariableType {
    Dynamic,
    Inferred,
    Static(Type),
}

#[derive(Debug)]
pub struct Expression {
    pub ty: ExpressionType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub static_: bool,
    pub args: Vec<FunctionParameter>,
    pub return_type: FunctionReturnType,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub name: String,
    pub ty: Option<Type>,
    pub default: Option<Expression>,
}

#[derive(Debug)]
pub enum FunctionReturnType {
    Dynamic,
    Void,
    Static(Type),
}

#[derive(Debug)]
pub enum ExpressionType {
    Identifier(String),
    String(String),
    StringName(String),
    Null,
    Integer(i64),
    Bool(bool),
    Float(f64),
    Array(Vec<Expression>),
    Ellipsis,
    Super,
    Preload(String),
    Parenthesis(Box<Expression>),
    Dictionary {
        kv_pairs: Vec<(String, Expression)>,
    },
    AttributeAccess {
        expr: Box<Expression>,
        attribute: String,
    },
    Function(Function),
    FunctionCall(FunctionCallExpression),
    Negate(Box<Expression>),
    Not(Box<Expression>),
    BitwiseNot(Box<Expression>),
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        operator: BinaryOperator,
    },
    InstanceOf {
        expr: Box<Expression>,
        invert: bool,
        ty: String,
    },
    Cast {
        expr: Box<Expression>,
        into: String,
    },
    Await(Box<Expression>),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    IntegerDivide,
    Modulo,
    Exponent,
    ShiftLeft,
    ShiftRight,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    In,
    NotIn,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl TryFrom<TokenType> for BinaryOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Plus => Ok(BinaryOperator::Add),
            TokenType::Minus => Ok(BinaryOperator::Subtract),
            TokenType::Star => Ok(BinaryOperator::Multiply),
            TokenType::Slash => Ok(BinaryOperator::Divide),
            TokenType::SlashSlash => Ok(BinaryOperator::IntegerDivide),
            TokenType::Percent => Ok(BinaryOperator::Modulo),
            TokenType::DoubleLChevron => Ok(BinaryOperator::ShiftLeft),
            TokenType::DoubleRChevron => Ok(BinaryOperator::ShiftRight),
            TokenType::LChevron => Ok(BinaryOperator::GreaterThan),
            TokenType::RChevron => Ok(BinaryOperator::LessThan),
            TokenType::LChevronEq => Ok(BinaryOperator::GreaterThanOrEqual),
            TokenType::RChevronEq => Ok(BinaryOperator::LessThanOrEqual),
            TokenType::EqualEqual => Ok(BinaryOperator::Equal),
            TokenType::BangEq => Ok(BinaryOperator::NotEqual),
            TokenType::And | TokenType::DoubleAmpersand => Ok(BinaryOperator::And),
            TokenType::Or | TokenType::DoublePipe => Ok(BinaryOperator::Or),
            TokenType::In => Ok(BinaryOperator::In),
            TokenType::Ampersand => Ok(BinaryOperator::BitwiseAnd),
            TokenType::Pipe => Ok(BinaryOperator::BitwiseOr),
            TokenType::Carot => Ok(BinaryOperator::BitwiseXor),
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
    InvalidProperty {
        expected: Vec<String>,
        actual: String,
        line: usize,
        column: usize,
    },
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
            ParseError::InvalidProperty {
                expected,
                actual,
                line,
                column,
            } => {
                let expected_formatted = expected.join(" or ");

                write!(
                    f,
                    "{}:{}: expected property accessor {} but found '{}'",
                    line, column, expected_formatted, actual
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
            TokenType::Assert => {
                self.consume_token();
                return self.assert_statement(first_tok);
            }
            TokenType::From => {
                self.consume_token();
                let from_module = self.expect_identifier()?.to_string();
                self.expect(TokenType::Import)?;
                let mut imports = Vec::new();

                loop {
                    let identifier = self.expect_identifier()?.to_string();
                    let alias = if self.consume_if(TokenType::As) {
                        Some(self.expect_identifier()?.to_string())
                    } else {
                        None
                    };
                    imports.push((identifier, alias));

                    if !self.consume_if(TokenType::Comma) {
                        break;
                    }
                }

                return Ok(Statement {
                    ty: StatementType::Import {
                        from: Some(from_module),
                        imports,
                    },
                    line: first_tok.line,
                    column: first_tok.column,
                });
            }
            TokenType::Import => {
                self.consume_token();
                let mut imports = Vec::new();

                loop {
                    let identifier = self.expect_identifier()?.to_string();
                    let alias = if self.consume_if(TokenType::As) {
                        Some(self.expect_identifier()?.to_string())
                    } else {
                        None
                    };
                    imports.push((identifier, alias));

                    if !self.consume_if(TokenType::Comma) {
                        break;
                    }
                }

                return Ok(Statement {
                    ty: StatementType::Import {
                        from: None,
                        imports,
                    },
                    line: first_tok.line,
                    column: first_tok.column,
                });
            }
            TokenType::Class => {
                self.consume_token();
                return self.parse_class(first_tok);
            }
            TokenType::Signal => {
                self.consume_token();
                return self.parse_signal(first_tok);
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
            TokenType::Static => {
                self.consume_token();

                let next_token = self.peek_tok().cloned().ok_or(ParseError::UnexpectedEof)?;
                match next_token.ty {
                    TokenType::Var | TokenType::Const => {
                        self.consume_token();
                        return self.var_or_const(next_token, true);
                    }
                    TokenType::Def => {
                        self.consume_token();
                        let expr = self.parse_func(true)?;
                        return Ok(Statement {
                            ty: StatementType::Expression(Expression {
                                ty: expr,
                                line: first_tok.line,
                                column: first_tok.column,
                            }),
                            line: first_tok.line,
                            column: first_tok.column,
                        });
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![TokenType::Var, TokenType::Const, TokenType::Def],
                            actual: next_token,
                        });
                    }
                }
            }
            TokenType::Var | TokenType::Const => {
                self.consume_token();
                return self.var_or_const(first_tok, false);
            }
            TokenType::Match => {
                self.consume_token();
                return self.parse_match(first_tok);
            }
            TokenType::Return => {
                self.consume_token();

                let expr = match self.peek_tok() {
                    Some(Token {
                        ty: TokenType::Eol, ..
                    })
                    | None => None,
                    _ => Some(self.expression()?),
                };

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

        let var_type = if self.consume_if(TokenType::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenType::In)?;
        let iterator = self.expression()?;
        self.expect(TokenType::Colon)?;

        let statements = self.parse_block_scope()?;

        Ok(Statement {
            ty: StatementType::For {
                variable,
                var_type,
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

    fn parse_class(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        let mut extends = Vec::new();
        if self.consume_if(TokenType::LParen) {
            loop {
                if self.consume_if(TokenType::RParen) {
                    break;
                }

                extends.push(self.expression()?);

                if !self.consume_if(TokenType::Comma) {
                    self.expect(TokenType::RParen)?;
                    break;
                }
            }
        }

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

    fn assert_statement(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let condition = self.expression()?;

        Ok(Statement {
            ty: StatementType::Assert { condition },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_signal(&mut self, first_tok: Token) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;
        let mut parameters = Vec::new();

        self.expect(TokenType::LParen)?;

        loop {
            if self.consume_if(TokenType::RParen) {
                break;
            }

            let param_name = self.expect_identifier()?;
            parameters.push(param_name);

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RParen)?;
                break;
            }
        }

        Ok(Statement {
            ty: StatementType::Signal { name, parameters },
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

    fn var_or_const(&mut self, first_tok: Token, static_: bool) -> Result<Statement, ParseError> {
        let konst = first_tok.ty == TokenType::Const;
        let identifier = self.expect_identifier()?;

        let (ty, value) = if self.consume_if(TokenType::Colon) {
            if self.consume_if(TokenType::Equal) {
                let value = self.expression()?;
                (VariableType::Inferred, Some(value))
            } else {
                let ty = self.parse_type()?;

                let value = if self.consume_if(TokenType::Equal) {
                    Some(self.expression()?)
                } else {
                    None
                };

                (VariableType::Static(ty), value)
            }
        } else {
            let value = if self.consume_if(TokenType::Equal) {
                Some(self.expression()?)
            } else {
                None
            };
            (VariableType::Dynamic, value)
        };

        let (getter, setter) = if self.consume_if(TokenType::Colon) {
            self.expect(TokenType::Eol)?;
            self.parse_property()?
        } else {
            (None, None)
        };

        Ok(Statement {
            ty: StatementType::Var {
                konst,
                static_,
                identifier,
                ty,
                value,
                getter,
                setter,
            },
            line: first_tok.line,
            column: first_tok.column,
        })
    }

    fn parse_property(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<(String, Vec<Statement>)>), ParseError> {
        let expected_indent = self.indent_stack.last().copied().unwrap_or(0);
        let mut getter = None;
        let mut setter = None;

        let indent_info = self
            .consume_until_nonempty_line()
            .ok_or(ParseError::UnexpectedEof)?;

        if indent_info.level <= expected_indent {
            return Err(ParseError::ExpectedIndent {
                line: indent_info.line,
                column: indent_info.column,
            });
        }

        self.consume_token();

        self.parse_property_accessor(&mut getter, &mut setter)?;

        if let Some(indent_info2) = self.consume_until_nonempty_line() {
            if indent_info2.level == indent_info.level {
                self.consume_token();
                self.parse_property_accessor(&mut getter, &mut setter)?;
            }
        }

        Ok((getter, setter))
    }

    fn parse_property_accessor(
        &mut self,
        getter: &mut Option<Vec<Statement>>,
        setter: &mut Option<(String, Vec<Statement>)>,
    ) -> Result<(), ParseError> {
        let (accessor_type, line, column) = match self.expect_token()? {
            Token {
                ty: TokenType::Identifier(name),
                line,
                column,
            } => (name.to_owned(), *line, *column),
            other => {
                return Err(ParseError::UnexpectedToken {
                    expected: vec![TokenType::Identifier("".to_owned())],
                    actual: other.clone(),
                })
            }
        };

        if accessor_type == "get" {
            if getter.is_some() {
                return Err(ParseError::InvalidProperty {
                    expected: vec!["set".to_owned()],
                    actual: accessor_type,
                    line,
                    column,
                });
            }

            self.expect(TokenType::Colon)?;

            let getter_block = if self.consume_if(TokenType::Eol) {
                self.parse_block_scope()?
            } else {
                self.parse_single_line_scope()?
            };

            let _ = getter.insert(getter_block);
        } else if accessor_type == "set" {
            if setter.is_some() {
                return Err(ParseError::InvalidProperty {
                    expected: vec!["get".to_owned()],
                    actual: accessor_type,
                    line,
                    column,
                });
            }

            self.expect(TokenType::LParen)?;
            let param_name = self.expect_identifier()?;
            self.expect(TokenType::RParen)?;
            self.expect(TokenType::Colon)?;

            let setter_block = if self.consume_if(TokenType::Eol) {
                self.parse_block_scope()?
            } else {
                self.parse_single_line_scope()?
            };

            let _ = setter.insert((param_name, setter_block));
        } else {
            return Err(ParseError::InvalidProperty {
                expected: vec!["get".to_string(), "set".to_string()],
                actual: accessor_type,
                line,
                column,
            });
        }

        Ok(())
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
        self.is_operator()
    }

    fn is_operator(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.await_expr()?;

        if let Some(token) = self.peek_tok() {
            if token.ty == TokenType::Is {
                self.consume_token();

                let invert = self.consume_if(TokenType::Not);
                let rhs = self.await_expr()?;

                expr = Expression {
                    line: expr.line,
                    column: expr.column,
                    ty: ExpressionType::Binary {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                        operator: if invert {
                            BinaryOperator::NotEqual
                        } else {
                            BinaryOperator::Equal
                        },
                    },
                };
            }
        }

        Ok(expr)
    }

    fn await_expr(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.peek_tok() {
            if token.ty == TokenType::Await {
                let token = token.clone();
                self.consume_token();
                let expr = self.logical()?;

                return Ok(Expression {
                    line: token.line,
                    column: token.column,
                    ty: ExpressionType::Await(Box::new(expr)),
                });
            }
        }

        self.logical()
    }

    fn logical(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.not()?;

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
                            rhs: Box::new(self.not()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn not(&mut self) -> Result<Expression, ParseError> {
        if let Some(Token {
            ty: TokenType::Not,
            line,
            column,
        }) = self.peek_tok()
        {
            let line = *line;
            let column = *column;

            self.consume_token();
            let expr = self.in_expr()?;
            return Ok(Expression {
                line,
                column,
                ty: ExpressionType::Not(Box::new(expr)),
            });
        }

        self.in_expr()
    }

    fn in_expr(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        match self.peek_tok() {
            Some(Token {
                ty: TokenType::Not, ..
            }) => {
                if let Some(Token {
                    ty: TokenType::In, ..
                }) = self.peek_n(2)
                {
                    self.consume_token();
                    self.consume_token();

                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.comparison()?),
                            operator: BinaryOperator::NotIn,
                        },
                    };
                }
            }
            Some(Token {
                ty: TokenType::In, ..
            }) => {
                self.consume_token();
                expr = Expression {
                    line: expr.line,
                    column: expr.column,
                    ty: ExpressionType::Binary {
                        lhs: Box::new(expr),
                        rhs: Box::new(self.comparison()?),
                        operator: BinaryOperator::In,
                    },
                };
            }
            _ => {}
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expression = self.bitwise_or()?;

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
                            rhs: Box::new(self.bitwise_or()?),
                            operator: op,
                        },
                    }
                }
                Err(_) => break,
            }
        }

        Ok(expression)
    }

    fn bitwise_or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bitwise_xor()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Pipe => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.bitwise_xor()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn bitwise_xor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bitwise_and()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Carot => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.bitwise_and()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn bitwise_and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bit_shift()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Ampersand => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.bit_shift()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn bit_shift(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.add_sub()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::DoubleLChevron | TokenType::DoubleRChevron => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.add_sub()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
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
        let mut expr = self.prefix()?;

        loop {
            let Some(token) = self.peek_tok() else { break };

            match token.ty {
                TokenType::Star | TokenType::Slash | TokenType::SlashSlash | TokenType::Percent => {
                    let op = BinaryOperator::try_from(token.ty.clone()).unwrap();
                    self.consume_token();
                    expr = Expression {
                        line: expr.line,
                        column: expr.column,
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(self.prefix()?),
                            operator: op,
                        },
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn prefix(&mut self) -> Result<Expression, ParseError> {
        match self.peek_tok() {
            Some(Token {
                ty: TokenType::Minus,
                line,
                column,
            }) => {
                let line = *line;
                let column = *column;
                self.consume_token();

                Ok(Expression {
                    ty: ExpressionType::Negate(Box::new(self.prefix()?)),
                    line,
                    column,
                })
            }
            Some(Token {
                ty: TokenType::Plus,
                ..
            }) => {
                self.consume_token();
                self.prefix()
            }
            Some(Token {
                ty: TokenType::Tilde,
                line,
                column,
            }) => {
                let line = *line;
                let column = *column;
                self.consume_token();

                Ok(Expression {
                    ty: ExpressionType::BitwiseNot(Box::new(self.prefix()?)),
                    line,
                    column,
                })
            }
            _ => self.exponent(),
        }
    }

    fn exponent(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.attribute_access()?;

        loop {
            let Some(token) = self.peek_tok() else { break };
            let token = token.clone();

            match token.ty {
                TokenType::StarStar => {
                    self.consume_token();
                    let right = self.attribute_access()?;

                    expr = Expression {
                        ty: ExpressionType::Binary {
                            lhs: Box::new(expr),
                            rhs: Box::new(right),
                            operator: BinaryOperator::Exponent,
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

    fn attribute_access(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.expr_as()?;

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

    fn expr_as(&mut self) -> Result<Expression, ParseError> {
        let token = self.expect_token()?.clone();
        let mut expr = self.literal(&token)?;

        while self.consume_if(TokenType::As) {
            let ty = self.expect_identifier()?;
            expr = Expression {
                ty: ExpressionType::Cast {
                    expr: Box::new(expr),
                    into: ty,
                },
                line: token.line,
                column: token.column,
            }
        }

        Ok(expr)
    }

    fn literal(&mut self, first_tok: &Token) -> Result<Expression, ParseError> {
        Ok(Expression {
            ty: match first_tok.ty {
                TokenType::Preload => {
                    self.expect(TokenType::LParen)?;

                    let path = match self.expect_token()? {
                        Token {
                            ty: TokenType::String(path),
                            ..
                        } => path.clone(),
                        other => {
                            return Err(ParseError::UnexpectedToken {
                                expected: vec![TokenType::String("".to_owned())],
                                actual: other.clone(),
                            })
                        }
                    };

                    self.expect(TokenType::RParen)?;
                    ExpressionType::Preload(path)
                }
                TokenType::Identifier(ref name) => ExpressionType::Identifier(name.clone()),
                TokenType::Null => ExpressionType::Null,
                TokenType::True => ExpressionType::Bool(true),
                TokenType::False => ExpressionType::Bool(false),
                TokenType::String(ref string) => ExpressionType::String(string.clone()),
                TokenType::StringName(ref string) => ExpressionType::StringName(string.clone()),
                TokenType::Integer(integer) => ExpressionType::Integer(integer),
                TokenType::Float(float) => ExpressionType::Float(float),
                TokenType::Super => ExpressionType::Super,
                TokenType::Ellipsis => ExpressionType::Ellipsis,
                TokenType::Def => self.parse_func(false)?,
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

    fn parse_func(&mut self, static_: bool) -> Result<ExpressionType, ParseError> {
        let name = self.expect_identifier()?;

        self.expect(TokenType::LParen)?;

        let mut args = Vec::new();

        loop {
            if self.consume_if(TokenType::RParen) {
                break;
            }

            let name = self.expect_identifier()?;

            let ty = if self.consume_if(TokenType::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            let default_value = if self.consume_if(TokenType::Equal) {
                Some(self.expression()?)
            } else {
                None
            };

            args.push(FunctionParameter {
                name,
                ty,
                default: default_value,
            });

            if !self.consume_if(TokenType::Comma) {
                self.expect(TokenType::RParen)?;
                break;
            }
        }

        let return_type = if self.consume_if(TokenType::Arrow) {
            if self.consume_if(TokenType::Void) {
                FunctionReturnType::Void
            } else {
                FunctionReturnType::Static(self.parse_type()?)
            }
        } else {
            FunctionReturnType::Dynamic
        };

        self.expect(TokenType::Colon)?;

        let statements = if self.consume_if(TokenType::Eol) {
            self.parse_block_scope()?
        } else {
            self.parse_single_line_scope()?
        };

        Ok(ExpressionType::Function(Function {
            name,
            static_,
            args,
            return_type,
            statements,
        }))
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

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let base = self.expect_identifier()?;

        let generic_args = if self.consume_if(TokenType::LBracket) {
            let mut args = Vec::new();

            loop {
                if self.consume_if(TokenType::RBracket) {
                    break;
                }

                args.push(self.expect_identifier()?);

                if !self.consume_if(TokenType::Comma) {
                    self.expect(TokenType::RBracket)?;
                    break;
                }
            }

            args
        } else {
            Vec::new()
        };

        Ok(Type {
            ty: base,
            generic_args,
        })
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
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut parser = Parser::new(tokens);
    let mut result = Vec::new();

    while let Some(statement) = parser.next() {
        result.push(statement?);
    }

    Ok(result)
}
