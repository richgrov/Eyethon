use crate::parse::{Expression, ExpressionType, FunctionCallExpression, Statement, StatementType};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Value(pub Vec<f64>);

type NativeFn = Box<dyn Fn(&[Value]) -> Value>;

pub struct VM {
    variables: HashMap<String, Value>,
    native_fns: HashMap<String, NativeFn>,
}

#[derive(Debug)]
pub enum VMError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeError(&'static str),
    ArgumentError { expected: usize, got: usize },
}

impl VM {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            native_fns: HashMap::new(),
        }
    }

    pub fn register_native(&mut self, name: &str, f: impl Fn(&[Value]) -> Value + 'static) {
        self.native_fns.insert(name.to_string(), Box::new(f));
    }

    pub fn run(&mut self, statements: &[Statement]) -> Result<(), VMError> {
        for stmt in statements {
            self.execute_statement(stmt)?;
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &Statement) -> Result<(), VMError> {
        match &stmt.ty {
            StatementType::Var {
                konst: _,
                identifier,
                value,
            } => {
                let val = self.evaluate_expression(value)?;
                self.variables.insert(identifier.clone(), val);
            }
            StatementType::Function {
                name,
                args: _,
                statements: _,
            } => {
                if !self.native_fns.contains_key(name) {
                    return Err(VMError::UndefinedFunction(name.clone()));
                }
            }
            _ => {} // Ignore other statement types for now
        }
        Ok(())
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, VMError> {
        match &expr.ty {
            ExpressionType::Identifier(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| VMError::UndefinedVariable(name.clone())),
            ExpressionType::Integer(i) => Ok(Value(vec![*i as f64])),
            ExpressionType::Float(f) => Ok(Value(vec![*f])),
            ExpressionType::Array(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    let Value(mut vals) = self.evaluate_expression(elem)?;
                    values.append(&mut vals);
                }
                Ok(Value(values))
            }
            ExpressionType::FunctionCall(FunctionCallExpression { callee, args }) => {
                match &callee.ty {
                    ExpressionType::Identifier(name) => {
                        let mut evaluated_args = Vec::new();
                        for arg in args {
                            evaluated_args.push(self.evaluate_expression(arg)?);
                        }

                        let f = self
                            .native_fns
                            .get(name)
                            .ok_or_else(|| VMError::UndefinedFunction(name.clone()))?;

                        Ok(f(&evaluated_args))
                    }
                    _ => Err(VMError::TypeError("Function callee must be an identifier")),
                }
            }
            _ => Err(VMError::TypeError("Unsupported expression type")),
        }
    }
}

// Example usage:
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{Expression, ExpressionType, Statement, StatementType};

    #[test]
    fn test_native_function() {
        let mut vm = VM::new();

        vm.register_native("add", |args: &[Value]| {
            if args.len() != 2 {
                panic!("add requires 2 arguments");
            }
            let a = &args[0].0;
            let b = &args[1].0;
            let len = a.len().min(b.len());

            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                result.push(a[i] + b[i]);
            }
            Value(result)
        });

        let statements = vec![Statement {
            ty: StatementType::Var {
                konst: false,
                identifier: "x".to_string(),
                value: Expression {
                    ty: ExpressionType::Array(vec![
                        Expression {
                            ty: ExpressionType::Integer(1),
                            line: 1,
                            column: 1,
                        },
                        Expression {
                            ty: ExpressionType::Integer(2),
                            line: 1,
                            column: 2,
                        },
                    ]),
                    line: 1,
                    column: 1,
                },
            },
            line: 1,
            column: 1,
        }];

        vm.run(&statements).unwrap();

        assert_eq!(vm.variables["x"].0, vec![1.0, 2.0]);
    }
}
