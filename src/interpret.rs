use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

use crate::compile::{ClassBytecode, Instruction};

#[derive(Clone)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Dictionary(Rc<RefCell<HashMap<Value, Value>>>),
    Object {
        variables: Rc<RefCell<Vec<Value>>>,
        variable_names: HashMap<String, usize>,
        class_name: String,
    },
    NativeFunction(Rc<dyn Fn(Vec<Value>) -> Value>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Null => 0.hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Array(o) => {
                o.len().hash(state);
                for value in o {
                    value.hash(state);
                }
            }
            Value::Dictionary(o) => {
                let dict = o.borrow();
                dict.len().hash(state);

                for (key, value) in dict.deref() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Object {
                variables,
                class_name,
                ..
            } => {
                let variables = variables.borrow();
                variables.hash(state);
                class_name.hash(state);
            }
            Value::NativeFunction(func) => {
                let ptr = Rc::<_>::as_ptr(func);
                ptr.hash(state);
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Integer(i1), Value::Integer(i2)) => i1 == i2,
            (Value::Float(f1), Value::Float(f2)) => f1 == f2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Dictionary(o1), Value::Dictionary(o2)) => {
                let dict1 = o1.borrow();
                let dict2 = o2.borrow();
                dict1.deref() == dict2.deref()
            }
            (
                Value::Object {
                    variables: v1,
                    class_name: c1,
                    ..
                },
                Value::Object {
                    variables: v2,
                    class_name: c2,
                    ..
                },
            ) => {
                let vars1 = v1.borrow();
                let vars2 = v2.borrow();
                c1 == c2 && vars1.deref() == vars2.deref()
            }
            (Value::NativeFunction(rc1), Value::NativeFunction(rc2)) => {
                std::ptr::addr_eq(Rc::<_>::as_ptr(rc1), Rc::<_>::as_ptr(rc2))
            }
            _ => false,
        }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Dictionary(o) => {
                write!(f, "{{")?;
                for (i, (k, v)) in o.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Object {
                variables,
                variable_names,
                class_name,
            } => {
                let vars = variables.borrow();

                write!(f, "{} {{", class_name)?;
                for (i, (name, index)) in variable_names.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, vars[*index])?;
                }
                write!(f, "}}")
            }
            Value::NativeFunction(_) => write!(f, "<native function>"),
        }
    }
}

pub struct Interpreter {
    classes: Rc<RefCell<HashMap<String, ClassBytecode>>>,
    class_objects: HashMap<String, Value>,
    globals: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum RuntimeError {
    NotSettable(Value),
    NotCallable,
    NoSuchMethod(String),
    BadStack,
    OutOfInstructions,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::NotSettable(value) => write!(f, "{} is not settable", value),
            RuntimeError::NotCallable => write!(f, "not callable"),
            RuntimeError::NoSuchMethod(name) => write!(f, "no such method: {}", name),
            RuntimeError::BadStack => write!(f, "bad stack"),
            RuntimeError::OutOfInstructions => write!(f, "out of instructions"),
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            classes: Rc::new(RefCell::new(HashMap::new())),
            class_objects: HashMap::new(),
            globals: HashMap::new(),
        }
    }

    pub fn set_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    pub fn register_class(&mut self, class: ClassBytecode) -> Result<(), RuntimeError> {
        self.class_objects.insert(
            class.name.clone(),
            Value::Object {
                variables: Rc::new(RefCell::new(Vec::new())),
                variable_names: HashMap::new(),
                class_name: "".to_owned(),
            },
        );
        self.classes.borrow_mut().insert(class.name.clone(), class);

        Ok(())
    }

    pub fn new_instance(&mut self, class_name: &str) -> Result<Value, RuntimeError> {
        let classes = self.classes.clone();
        let classes = classes.borrow();
        let class = classes.get(class_name).unwrap();

        let num_members = class.member_variables.len();
        let mut variable_names = HashMap::with_capacity(num_members);
        for (i, name) in class.member_variables.iter().enumerate() {
            variable_names.insert(name.to_owned(), i);
        }

        let this = Value::Object {
            variables: Rc::new(RefCell::new(vec![Value::Null; num_members])),
            variable_names,
            class_name: class_name.to_owned(),
        };

        self.call_function(&class.bytecode, &vec![this.clone()])?;

        Ok(this)
    }

    pub fn call_method(
        &mut self,
        this: Value,
        method_name: &str,
    ) -> Result<Option<Value>, RuntimeError> {
        let class_name = match &this {
            Value::Object { class_name, .. } => class_name,
            _ => panic!("not callable"),
        };

        let classes = self.classes.clone();
        let classes = classes.borrow();
        let class = &classes.get(class_name).unwrap();

        let func_addr = class
            .functions
            .get(method_name)
            .ok_or(RuntimeError::NoSuchMethod(method_name.to_owned()))?;

        let instructions = &class.bytecode[*func_addr..];
        let args = vec![this];

        self.call_function(instructions, &args)
    }

    fn call_function(
        &mut self,
        instructions: &[Instruction],
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let mut pc = 0;
        let mut stack = Vec::new();
        stack.extend_from_slice(args);

        while pc < instructions.len() {
            match &instructions[pc] {
                Instruction::Duplicate(index) => {
                    stack.push(stack[*index].clone());
                }
                Instruction::PushInt(i) => {
                    stack.push(Value::Integer(*i));
                }
                Instruction::PushFloat(f) => {
                    stack.push(Value::Float(*f));
                }
                Instruction::MakeArray { len } => {
                    let elements = stack.split_off(stack.len() - len);
                    stack.push(Value::Array(elements));
                }
                Instruction::PushString(s) => {
                    stack.push(Value::String(s.clone()));
                }
                Instruction::PushGlobal(identifier) => {
                    let global = self.globals.get(identifier).unwrap();
                    stack.push(global.clone());
                }
                Instruction::Call { n_args } => {
                    let callee = stack.pop().unwrap();
                    if let Value::NativeFunction(func) = callee {
                        let args = stack.split_off(stack.len() - n_args);
                        let result = func(args);
                        stack.push(result);
                    } else {
                        return Err(RuntimeError::NotCallable);
                    }
                }
                Instruction::Store => {
                    let val = stack.pop().unwrap();
                    let key = stack.pop().unwrap();
                    let obj = stack.pop().unwrap();

                    match obj {
                        Value::Dictionary(dict) => {
                            dict.borrow_mut().insert(key, val);
                        }
                        Value::Object {
                            variables,
                            variable_names,
                            ..
                        } => {
                            let Value::String(attribute) = key else {
                                panic!();
                            };

                            let index = variable_names.get(&attribute).unwrap();
                            variables.borrow_mut()[*index] = val;
                        }
                        other => {
                            return Err(RuntimeError::NotSettable(other));
                        }
                    }
                }
                Instruction::Return => {
                    if stack.len() == args.len() {
                        return Ok(None);
                    } else if stack.len() == args.len() + 1 {
                        return Ok(stack.pop());
                    } else {
                        return Err(RuntimeError::BadStack);
                    }
                }
            }

            pc += 1;
        }

        return Err(RuntimeError::OutOfInstructions);
    }
}
