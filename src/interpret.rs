use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::compile::{ClassBytecode, Instruction};

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Object(Rc<RefCell<HashMap<Value, Value>>>),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Object(o) => {
                let ptr = Rc::<_>::as_ptr(o);
                ptr.hash(state);
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(i1), Value::Integer(i2)) => i1 == i2,
            (Value::Float(f1), Value::Float(f2)) => f1 == f2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Object(o1), Value::Object(o2)) => Rc::<_>::as_ptr(o1) == Rc::<_>::as_ptr(o2),
            _ => false,
        }
    }
}

impl Eq for Value {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Object(o) => {
                write!(f, "{{")?;
                for (i, (k, v)) in o.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
        }
    }
}

pub struct Interpreter {
    classes: HashMap<String, ClassBytecode>,
    class_objects: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum RuntimeError {
    NotSettable,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeError::NotSettable => write!(f, "not settable"),
        }
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            classes: HashMap::new(),
            class_objects: HashMap::new(),
        }
    }

    pub fn register_class(&mut self, class: ClassBytecode) -> Result<(), RuntimeError> {
        self.class_objects.insert(
            class.name.clone(),
            Value::Object(Rc::new(RefCell::new(HashMap::new()))),
        );
        self.classes.insert(class.name.clone(), class);

        Ok(())
    }

    pub fn new_instance(&mut self, class_name: &str) -> Result<(), RuntimeError> {
        let this = Value::Object(Rc::new(RefCell::new(HashMap::new())));
        self.call_function(class_name, &vec![this.clone()])?;

        println!("{}", this);

        Ok(())
    }

    fn call_function(&mut self, class_name: &str, args: &[Value]) -> Result<(), RuntimeError> {
        let mut pc = 0;
        let mut stack = Vec::new();
        stack.extend_from_slice(args);

        let instructions = &self.classes.get(class_name).unwrap().bytecode;

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
                Instruction::PushString(s) => {
                    stack.push(Value::String(s.clone()));
                }
                Instruction::Store => {
                    let val = stack.pop().unwrap();
                    let key = stack.pop().unwrap();
                    let obj = stack.pop().unwrap();

                    let Value::Object(dict) = obj else {
                        return Err(RuntimeError::NotSettable);
                    };

                    dict.borrow_mut().insert(key, val);
                }
            }

            pc += 1;
        }

        Ok(())
    }
}
