use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

pub enum FunctionArity {
    Exact(usize),
    AtLeast(usize),
    Between(usize, usize),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum LambdaArg {
    Required(String),
    Optional(String),
    Rest(String),
}

impl LambdaArg {
    pub fn get_name(&self) -> &str {
        match self {
            LambdaArg::Required(name) => name,
            LambdaArg::Optional(name) => name,
            LambdaArg::Rest(name) => name,
        }
    }

    pub fn is_required(&self) -> bool {
        matches!(self, LambdaArg::Required(_))
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, LambdaArg::Optional(_))
    }

    pub fn is_rest(&self) -> bool {
        matches!(self, LambdaArg::Rest(_))
    }

    pub fn as_required(&self) -> Result<&str> {
        match self {
            LambdaArg::Required(name) => Ok(name),
            _ => Err(anyhow!(
                "expected a required argument, but got a {} one",
                self.get_name()
            )),
        }
    }

    pub fn as_optional(&self) -> Result<&str> {
        match self {
            LambdaArg::Optional(name) => Ok(name),
            _ => Err(anyhow!(
                "expected an optional argument, but got a {} one",
                self.get_name()
            )),
        }
    }

    pub fn as_rest(&self) -> Result<&str> {
        match self {
            LambdaArg::Rest(name) => Ok(name),
            _ => Err(anyhow!(
                "expected a rest argument, but got a {} one",
                self.get_name()
            )),
        }
    }
}

impl Display for LambdaArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaArg::Required(name) => write!(f, "{}", name),
            LambdaArg::Optional(name) => write!(f, "{}?", name),
            LambdaArg::Rest(name) => write!(f, "...{}", name),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct LambdaDef {
    pub name: Option<String>,
    pub args: Vec<LambdaArg>,
    pub body: String,
    pub scope: HashMap<String, Value>,
}

impl LambdaDef {
    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub fn get_arity(&self) -> FunctionArity {
        let has_rest = self.args.iter().any(|arg| arg.is_rest());
        let min = self.args.iter().filter(|arg| arg.is_required()).count();
        let max = self.args.len();

        if has_rest {
            FunctionArity::AtLeast(min)
        } else if min == max {
            FunctionArity::Exact(min)
        } else {
            FunctionArity::Between(min, max)
        }
    }
}

impl PartialOrd for LambdaDef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.args > other.args {
            Some(std::cmp::Ordering::Greater)
        } else if self.args < other.args {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum IterableValue {
    List(Vec<Value>),
    String(String),
    Record(BTreeMap<String, Value>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum IterableValueType {
    List,
    String,
    Record,
}

impl IterableValue {
    pub fn get_type(&self) -> IterableValueType {
        match self {
            IterableValue::List(_) => IterableValueType::List,
            IterableValue::String(_) => IterableValueType::String,
            IterableValue::Record(_) => IterableValueType::Record,
        }
    }
}

impl IntoIterator for IterableValue {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            IterableValue::List(l) => l.into_iter(), // Yields an iterator over the values in the list.
            IterableValue::String(s) => s
                .chars()
                .map(|c| Value::String(c.to_string()))
                .collect::<Vec<Value>>()
                .into_iter(), // Yields an iterator over the characters in the string.
            IterableValue::Record(r) => r
                .into_iter()
                .map(|(k, v)| Value::List(vec![Value::String(k), v]))
                .collect::<Vec<Value>>()
                .into_iter(), // Yields an iterator over the [key, value] pairs of the record.
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum ValueType {
    Number,
    List,
    Spread,
    Each,
    Bool,
    Lambda,
    BuiltIn,
    String,
    Record,
    Null,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Number => write!(f, "number"),
            ValueType::List => write!(f, "list"),
            ValueType::Spread => write!(f, "spread"),
            ValueType::Each => write!(f, "each"),
            ValueType::Bool => write!(f, "boolean"),
            ValueType::Lambda => write!(f, "lambda"),
            ValueType::BuiltIn => write!(f, "built-in function"),
            ValueType::String => write!(f, "string"),
            ValueType::Record => write!(f, "record"),
            ValueType::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum Value {
    /// A number is a floating-point value.
    Number(f64),
    /// A list is a sequence of values.
    List(Vec<Value>),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(IterableValue),
    /// Like a spread, but the value is never unwrapped. This is used for the "each" keyword. (internal only)
    Each(IterableValue),
    /// A boolean value is either true or false.
    Bool(bool),
    /// A lambda is a function definition.
    Lambda(LambdaDef),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(String),
    /// A string is a sequence of characters.
    String(String),
    /// A record is a collection of key-value pairs.
    Record(BTreeMap<String, Value>),
    /// A null value represents the absence of a value.
    Null,
}

impl IntoIterator for Value {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Value::Spread(iterable) => iterable.into_iter(), // Yields an iterator over the values in the spread.
            _ => vec![self].into_iter(), // Yields a single value wrapped in a Vec. Note that this will include "Each" values.
        }
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::List(_) => ValueType::List,
            Value::Spread(_) => ValueType::Spread,
            Value::Each(_) => ValueType::Each,
            Value::Bool(_) => ValueType::Bool,
            Value::Lambda(_) => ValueType::Lambda,
            Value::String(_) => ValueType::String,
            Value::Null => ValueType::Null,
            Value::BuiltIn(_) => ValueType::BuiltIn,
            Value::Record(_) => ValueType::Record,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    pub fn is_spread(&self) -> bool {
        matches!(self, Value::Spread(_))
    }

    pub fn is_each(&self) -> bool {
        matches!(self, Value::Spread(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self, Value::Lambda(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_built_in(&self) -> bool {
        matches!(self, Value::BuiltIn(_))
    }

    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a {}", self.get_type())),
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_spread(&self) -> Result<&IterableValue> {
        match self {
            Value::Spread(v) => Ok(v),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn as_each(&self) -> Result<&IterableValue> {
        match self {
            Value::Each(v) => Ok(v),
            _ => Err(anyhow!("expected an each, but got a {}", self.get_type())),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(anyhow!("expected a boolean, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda(&self) -> Result<&LambdaDef> {
        match self {
            Value::Lambda(def) => Ok(def),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_string(&self) -> Result<&str> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(anyhow!("expected a string, but got a {}", self.get_type())),
        }
    }

    pub fn as_null(&self) -> Result<()> {
        match self {
            Value::Null => Ok(()),
            _ => Err(anyhow!("expected a null, but got a {}", self.get_type())),
        }
    }

    pub fn as_built_in(&self) -> Result<&str> {
        match self {
            Value::BuiltIn(s) => Ok(s),
            _ => Err(anyhow!(
                "expected a built-in function, but got a {}",
                self.get_type()
            )),
        }
    }

    /// Stringify the value. Returns the same thing as the Display trait impl, except for
    /// Value::String, which is returned without wrapping quotes. Use this for string
    /// concatenation, formatting, etc.
    pub fn stringify(&self) -> String {
        match self {
            Value::String(s) => format!("{}", s),
            _ => format!("{}", self),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, value) in l.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Spread(v) => match v {
                IterableValue::List(l) => {
                    write!(f, "...[")?;
                    for (i, value) in l.iter().enumerate() {
                        write!(f, "{}", value)?;
                        if i < l.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
                IterableValue::String(s) => write!(f, "...\"{}\"", s),
                IterableValue::Record(r) => {
                    write!(f, "...{{")?;
                    for (i, (key, value)) in r.iter().enumerate() {
                        write!(f, "{}: {}", key, value)?;
                        if i < r.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "}}")
                }
            },
            Value::Each(v) => match v {
                IterableValue::List(l) => {
                    write!(f, "each [")?;
                    for (i, value) in l.iter().enumerate() {
                        write!(f, "{}", value)?;
                        if i < l.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "]")
                }
                IterableValue::String(s) => write!(f, "each \"{}\"", s),
                IterableValue::Record(r) => {
                    write!(f, "each {{")?;
                    for (i, (key, value)) in r.iter().enumerate() {
                        write!(f, "{}: {}", key, value)?;
                        if i < r.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "}}")
                }
            },
            Value::Bool(b) => write!(f, "{}", b),
            Value::Lambda(def) => {
                write!(f, "(")?;
                for (i, arg) in def.args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < def.args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") => {}", def.body)
            }
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Null => write!(f, "null"),
            Value::BuiltIn(s) => write!(f, "{} (built-in)", s),
            Value::Record(r) => {
                write!(f, "{{")?;
                for (i, (key, value)) in r.iter().enumerate() {
                    write!(f, "{}: {}", key, value)?;
                    if i < r.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}
