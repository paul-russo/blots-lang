use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    fmt::Display,
    rc::Rc,
};

use crate::{
    functions::{get_built_in_function_id, get_built_in_function_ident},
    heap::{
        Heap, HeapPointer, HeapValue, IterablePointer, LambdaPointer, ListPointer, RecordPointer,
        StringPointer,
    },
};

#[derive(Debug)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDef {
    pub name: Option<String>,
    pub args: Vec<LambdaArg>,
    pub body: String,
    pub scope: HashMap<String, Value>,
}

impl LambdaDef {
    pub fn set_name(&mut self, name: String, _value: Value) {
        self.name = Some(name.clone());
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
pub struct SerializableLambdaDef {
    pub name: Option<String>,
    pub args: Vec<LambdaArg>,
    pub body: String,
    pub scope: Option<BTreeMap<String, SerializableValue>>,
}

pub struct WithHeap<'h, T> {
    pub value: &'h T,
    pub heap: Rc<RefCell<Heap>>,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ReifiedIterableValue<'h> {
    List(&'h Vec<Value>),
    String(&'h String),
    Record(&'h BTreeMap<String, Value>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ReifiedIterableValueType {
    List,
    String,
    Record,
}

impl<'h> ReifiedIterableValue<'h> {
    pub fn with_heap(&'h self, heap: Rc<RefCell<Heap>>) -> WithHeap<'h, ReifiedIterableValue<'h>> {
        WithHeap { value: self, heap }
    }

    pub fn get_type(&self) -> ReifiedIterableValueType {
        match self {
            ReifiedIterableValue::List(_) => ReifiedIterableValueType::List,
            ReifiedIterableValue::String(_) => ReifiedIterableValueType::String,
            ReifiedIterableValue::Record(_) => ReifiedIterableValueType::Record,
        }
    }
}

impl<'h> IntoIterator for WithHeap<'h, ReifiedIterableValue<'h>> {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self.value {
            // Yields an iterator over the values in the list.
            ReifiedIterableValue::List(l) => (*l).clone().into_iter(),
            // Yields an iterator over the characters in the string.
            ReifiedIterableValue::String(s) => s
                .chars()
                .map(|c| self.heap.borrow_mut().insert_string(c.to_string()))
                .collect::<Vec<Value>>()
                .into_iter(),
            // Yields an iterator over the [key, value] pairs of the record.
            ReifiedIterableValue::Record(r) => r
                .into_iter()
                .map(|(k, v)| {
                    let list = vec![self.heap.borrow_mut().insert_string(k.to_string()), *v];
                    self.heap.borrow_mut().insert_list(list)
                })
                .collect::<Vec<Value>>()
                .into_iter(),
        }
    }
}

impl<'h> IntoIterator for WithHeap<'h, ReifiedValue<'h>> {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self.value {
            ReifiedValue::Spread(iterable, _) => iterable.with_heap(self.heap).into_iter(),
            _ => vec![(*self.value).into()].into_iter(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ValueType {
    Number,
    List,
    Spread,
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
            ValueType::Bool => write!(f, "boolean"),
            ValueType::Lambda => write!(f, "lambda"),
            ValueType::BuiltIn => write!(f, "built-in function"),
            ValueType::String => write!(f, "string"),
            ValueType::Record => write!(f, "record"),
            ValueType::Null => write!(f, "null"),
        }
    }
}

/// A value, after it has been "reified" (borrowed) from a pointer.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ReifiedValue<'h> {
    /// A number is a floating-point value.
    Number(f64),
    /// A boolean value is either true or false.
    Bool(bool),
    /// A null value represents the absence of a value.
    Null,
    /// A list is a sequence of values.
    List(&'h Vec<Value>, ListPointer),
    /// A string is a sequence of characters.
    String(&'h str, StringPointer),
    /// A record is a collection of key-value pairs.
    Record(&'h BTreeMap<String, Value>, RecordPointer),
    /// A lambda is a function definition.
    Lambda(&'h LambdaDef, LambdaPointer),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(ReifiedIterableValue<'h>, IterablePointer),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(usize),
}

impl<'h> ReifiedValue<'h> {
    pub fn with_heap(&'h self, heap: Rc<RefCell<Heap>>) -> WithHeap<'h, ReifiedValue<'h>> {
        WithHeap { value: self, heap }
    }
}

impl From<ReifiedValue<'_>> for Value {
    fn from(value: ReifiedValue) -> Self {
        match value {
            ReifiedValue::Number(n) => Value::Number(n),
            ReifiedValue::Bool(b) => Value::Bool(b),
            ReifiedValue::Null => Value::Null,
            ReifiedValue::List(_, p) => Value::List(p),
            ReifiedValue::String(_, p) => Value::String(p),
            ReifiedValue::Record(_, p) => Value::Record(p),
            ReifiedValue::Lambda(_, p) => Value::Lambda(p),
            ReifiedValue::Spread(_, p) => Value::Spread(p),
            ReifiedValue::BuiltIn(id) => Value::BuiltIn(id),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum SerializableIterableValue {
    List(Vec<SerializableValue>),
    String(String),
    Record(BTreeMap<String, SerializableValue>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum SerializableValue {
    Number(f64),
    Bool(bool),
    Null,
    List(Vec<SerializableValue>),
    String(String),
    Record(BTreeMap<String, SerializableValue>),
    Lambda(SerializableLambdaDef),
    BuiltIn(String),
}

impl SerializableValue {
    pub fn from_value<'h>(value: &Value, heap: &'h Heap) -> Result<SerializableValue> {
        match value {
            Value::Number(n) => Ok(SerializableValue::Number(*n)),
            Value::Bool(b) => Ok(SerializableValue::Bool(*b)),
            Value::Null => Ok(SerializableValue::Null),
            Value::List(p) => {
                let list = p.reify(heap).as_list()?;
                let serialized_list = list
                    .iter()
                    .map(|v| SerializableValue::from_value(v, heap))
                    .collect::<Result<Vec<SerializableValue>>>()?;
                Ok(SerializableValue::List(serialized_list))
            }
            Value::String(p) => {
                let string = p.reify(heap).as_string()?;
                Ok(SerializableValue::String(string.to_string()))
            }
            Value::Record(p) => {
                let record = p.reify(heap).as_record()?;
                let serialized_record = record
                    .iter()
                    .map(|(k, v)| Ok((k.to_string(), SerializableValue::from_value(v, heap)?)))
                    .collect::<Result<BTreeMap<String, SerializableValue>>>()?;
                Ok(SerializableValue::Record(serialized_record))
            }
            Value::Lambda(p) => {
                let lambda = p.reify(heap).as_lambda()?;
                Ok(SerializableValue::Lambda(SerializableLambdaDef {
                    name: lambda.name.clone(),
                    args: lambda.args.clone(),
                    body: lambda.body.clone(),
                    scope: Some(
                        lambda
                            .scope
                            .clone()
                            .into_iter()
                            .map(|(k, v)| SerializableValue::from_value(&v, heap).map(|sv| (k, sv)))
                            .collect::<Result<BTreeMap<String, SerializableValue>>>()?,
                    ),
                }))
            }
            Value::BuiltIn(id) => Ok(SerializableValue::BuiltIn(
                get_built_in_function_ident(*id).unwrap().to_string(),
            )),
            Value::Spread(_) => Err(anyhow!("cannot serialize a spread value")),
        }
    }

    pub fn to_value<'h>(&self, heap: &'h mut Heap) -> Result<Value> {
        match self {
            SerializableValue::Number(n) => Ok(Value::Number(*n)),
            SerializableValue::Bool(b) => Ok(Value::Bool(*b)),
            SerializableValue::Null => Ok(Value::Null),
            SerializableValue::List(list) => {
                let deserialized_list = list
                    .iter()
                    .map(|v| SerializableValue::to_value(v, heap))
                    .collect::<Result<Vec<Value>>>()?;

                Ok(heap.insert_list(deserialized_list))
            }
            SerializableValue::String(s) => Ok(heap.insert_string(s.to_string())),
            SerializableValue::Record(record) => {
                let deserialized_record = record
                    .iter()
                    .map(|(k, v)| Ok((k.to_string(), SerializableValue::to_value(v, heap)?)))
                    .collect::<Result<BTreeMap<String, Value>>>()?;
                Ok(heap.insert_record(deserialized_record))
            }
            SerializableValue::Lambda(s_lambda) => {
                let scope = if let Some(scope) = s_lambda.scope.clone() {
                    scope
                        .iter()
                        .map(|(k, v)| Ok((k.to_string(), SerializableValue::to_value(v, heap)?)))
                        .collect::<Result<HashMap<String, Value>>>()?
                } else {
                    HashMap::new()
                };

                let lambda = LambdaDef {
                    name: s_lambda.name.clone(),
                    args: s_lambda.args.clone(),
                    body: s_lambda.body.clone(),
                    scope,
                };

                Ok(heap.insert_lambda(lambda))
            }
            SerializableValue::BuiltIn(ident) => get_built_in_function_id(ident)
                .ok_or(anyhow!("built-in function with ident {} not found", ident))
                .map(|id| Value::BuiltIn(id)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum PrimitiveValue {
    Number(f64),
    Bool(bool),
    Null,
}

impl Into<Value> for PrimitiveValue {
    fn into(self) -> Value {
        match self {
            PrimitiveValue::Number(n) => Value::Number(n),
            PrimitiveValue::Bool(b) => Value::Bool(b),
            PrimitiveValue::Null => Value::Null,
        }
    }
}

impl Into<SerializableValue> for PrimitiveValue {
    fn into(self) -> SerializableValue {
        match self {
            PrimitiveValue::Number(n) => SerializableValue::Number(n),
            PrimitiveValue::Bool(b) => SerializableValue::Bool(b),
            PrimitiveValue::Null => SerializableValue::Null,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Value {
    /// A number is a floating-point value.
    Number(f64),
    /// A boolean value is either true or false.
    Bool(bool),
    /// A null value represents the absence of a value.
    Null,
    /// A list is a sequence of values.
    List(ListPointer),
    /// A string is a sequence of characters.
    String(StringPointer),
    /// A record is a collection of key-value pairs.
    Record(RecordPointer),
    /// A lambda is a function definition.
    Lambda(LambdaPointer),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(IterablePointer),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(usize),
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::List(_) => ValueType::List,
            Value::Spread(_) => ValueType::Spread,
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

    pub fn is_callable(&self) -> bool {
        matches!(self, Value::BuiltIn(_) | Value::Lambda(_))
    }

    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a {}", self.get_type())),
        }
    }

    pub fn as_list<'h>(&self, heap: &'h Heap) -> Result<&'h Vec<Value>> {
        match self {
            Value::List(l) => l.reify(heap).as_list(),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_spread<'h>(&self, heap: &'h Heap) -> Result<&'h HeapValue> {
        match self {
            Value::Spread(v) => Ok(v.reify(heap)),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(anyhow!("expected a boolean, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda<'h>(&self, heap: &'h Heap) -> Result<&'h LambdaDef> {
        match self {
            Value::Lambda(l) => l.reify(heap).as_lambda(),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_string<'h>(&self, heap: &'h Heap) -> Result<&'h str> {
        match self {
            Value::String(p) => p.reify(heap).as_string(),
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
            Value::BuiltIn(id) => get_built_in_function_ident(*id)
                .ok_or(anyhow!("built-in function with ID {} not found", id)),
            _ => Err(anyhow!(
                "expected a built-in function, but got a {}",
                self.get_type()
            )),
        }
    }

    pub fn as_record<'h>(&self, heap: &'h Heap) -> Result<&'h BTreeMap<String, Value>> {
        match self {
            Value::Record(r) => r.reify(heap).as_record(),
            _ => Err(anyhow!("expected a record, but got a {}", self.get_type())),
        }
    }

    pub fn as_list_pointer(&self) -> Result<ListPointer> {
        match self {
            Value::List(p) => Ok(*p),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_string_pointer(&self) -> Result<StringPointer> {
        match self {
            Value::String(p) => Ok(*p),
            _ => Err(anyhow!("expected a string, but got a {}", self.get_type())),
        }
    }

    pub fn as_record_pointer(&self) -> Result<RecordPointer> {
        match self {
            Value::Record(p) => Ok(*p),
            _ => Err(anyhow!("expected a record, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda_pointer(&self) -> Result<LambdaPointer> {
        match self {
            Value::Lambda(p) => Ok(*p),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_iterable_pointer(&self) -> Result<IterablePointer> {
        match self {
            Value::Spread(p) => Ok(*p),
            _ => Err(anyhow!(
                "expected a spread, but got a {}",
                self.get_type()
            )),
        }
    }

    pub fn to_serializable_value<'h>(&self, heap: &'h Heap) -> Result<SerializableValue> {
        SerializableValue::from_value(self, heap)
    }

    pub fn reify<'h>(&self, heap: &'h Heap) -> Result<ReifiedValue<'h>> {
        match self {
            Value::Number(n) => Ok(ReifiedValue::Number(*n)),
            Value::List(p) => Ok(ReifiedValue::List(p.reify(heap).as_list()?, *p)),
            Value::Spread(p) => Ok(ReifiedValue::Spread(p.reify(heap).as_iterable()?, *p)),
            Value::Bool(b) => Ok(ReifiedValue::Bool(*b)),
            Value::Lambda(p) => Ok(ReifiedValue::Lambda(p.reify(heap).as_lambda()?, *p)),
            Value::String(p) => Ok(ReifiedValue::String(p.reify(heap).as_string()?, *p)),
            Value::Null => Ok(ReifiedValue::Null),
            Value::BuiltIn(id) => Ok(ReifiedValue::BuiltIn(*id)),
            Value::Record(p) => Ok(ReifiedValue::Record(p.reify(heap).as_record()?, *p)),
        }
    }

    /// Stringify the value. Returns the same thing as the Display trait impl, except for
    /// Value::String, which is returned without wrapping quotes. Use this for string
    /// concatenation, formatting, etc.
    pub fn stringify<'h>(&self, heap: &'h Heap) -> String {
        match self {
            Value::String(p) => p.reify(heap).as_string().map(|s| format!("{}", s)).unwrap(),
            Value::List(p) => {
                let mut result = String::from("[");
                let list = p.reify(heap).as_list().unwrap();

                for (i, value) in list.iter().enumerate() {
                    result.push_str(&value.stringify(heap));
                    if i < list.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str("]");
                result
            }
            Value::Record(p) => {
                let mut result = String::from("{");
                let record = p.reify(heap).as_record().unwrap();

                for (i, (key, value)) in record.iter().enumerate() {
                    result.push_str(&format!("{}: {}", key, value.stringify(heap)));
                    if i < record.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str("}");
                result
            }
            Value::Lambda(p) => {
                let lambda = p.reify(heap).as_lambda().unwrap();
                let mut result = String::from("(");
                for (i, arg) in lambda.args.iter().enumerate() {
                    result.push_str(&arg.to_string());
                    if i < lambda.args.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(&format!(") => {}", lambda.body));
                result
            }
            Value::BuiltIn(id) => {
                format!("{} (built-in)", get_built_in_function_ident(*id).unwrap())
            }
            Value::Spread(p) => match p {
                IterablePointer::List(l) => {
                    let list = l.reify(heap).as_list().unwrap();
                    let mut result = String::from("...");
                    result.push_str(&list.iter().map(|v| v.stringify(heap)).collect::<String>());
                    result
                }
                IterablePointer::String(s) => {
                    let string = s.reify(heap).as_string().unwrap();
                    format!("...{}", string)
                }
                IterablePointer::Record(r) => {
                    let record = r.reify(heap).as_record().unwrap();
                    let mut result = String::from("...");
                    result.push_str("{");
                    for (i, (key, value)) in record.iter().enumerate() {
                        result.push_str(&format!("{}: {}", key, value.stringify(heap)));
                        if i < record.len() - 1 {
                            result.push_str(", ");
                        }
                    }
                    result.push_str("}");
                    result
                }
            },
            Value::Bool(_) | Value::Number(_) | Value::Null => format!("{}", self),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::List(p) => write!(f, "{}", p),
            Value::Spread(p) => write!(f, "...{}", p),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Lambda(p) => write!(f, "{}", p),
            Value::String(p) => write!(f, "{}", p),
            Value::Null => write!(f, "null"),
            Value::BuiltIn(id) => write!(
                f,
                "{} (built-in)",
                get_built_in_function_ident(*id).unwrap()
            ),
            Value::Record(p) => write!(f, "{}", p),
        }
    }
}
