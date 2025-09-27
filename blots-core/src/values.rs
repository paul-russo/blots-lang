use anyhow::{Result, anyhow};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    ast::Expr,
    functions::BuiltInFunction,
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
    pub body: Expr,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SerializableLambdaDef {
    pub name: Option<String>,
    pub args: Vec<LambdaArg>,
    pub body: String,
    pub scope: Option<IndexMap<String, SerializableValue>>,
}

pub struct WithHeap<'h, T> {
    pub value: &'h T,
    pub heap: Rc<RefCell<Heap>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReifiedIterableValue<'h> {
    List(&'h Vec<Value>),
    String(&'h String),
    Record(&'h IndexMap<String, Value>),
}

#[derive(Debug, Clone, PartialEq)]
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
            ValueType::Lambda => write!(f, "function"),
            ValueType::BuiltIn => write!(f, "built-in function"),
            ValueType::String => write!(f, "string"),
            ValueType::Record => write!(f, "record"),
            ValueType::Null => write!(f, "null"),
        }
    }
}

/// A value, after it has been "reified" (borrowed) from a pointer.
#[derive(Debug, Copy, Clone, PartialEq)]
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
    Record(&'h IndexMap<String, Value>, RecordPointer),
    /// A lambda is a function definition.
    Lambda(&'h LambdaDef, LambdaPointer),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(ReifiedIterableValue<'h>, IterablePointer),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(BuiltInFunction),
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SerializableIterableValue {
    List(Vec<SerializableValue>),
    String(String),
    Record(IndexMap<String, SerializableValue>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SerializableValue {
    Number(f64),
    Bool(bool),
    Null,
    List(Vec<SerializableValue>),
    String(String),
    Record(IndexMap<String, SerializableValue>),
    Lambda(SerializableLambdaDef),
    BuiltIn(String),
}

impl SerializableValue {
    /// Convert from serde_json::Value to SerializableValue
    pub fn from_json(value: &serde_json::Value) -> SerializableValue {
        match value {
            serde_json::Value::Number(n) => SerializableValue::Number(n.as_f64().unwrap_or(0.0)),
            serde_json::Value::Bool(b) => SerializableValue::Bool(*b),
            serde_json::Value::Null => SerializableValue::Null,
            serde_json::Value::String(s) => SerializableValue::String(s.clone()),
            serde_json::Value::Array(arr) => {
                SerializableValue::List(arr.iter().map(Self::from_json).collect())
            }
            serde_json::Value::Object(obj) => {
                // Check if this is a function object
                if let Some(func_value) = obj.get("__blots_function") {
                    if let Some(func_str) = func_value.as_str() {
                        // Try to parse as a built-in function first (just a name)
                        if crate::functions::BuiltInFunction::from_ident(func_str).is_some() {
                            return SerializableValue::BuiltIn(func_str.to_string());
                        }

                        // Otherwise, parse as a lambda function
                        // We need to parse the function source and convert it to a SerializableLambdaDef
                        if let Ok(lambda_def) = Self::parse_function_source(func_str) {
                            return SerializableValue::Lambda(lambda_def);
                        }
                    }
                }

                // Regular record
                let map: IndexMap<String, SerializableValue> = obj
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::from_json(v)))
                    .collect();
                SerializableValue::Record(map)
            }
        }
    }

    /// Parse a function source string into a SerializableLambdaDef
    fn parse_function_source(source: &str) -> Result<SerializableLambdaDef> {
        use crate::expressions::pairs_to_expr;
        use crate::parser::get_pairs;

        // Parse the source as an expression
        let pairs = get_pairs(source)?;

        // Extract the lambda expression from the parsed pairs
        for pair in pairs {
            if let crate::parser::Rule::statement = pair.as_rule() {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let crate::parser::Rule::expression = inner_pair.as_rule() {
                        // Parse the expression to get an AST
                        let expr = pairs_to_expr(inner_pair.into_inner())?;

                        // Check if it's a lambda
                        if let crate::ast::Expr::Lambda { args, body } = expr {
                            // Since the function is already inlined (no scope needed),
                            // we create a SerializableLambdaDef with the source as the body
                            return Ok(SerializableLambdaDef {
                                name: None,
                                args,
                                body: crate::ast_to_source::expr_to_source(&body),
                                scope: None, // Functions from JSON have no scope - they're already inlined
                            });
                        }
                    }
                }
            }
        }

        Err(anyhow!("Failed to parse function source: {}", source))
    }

    /// Convert SerializableValue to clean serde_json::Value
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            SerializableValue::Number(n) => serde_json::Value::Number(
                serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
            ),
            SerializableValue::Bool(b) => serde_json::Value::Bool(*b),
            SerializableValue::Null => serde_json::Value::Null,
            SerializableValue::String(s) => serde_json::Value::String(s.clone()),
            SerializableValue::List(items) => {
                serde_json::Value::Array(items.iter().map(|v| v.to_json()).collect())
            }
            SerializableValue::Record(fields) => {
                let map: serde_json::Map<String, serde_json::Value> = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_json()))
                    .collect();
                serde_json::Value::Object(map)
            }
            SerializableValue::Lambda(lambda_def) => {
                // We can't inline scope values from SerializableLambdaDef because the body is already a string
                // For now, output the function as a JSON object with the source code
                let mut map = serde_json::Map::new();

                // Build the function source with arguments
                let args_str: Vec<String> = lambda_def
                    .args
                    .iter()
                    .map(|arg| match arg {
                        LambdaArg::Required(name) => name.clone(),
                        LambdaArg::Optional(name) => format!("{}?", name),
                        LambdaArg::Rest(name) => format!("...{}", name),
                    })
                    .collect();

                let function_source = format!("({}) => {}", args_str.join(", "), lambda_def.body);

                map.insert(
                    "__blots_function".to_string(),
                    serde_json::Value::String(function_source),
                );
                serde_json::Value::Object(map)
            }
            SerializableValue::BuiltIn(name) => {
                // Output built-in functions in the same format as lambdas
                let mut map = serde_json::Map::new();
                map.insert(
                    "__blots_function".to_string(),
                    serde_json::Value::String(name.clone()),
                );
                serde_json::Value::Object(map)
            }
        }
    }

    pub fn from_value(value: &Value, heap: &Heap) -> Result<SerializableValue> {
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
                    .collect::<Result<IndexMap<String, SerializableValue>>>()?;
                Ok(SerializableValue::Record(serialized_record))
            }
            Value::Lambda(p) => {
                let lambda = p.reify(heap).as_lambda()?;

                // Convert the scope to SerializableValues
                let serializable_scope: IndexMap<String, SerializableValue> = lambda
                    .scope
                    .clone()
                    .into_iter()
                    .map(|(k, v)| SerializableValue::from_value(&v, heap).map(|sv| (k, sv)))
                    .collect::<Result<IndexMap<String, SerializableValue>>>()?;

                // Generate the body with inlined scope values
                let body_with_inlined_scope = crate::ast_to_source::expr_to_source_with_scope(
                    &lambda.body,
                    &serializable_scope,
                );

                Ok(SerializableValue::Lambda(SerializableLambdaDef {
                    name: lambda.name.clone(),
                    args: lambda.args.clone(),
                    body: body_with_inlined_scope,
                    scope: Some(serializable_scope),
                }))
            }
            Value::BuiltIn(built_in) => Ok(SerializableValue::BuiltIn(built_in.name().to_string())),
            Value::Spread(_) => Err(anyhow!("cannot serialize a spread value")),
        }
    }

    pub fn to_value(&self, heap: &mut Heap) -> Result<Value> {
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
                    .collect::<Result<IndexMap<String, Value>>>()?;
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

                // For now, parse the body string back to AST
                // In a real implementation, we'd want to serialize/deserialize the AST properly
                let body_ast = crate::expressions::pairs_to_expr(
                    crate::parser::get_pairs(&s_lambda.body)?
                        .next()
                        .unwrap()
                        .into_inner(),
                )?;

                let lambda = LambdaDef {
                    name: s_lambda.name.clone(),
                    args: s_lambda.args.clone(),
                    body: body_ast,
                    scope,
                };

                Ok(heap.insert_lambda(lambda))
            }
            SerializableValue::BuiltIn(ident) => BuiltInFunction::from_ident(ident)
                .ok_or(anyhow!("built-in function with ident {} not found", ident))
                .map(Value::BuiltIn),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum PrimitiveValue {
    Number(f64),
    Bool(bool),
    Null,
}

impl From<PrimitiveValue> for Value {
    fn from(val: PrimitiveValue) -> Self {
        match val {
            PrimitiveValue::Number(n) => Value::Number(n),
            PrimitiveValue::Bool(b) => Value::Bool(b),
            PrimitiveValue::Null => Value::Null,
        }
    }
}

impl From<PrimitiveValue> for SerializableValue {
    fn from(val: PrimitiveValue) -> Self {
        match val {
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
    BuiltIn(BuiltInFunction),
}

impl Value {
    /// Compare two values for equality by their actual content, not by reference
    pub fn equals(&self, other: &Value, heap: &Heap) -> Result<bool> {
        match (self, other) {
            // Primitive types - compare directly
            (Value::Number(a), Value::Number(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Null, Value::Null) => Ok(true),

            // String comparison - compare content
            (Value::String(a_ptr), Value::String(b_ptr)) => {
                let a_str = a_ptr.reify(heap).as_string()?;
                let b_str = b_ptr.reify(heap).as_string()?;
                Ok(a_str == b_str)
            }

            // List comparison - compare elements recursively
            (Value::List(a_ptr), Value::List(b_ptr)) => {
                let a_list = a_ptr.reify(heap).as_list()?;
                let b_list = b_ptr.reify(heap).as_list()?;

                if a_list.len() != b_list.len() {
                    return Ok(false);
                }

                for (a_elem, b_elem) in a_list.iter().zip(b_list.iter()) {
                    if !a_elem.equals(b_elem, heap)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }

            // Record comparison - compare keys and values
            (Value::Record(a_ptr), Value::Record(b_ptr)) => {
                let a_record = a_ptr.reify(heap).as_record()?;
                let b_record = b_ptr.reify(heap).as_record()?;

                if a_record.len() != b_record.len() {
                    return Ok(false);
                }

                for (key, a_value) in a_record.iter() {
                    match b_record.get(key) {
                        Some(b_value) => {
                            if !a_value.equals(b_value, heap)? {
                                return Ok(false);
                            }
                        }
                        None => return Ok(false),
                    }
                }
                Ok(true)
            }

            // Lambda comparison - compare by structure (AST equality)
            (Value::Lambda(a_ptr), Value::Lambda(b_ptr)) => {
                let a_lambda = a_ptr.reify(heap).as_lambda()?;
                let b_lambda = b_ptr.reify(heap).as_lambda()?;

                // Compare argument lists
                if a_lambda.args != b_lambda.args {
                    return Ok(false);
                }

                // Compare AST bodies
                if a_lambda.body != b_lambda.body {
                    return Ok(false);
                }

                // Note: We don't compare scopes because two functions with the same
                // definition but different closures would have different behavior
                // For now, we only compare structure, not captured variables
                Ok(true)
            }

            // Built-in functions - compare by ID
            (Value::BuiltIn(a), Value::BuiltIn(b)) => Ok(a == b),

            // Spread values - compare the underlying iterable
            (Value::Spread(a_ptr), Value::Spread(b_ptr)) => match (a_ptr, b_ptr) {
                (IterablePointer::List(a), IterablePointer::List(b)) => {
                    Value::List(*a).equals(&Value::List(*b), heap)
                }
                (IterablePointer::String(a), IterablePointer::String(b)) => {
                    Value::String(*a).equals(&Value::String(*b), heap)
                }
                (IterablePointer::Record(a), IterablePointer::Record(b)) => {
                    Value::Record(*a).equals(&Value::Record(*b), heap)
                }
                _ => Ok(false),
            },

            // Different types are never equal
            _ => Ok(false),
        }
    }

    /// Compare two values for ordering
    pub fn compare(&self, other: &Value, heap: &Heap) -> Result<Option<std::cmp::Ordering>> {
        match (self, other) {
            // Numbers have natural ordering
            (Value::Number(a), Value::Number(b)) => Ok(a.partial_cmp(b)),

            // Booleans: false < true
            (Value::Bool(a), Value::Bool(b)) => Ok(a.partial_cmp(b)),

            // Strings compare lexicographically
            (Value::String(a_ptr), Value::String(b_ptr)) => {
                let a_str = a_ptr.reify(heap).as_string()?;
                let b_str = b_ptr.reify(heap).as_string()?;
                Ok(a_str.partial_cmp(b_str))
            }

            // Lists compare lexicographically
            (Value::List(a_ptr), Value::List(b_ptr)) => {
                let a_list = a_ptr.reify(heap).as_list()?;
                let b_list = b_ptr.reify(heap).as_list()?;

                for (a_elem, b_elem) in a_list.iter().zip(b_list.iter()) {
                    match a_elem.compare(b_elem, heap)? {
                        Some(std::cmp::Ordering::Equal) => continue,
                        other => return Ok(other),
                    }
                }

                // If all compared elements are equal, compare lengths
                Ok(a_list.len().partial_cmp(&b_list.len()))
            }

            // Other types don't have a natural ordering
            _ => Ok(None),
        }
    }

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
            Value::BuiltIn(built_in) => Ok(built_in.name()),
            _ => Err(anyhow!(
                "expected a built-in function, but got a {}",
                self.get_type()
            )),
        }
    }

    pub fn as_record<'h>(&self, heap: &'h Heap) -> Result<&'h IndexMap<String, Value>> {
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
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn to_serializable_value(&self, heap: &Heap) -> Result<SerializableValue> {
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

    /// Stringify the value. Returns the same thing as stringify, except for
    /// Value::String, which is returned without wrapping quotes. Use this for string
    /// concatenation, formatting, etc. Don't use this for displaying values to the user.
    pub fn stringify_internal(&self, heap: &Heap) -> String {
        self.stringify(heap, false)
    }

    pub fn stringify_external(&self, heap: &Heap) -> String {
        self.stringify(heap, true)
    }

    fn stringify(&self, heap: &Heap, wrap_strings: bool) -> String {
        match self {
            Value::String(p) => p
                .reify(heap)
                .as_string()
                .map(|s| {
                    if wrap_strings {
                        format!("\"{}\"", s)
                    } else {
                        s.to_string()
                    }
                })
                .unwrap(),
            Value::List(p) => {
                let mut result = String::from("[");
                let list = p.reify(heap).as_list().unwrap();

                for (i, value) in list.iter().enumerate() {
                    result.push_str(&value.stringify(heap, wrap_strings));
                    if i < list.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            Value::Record(p) => {
                let mut result = String::from("{");
                let record = p.reify(heap).as_record().unwrap();

                for (i, (key, value)) in record.iter().enumerate() {
                    result.push_str(&format!("{}: {}", key, value.stringify(heap, wrap_strings)));
                    if i < record.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push('}');
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
                result.push_str(") => ");
                result.push_str(&crate::ast_to_source::expr_to_source(&lambda.body));
                result
            }
            Value::BuiltIn(built_in) => {
                format!("{} (built-in)", built_in.name())
            }
            Value::Spread(p) => match p {
                IterablePointer::List(l) => {
                    let list = l.reify(heap).as_list().unwrap();
                    let mut result = String::from("...");
                    result.push_str(
                        &list
                            .iter()
                            .map(|v| v.stringify(heap, wrap_strings))
                            .collect::<String>(),
                    );
                    result
                }
                IterablePointer::String(s) => {
                    let string = s.reify(heap).as_string().unwrap();
                    format!("...{}", string)
                }
                IterablePointer::Record(r) => {
                    let record = r.reify(heap).as_record().unwrap();
                    let mut result = String::from("...");
                    result.push('{');
                    for (i, (key, value)) in record.iter().enumerate() {
                        result.push_str(&format!(
                            "{}: {}",
                            key,
                            value.stringify(heap, wrap_strings)
                        ));
                        if i < record.len() - 1 {
                            result.push_str(", ");
                        }
                    }
                    result.push('}');
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
            Value::BuiltIn(built_in) => write!(f, "{} (built-in)", built_in.name()),
            Value::Record(p) => write!(f, "{}", p),
        }
    }
}
