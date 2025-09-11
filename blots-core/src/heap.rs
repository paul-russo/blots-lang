use crate::values::{LambdaDef, PrimitiveValue, ReifiedIterableValue, Value};
use anyhow::{anyhow, Result};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};
use std::sync::LazyLock;

pub static CONSTANTS: LazyLock<IndexMap<String, PrimitiveValue>> = LazyLock::new(|| {
    let mut constants = IndexMap::new();
    constants.insert(
        String::from("pi"),
        PrimitiveValue::Number(core::f64::consts::PI),
    );
    constants.insert(
        String::from("e"),
        PrimitiveValue::Number(core::f64::consts::E),
    );
    constants.insert(String::from("max_value"), PrimitiveValue::Number(f64::MAX));
    constants.insert(
        String::from("min_value"),
        PrimitiveValue::Number(f64::MIN_POSITIVE),
    );
    constants
});

#[derive(Debug, Clone)]
pub enum HeapValue {
    List(Vec<Value>),
    String(String),
    Record(IndexMap<String, Value>),
    Lambda(LambdaDef),
}

impl HeapValue {
    pub fn get_type(&self) -> &str {
        match self {
            HeapValue::List(_) => "list",
            HeapValue::String(_) => "string",
            HeapValue::Record(_) => "record",
            HeapValue::Lambda(_) => "function",
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>> {
        match self {
            HeapValue::List(list) => Ok(list),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_string(&self) -> Result<&str> {
        match self {
            HeapValue::String(string) => Ok(string),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_record(&self) -> Result<&IndexMap<String, Value>> {
        match self {
            HeapValue::Record(record) => Ok(record),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda(&self) -> Result<&LambdaDef> {
        match self {
            HeapValue::Lambda(lambda) => Ok(lambda),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_iterable(&self) -> Result<ReifiedIterableValue<'_>> {
        match self {
            HeapValue::List(list) => Ok(ReifiedIterableValue::List(list)),
            HeapValue::String(string) => Ok(ReifiedIterableValue::String(string)),
            HeapValue::Record(record) => Ok(ReifiedIterableValue::Record(record)),
            _ => Err(anyhow!(
                "expected an iterable, but got a {}",
                self.get_type()
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Heap {
    values: Vec<HeapValue>,
}

impl Heap {
    pub fn new() -> Self {
        let mut s = Self { values: Vec::new() };
        s.insert_record(
            CONSTANTS
                .clone()
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
        );

        s
    }

    pub fn insert(&mut self, value: HeapValue) -> usize {
        self.values.push(value);

        self.values.len() - 1
    }

    pub fn insert_list(&mut self, list: Vec<Value>) -> Value {
        Value::List(ListPointer::new(self.insert(HeapValue::List(list))))
    }

    pub fn insert_string(&mut self, string: String) -> Value {
        Value::String(StringPointer::new(self.insert(HeapValue::String(string))))
    }

    pub fn insert_record(&mut self, record: IndexMap<String, Value>) -> Value {
        Value::Record(RecordPointer::new(self.insert(HeapValue::Record(record))))
    }

    pub fn insert_lambda(&mut self, lambda: LambdaDef) -> Value {
        Value::Lambda(LambdaPointer::new(self.insert(HeapValue::Lambda(lambda))))
    }

    pub fn get(&self, id: usize) -> Option<&HeapValue> {
        self.values.get(id)
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut HeapValue> {
        self.values.get_mut(id)
    }
}

pub trait HeapPointer<'h> {
    fn reify(&self, heap: &'h Heap) -> &'h HeapValue;
    fn reify_mut(&self, heap: &'h mut Heap) -> &'h mut HeapValue;
    fn get_type(&self) -> &str;
}

macro_rules! define_pointer {
    ($name:ident, $type:literal) => {
        #[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, PartialOrd)]
        pub struct $name(usize);

        impl $name {
            pub fn new(index: usize) -> Self {
                $name(index)
            }

            pub fn index(&self) -> usize {
                self.0
            }
        }

        impl<'h> HeapPointer<'h> for $name {
            fn reify(&self, heap: &'h Heap) -> &'h HeapValue {
                heap.get(self.0).expect("Dangling pointer!")
            }

            fn reify_mut(&self, heap: &'h mut Heap) -> &'h mut HeapValue {
                heap.get_mut(self.0).expect("Dangling pointer!")
            }

            fn get_type(&self) -> &str {
                $type
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{}@{}", $type, self.0)
            }
        }
    };
}

define_pointer!(ListPointer, "list");
define_pointer!(StringPointer, "string");
define_pointer!(RecordPointer, "record");
define_pointer!(LambdaPointer, "function");

#[derive(Debug, Copy, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum IterablePointer {
    List(ListPointer),
    String(StringPointer),
    Record(RecordPointer),
}

impl From<ListPointer> for IterablePointer {
    fn from(pointer: ListPointer) -> Self {
        IterablePointer::List(pointer)
    }
}

impl From<StringPointer> for IterablePointer {
    fn from(pointer: StringPointer) -> Self {
        IterablePointer::String(pointer)
    }
}

impl From<RecordPointer> for IterablePointer {
    fn from(pointer: RecordPointer) -> Self {
        IterablePointer::Record(pointer)
    }
}

impl<'h> HeapPointer<'h> for IterablePointer {
    fn get_type(&self) -> &str {
        match self {
            IterablePointer::List(pointer) => pointer.get_type(),
            IterablePointer::String(pointer) => pointer.get_type(),
            IterablePointer::Record(pointer) => pointer.get_type(),
        }
    }

    fn reify(&self, heap: &'h Heap) -> &'h HeapValue {
        match self {
            IterablePointer::List(pointer) => pointer.reify(heap),
            IterablePointer::String(pointer) => pointer.reify(heap),
            IterablePointer::Record(pointer) => pointer.reify(heap),
        }
    }

    fn reify_mut(&self, heap: &'h mut Heap) -> &'h mut HeapValue {
        match self {
            IterablePointer::List(pointer) => pointer.reify_mut(heap),
            IterablePointer::String(pointer) => pointer.reify_mut(heap),
            IterablePointer::Record(pointer) => pointer.reify_mut(heap),
        }
    }
}

impl Display for IterablePointer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            IterablePointer::List(pointer) => write!(f, "{}", pointer),
            IterablePointer::String(pointer) => write!(f, "{}", pointer),
            IterablePointer::Record(pointer) => write!(f, "{}", pointer),
        }
    }
}
