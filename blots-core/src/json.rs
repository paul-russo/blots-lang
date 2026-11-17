//! JSON, read into the values of a heap and written from them.
//!
//! Both go straight between the text and the heap: nothing is made of a document but the
//! values it stands for, and nothing of a value but its text. What is read and written is
//! what [`SerializableValue::from_json`] and [`SerializableValue::to_json`] come to by way
//! of a tree of each kind:
//!
//! - an object becomes a record that has its entries in the order of their keys, with the
//!   last entry of every key that the object has more than once, and a record is written
//!   with its entries in the order of their keys;
//! - an object that has the name of a built-in function or the source of a lambda under
//!   [`FUNCTION_KEY`] becomes that function, and a function is written as such an object;
//! - every number becomes a float, and a number that JSON has no text for is written as 0.

use crate::{
    heap::{Heap, HeapPointer},
    record::{Keys, Record, Shapes},
    values::{FUNCTION_KEY, SerializableValue, Value},
};
use serde::{
    de::{DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor},
    ser::{Error as _, Serialize, SerializeMap, SerializeSeq, Serializer},
};
use std::{borrow::Cow, cell::RefCell, collections::HashMap, fmt, rc::Rc};

/// What a JSON document gives a program as its inputs.
#[derive(Debug, PartialEq)]
pub enum Inputs {
    /// The entries of an object, in the order of their keys: an input under each key. An
    /// entry that has a function in it which cannot be made is not among them.
    Named(Vec<(String, Value)>),

    /// A value that is not an object: one input, which has no name of its own. There is
    /// none when the value has a function in it which cannot be made.
    Unnamed(Option<Value>),
}

/// The inputs that `text` holds, as values of `heap`.
pub fn read_inputs(text: &str, heap: &mut Heap) -> serde_json::Result<Inputs> {
    let mut reader = Reader {
        heap,
        items: Vec::new(),
        entries: Vec::new(),
        shapes: Shapes::default(),
        unreadable: false,
    };
    let mut deserializer = serde_json::Deserializer::from_str(text);

    let inputs = match starts_object(text) {
        true => Inputs::Named(deserializer.deserialize_map(InputsOf(&mut reader))?),
        false => {
            let value = ValueOf(&mut reader).deserialize(&mut deserializer)?;

            Inputs::Unnamed((!reader.unreadable).then_some(value))
        }
    };
    deserializer.end()?;

    Ok(inputs)
}

/// Whether the first thing in `text` that is not whitespace opens an object.
fn starts_object(text: &str) -> bool {
    let first = text
        .bytes()
        .find(|byte| !matches!(byte, b' ' | b'\t' | b'\n' | b'\r'));

    first == Some(b'{')
}

/// What reading a document takes: the heap its values go to, and the values that still
/// wait for the list or the object they are in to end.
struct Reader<'h, 'de> {
    heap: &'h mut Heap,

    /// The elements so far of the lists that are being read, the innermost list last.
    items: Vec<Value>,

    /// The entries so far of the objects that are being read, the innermost object last.
    entries: Vec<(Cow<'de, str>, Value)>,

    /// The tables of the records so far, which the records with the same keys share.
    shapes: Shapes,

    /// Whether a function was read that could not be made. The value that is in its place
    /// is not what the document says, and neither is anything that has it.
    unreadable: bool,
}

impl Reader<'_, '_> {
    /// The value that the object stands for whose entries are those from `start` on, which
    /// leave with it. The object has an entry under [`FUNCTION_KEY`] if it `is_marked`.
    fn finish_object(&mut self, start: usize, is_marked: bool) -> Value {
        order(&mut self.entries, start);

        let function = match is_marked {
            true => self.function_of(start),
            false => None,
        };

        let value = match function {
            Some(function) => function,
            None => {
                let entries = &self.entries[start..];
                let table = self.shapes.table_of(entries.iter().map(|(key, _)| &**key));
                let values: Box<[Value]> = entries.iter().map(|(_, value)| *value).collect();

                self.heap.insert_record(Record::new(table, values))
            }
        };
        self.entries.truncate(start);

        value
    }

    /// The function that the object stands for whose entries are those from `start` on, in
    /// the order of their keys, if it stands for one.
    fn function_of(&mut self, start: usize) -> Option<Value> {
        let entries = &self.entries[start..];
        let position = entries
            .binary_search_by(|(key, _)| (**key).cmp(FUNCTION_KEY))
            .ok()?;
        let Value::String(source) = entries[position].1 else {
            return None;
        };

        let source = source.reify(self.heap).as_string().ok()?.to_string();
        let function = SerializableValue::function_from_source(&source)?;

        Some(function.to_value(self.heap).unwrap_or_else(|_| {
            self.unreadable = true;

            Value::Null
        }))
    }
}

/// Put the entries from `start` on in the order of their keys, and leave only the last one
/// of those that have the same key.
fn order<T>(entries: &mut Vec<(Cow<'_, str>, T)>, start: usize) {
    // Most objects are written in the order of their keys or share that order with the
    // ones around them, so it is worth a look before sorting.
    let is_ordered = entries[start..]
        .windows(2)
        .all(|pair| pair[0].0 < pair[1].0);
    if is_ordered {
        return;
    }

    // The sort keeps entries with the same key in the order they were read in.
    entries[start..].sort_by(|(first, _), (second, _)| first.cmp(second));

    let mut kept = start;
    for position in start..entries.len() {
        let is_last_of_key = entries
            .get(position + 1)
            .is_none_or(|next| next.0 != entries[position].0);

        if is_last_of_key {
            entries.swap(kept, position);
            kept += 1;
        }
    }
    entries.truncate(kept);
}

/// Reads any JSON value as a value of the heap of its reader.
struct ValueOf<'r, 'h, 'de>(&'r mut Reader<'h, 'de>);

impl<'de> DeserializeSeed<'de> for ValueOf<'_, '_, 'de> {
    type Value = Value;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
        deserializer.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for ValueOf<'_, '_, 'de> {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    fn visit_bool<E>(self, value: bool) -> Result<Value, E> {
        Ok(Value::Bool(value))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
        Ok(Value::Number(value as f64))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
        Ok(Value::Number(value as f64))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
        Ok(Value::Number(value))
    }

    fn visit_unit<E>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_str<E>(self, value: &str) -> Result<Value, E> {
        Ok(self.0.heap.insert_string(value))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut list: A) -> Result<Value, A::Error> {
        let reader = self.0;
        let start = reader.items.len();

        while let Some(item) = list.next_element_seed(ValueOf(&mut *reader))? {
            reader.items.push(item);
        }

        // The elements leave for a vector that just fits them.
        let items = reader.items.split_off(start);

        Ok(reader.heap.insert_list(items))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut object: A) -> Result<Value, A::Error> {
        let reader = self.0;
        let start = reader.entries.len();
        let mut is_marked = false;

        while let Some(key) = object.next_key_seed(KeyOf)? {
            let value = object.next_value_seed(ValueOf(&mut *reader))?;

            is_marked |= key == FUNCTION_KEY;
            reader.entries.push((key, value));
        }

        Ok(reader.finish_object(start, is_marked))
    }
}

/// Reads the key of an entry: the characters of the document where it has them as they
/// are, and a copy of what they stand for where the document escapes some of them.
struct KeyOf;

impl<'de> DeserializeSeed<'de> for KeyOf {
    type Value = Cow<'de, str>;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_str(self)
    }
}

impl<'de> Visitor<'de> for KeyOf {
    type Value = Cow<'de, str>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a string key")
    }

    fn visit_borrowed_str<E>(self, key: &'de str) -> Result<Self::Value, E> {
        Ok(Cow::Borrowed(key))
    }

    fn visit_str<E>(self, key: &str) -> Result<Self::Value, E> {
        Ok(Cow::Owned(key.to_owned()))
    }
}

/// Reads the object that a document is as the inputs under its keys. That object does not
/// stand for a function, whatever its keys are.
struct InputsOf<'r, 'h, 'de>(&'r mut Reader<'h, 'de>);

impl<'de> Visitor<'de> for InputsOf<'_, '_, 'de> {
    type Value = Vec<(String, Value)>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a JSON object")
    }

    fn visit_map<A: MapAccess<'de>>(self, mut object: A) -> Result<Self::Value, A::Error> {
        let reader = self.0;

        // An entry that cannot be read still takes the place of the ones before it that
        // have its key, so it stays among the entries until they are in order.
        let mut entries: Vec<(Cow<'de, str>, Option<Value>)> = Vec::new();

        while let Some(key) = object.next_key_seed(KeyOf)? {
            let value = object.next_value_seed(ValueOf(&mut *reader))?;
            let is_readable = !std::mem::take(&mut reader.unreadable);

            entries.push((key, is_readable.then_some(value)));
        }

        order(&mut entries, 0);

        let inputs = entries
            .into_iter()
            .filter_map(|(key, value)| Some((key.into_owned(), value?)))
            .collect();

        Ok(inputs)
    }
}

/// The JSON text of `value`, which is a value of `heap`.
pub fn to_text(value: &Value, heap: &Heap) -> serde_json::Result<String> {
    let orders = Orders::default();

    serde_json::to_string(&Written {
        value,
        heap,
        orders: &orders,
    })
}

/// The positions of the entries of a record in the order they are written in, or `None`
/// for a record that has them in that order already.
type Order = Option<Rc<[usize]>>;

/// The order in which the entries of the records of each table are written, for the tables
/// that a record was written of. Records that share a table and have as many of its keys
/// have the same keys, and so the same order, which is worked out once.
///
/// A table is known by its address, which is that of no other table for as long as the heap
/// is borrowed that the records are in.
#[derive(Default)]
struct Orders(RefCell<HashMap<(*const Keys, usize), Order>>);

impl Orders {
    /// The order to write the entries of `record` in.
    fn of(&self, record: &Record) -> Order {
        let shape = (Rc::as_ptr(record.table()), record.len());

        self.0
            .borrow_mut()
            .entry(shape)
            .or_insert_with(|| order_of(record))
            .clone()
    }
}

/// The order to write the entries of `record` in: that of their keys.
fn order_of(record: &Record) -> Order {
    if record.keys().is_sorted() {
        return None;
    }

    let mut positions: Vec<usize> = (0..record.len()).collect();
    let keys = record.table().first(record.len());
    positions.sort_unstable_by(|&first, &second| keys[first].cmp(&keys[second]));

    Some(positions.into())
}

/// A value of a heap, for serde to write.
struct Written<'w> {
    value: &'w Value,
    heap: &'w Heap,
    orders: &'w Orders,
}

impl<'w> Written<'w> {
    /// `value`, which is in the same heap, for serde to write.
    fn with(&self, value: &'w Value) -> Written<'w> {
        Written { value, ..*self }
    }
}

impl Serialize for Written<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self.value {
            Value::Number(number) if number.is_finite() => serializer.serialize_f64(*number),
            Value::Number(_) => serializer.serialize_u64(0),
            Value::Bool(value) => serializer.serialize_bool(*value),
            Value::Null => serializer.serialize_unit(),
            Value::String(pointer) => {
                let text = pointer
                    .reify(self.heap)
                    .as_string()
                    .map_err(S::Error::custom)?;

                serializer.serialize_str(text)
            }
            Value::List(pointer) => {
                let list = pointer
                    .reify(self.heap)
                    .as_list()
                    .map_err(S::Error::custom)?;
                let mut items = serializer.serialize_seq(Some(list.len()))?;

                for item in list.as_slice() {
                    items.serialize_element(&self.with(item))?;
                }

                items.end()
            }
            Value::Record(pointer) => {
                let record = pointer
                    .reify(self.heap)
                    .as_record()
                    .map_err(S::Error::custom)?;
                let keys = record.table().first(record.len());
                let values = record.as_slice();
                let mut entries = serializer.serialize_map(Some(record.len()))?;

                match self.orders.of(record) {
                    None => {
                        for (key, value) in keys.iter().zip(values) {
                            entries.serialize_entry(&**key, &self.with(value))?;
                        }
                    }
                    Some(order) => {
                        for &position in order.iter() {
                            entries
                                .serialize_entry(&*keys[position], &self.with(&values[position]))?;
                        }
                    }
                }

                entries.end()
            }
            // A function is written as the object that it is read from, which takes its
            // source: that is made the way it always was.
            Value::Lambda(_) | Value::BuiltIn(_) | Value::Spread(_) => {
                SerializableValue::from_value(self.value, self.heap)
                    .map_err(S::Error::custom)?
                    .to_json()
                    .serialize(serializer)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::record::RecordBuilder;
    use indexmap::IndexMap;

    /// The inputs that `text` holds, under the key `value` when they have no names.
    fn inputs_of(text: &str, heap: &mut Heap) -> serde_json::Result<Vec<(String, Value)>> {
        let inputs = match read_inputs(text, heap)? {
            Inputs::Named(entries) => entries,
            Inputs::Unnamed(value) => value
                .map(|value| ("value".to_string(), value))
                .into_iter()
                .collect(),
        };

        Ok(inputs)
    }

    /// The inputs that `text` holds, written again: read and written here.
    fn directly(text: &str) -> Result<String, String> {
        let mut heap = Heap::new();
        let inputs = inputs_of(text, &mut heap).map_err(|error| error.to_string())?;

        let entries: Vec<String> = inputs
            .iter()
            .map(|(key, value)| {
                let key = serde_json::to_string(key).unwrap();

                format!("{key}:{}", to_text(value, &heap).unwrap())
            })
            .collect();

        Ok(format!("{{{}}}", entries.join(",")))
    }

    /// The inputs that `text` holds, written again: read and written by way of a tree of
    /// the document and a tree of every value, which is what reading and writing here has
    /// to come to.
    fn by_trees(text: &str) -> Result<String, String> {
        let document: serde_json::Value =
            serde_json::from_str(text).map_err(|error| error.to_string())?;
        let mut heap = Heap::new();

        let entries: Vec<(String, &serde_json::Value)> = match &document {
            serde_json::Value::Object(object) => object
                .iter()
                .map(|(key, value)| (key.clone(), value))
                .collect(),
            other => vec![("value".to_string(), other)],
        };

        let outputs: IndexMap<String, serde_json::Value> = entries
            .into_iter()
            .filter_map(|(key, value)| {
                let value = SerializableValue::from_json(value)
                    .to_value(&mut heap)
                    .ok()?;

                Some((key, value.to_serializable_value(&heap).unwrap().to_json()))
            })
            .collect();

        Ok(serde_json::to_string(&outputs).unwrap())
    }

    #[test]
    fn documents_come_to_what_their_trees_come_to() {
        let documents = [
            r#"{}"#,
            r#"[]"#,
            r#"null"#,
            r#"true"#,
            r#"-0"#,
            r#""text""#,
            r#"  {"b": 1, "a": [true, false, null], "c": {"z": {}, "y": []}}  "#,
            r#"{"a": 1, "b": 2, "a": 3, "b": {"y": 1, "x": 2, "y": 3}}"#,
            r#"[{"b": 1, "a": 2}, {"b": 3, "a": 4}, {"a": 5, "b": 6}, {"b": 7}]"#,
            r#"[1, 2.0, -0.0, 1e-7, 1E5, 0.1, 1e300, 5e-324, 1.5e17, 1e15, 1e16, 1e21]"#,
            r#"[123456789012345678, 18446744073709551615, 18446744073709551616]"#,
            r#"[-9223372036854775808, -9223372036854775809, 123456.789e3, 0.000001]"#,
            r#"[0.1234567890123456789, 2.2250738585072011e-308, 1.7976931348623157e308]"#,
            r#"["", "a\tb\n\"c\"\\", "\u0000\u001f\u007f", "h\u00e9 \ud83d\ude00 \u2028", "</script>"]"#,
            r#"{"k\u00e9y": 1, "k\ney": 2, "": 3, "a rather long key that no head holds": 4}"#,
            r#"{"abcdefgh": 1, "abcdefghi": 2, "abcdefg": 3, "a": 4, "ab": 5, "abc": 6, "abcd": 7,
                "abcde": 8, "abcdef": 9, "b": 10}"#,
            r#"{"f": {"__blots_function": "sum"}, "g": {"__blots_function": "x => x + 1"}}"#,
            r#"{"f": {"__blots_function": "(a, b?, ...rest) => a + len(rest)", "more": 1}}"#,
            r#"{"f": {"__blots_function": "not a function ("}, "g": {"__blots_function": 5}}"#,
            r#"{"f": {"__blots_function": "sum", "__blots_function": "no function either ["}}"#,
            r#"[{"__blots_function": "max"}, [{"__blots_function": "x => x * 2"}]]"#,
            r#"{"__blots_function": "sum"}"#,
        ];

        for document in documents {
            assert_eq!(directly(document), by_trees(document), "{document}");
        }
    }

    #[test]
    fn documents_that_are_wrong_are_refused_the_same_way() {
        let deep = "[".repeat(200);

        let documents = [
            "",
            "  ",
            "{",
            r#"{"a":1"#,
            r#"{"a":1} {"b":2}"#,
            r#"{"a": NaN}"#,
            r#"{"a": 1e400}"#,
            r#"{a: 1}"#,
            r#"{1: 1}"#,
            r#"{"a" 1}"#,
            r#"[1, 2,]"#,
            r#"[1 2]"#,
            r#""unterminated"#,
            r#""bad \x escape""#,
            r#""lone \ud83d surrogate""#,
            "\"control \u{1} character\"",
            "\u{feff}{}",
            "01",
            "nul",
            "[1, tru]",
            deep.as_str(),
        ];

        for document in documents {
            let refused = directly(document);

            assert!(refused.is_err(), "{document}");
            assert_eq!(refused, by_trees(document), "{document}");
        }
    }

    /// A document that `random` makes up: objects that share a few keys, so that some come
    /// more than once and in every order, around lists, texts and numbers of every kind.
    fn made_up(random: &mut fastrand::Rng, depth: usize) -> String {
        const KEYS: [&str; 8] = [
            "a",
            "b",
            "id",
            "name",
            "a key of some length",
            "é",
            "\\n",
            "z",
        ];
        const TEXTS: [&str; 8] = [
            "",
            "short",
            "a text that is too long to be kept where its string is",
            "tab\\there",
            "quote \\\" and \\\\",
            "\\u00e9\\ud83d\\ude00",
            "\\u0001\\b\\f\\r",
            "ünïcödé ✓",
        ];
        const NUMBERS: [&str; 10] = [
            "0",
            "-1",
            "42",
            "3.5",
            "-0.0",
            "1e3",
            "2.5E-3",
            "12345678901234567890",
            "0.1",
            "1e-9",
        ];

        let kinds = if depth == 0 { 4 } else { 6 };

        match random.usize(..kinds) {
            0 => ["null", "true", "false"][random.usize(..3)].to_string(),
            1 => NUMBERS[random.usize(..NUMBERS.len())].to_string(),
            2 | 3 => format!("\"{}\"", TEXTS[random.usize(..TEXTS.len())]),
            4 => {
                let items: Vec<String> = (0..random.usize(..5))
                    .map(|_| made_up(random, depth - 1))
                    .collect();

                format!("[{}]", items.join(", "))
            }
            _ => {
                let entries: Vec<String> = (0..random.usize(..6))
                    .map(|_| {
                        let key = KEYS[random.usize(..KEYS.len())];

                        format!("\"{key}\": {}", made_up(random, depth - 1))
                    })
                    .collect();

                format!("{{{}}}", entries.join(", "))
            }
        }
    }

    #[test]
    fn made_up_documents_come_to_what_their_trees_come_to() {
        let mut random = fastrand::Rng::with_seed(7);

        for _ in 0..2000 {
            let document = made_up(&mut random, 4);

            assert_eq!(directly(&document), by_trees(&document), "{document}");
        }
    }

    #[test]
    fn an_object_becomes_a_record_in_the_order_of_its_keys() {
        let mut heap = Heap::new();
        let inputs = inputs_of(r#"{"r": {"b": 1, "a": 2, "c": 3, "a": 4}}"#, &mut heap).unwrap();

        let Value::Record(pointer) = inputs[0].1 else {
            panic!("an object is read as a record");
        };
        let record = pointer.reify(&heap).as_record().unwrap();
        let entries: Vec<(&str, Value)> =
            record.iter().map(|(key, value)| (&**key, *value)).collect();

        assert_eq!(
            entries,
            [
                ("a", Value::Number(4.0)),
                ("b", Value::Number(1.0)),
                ("c", Value::Number(3.0)),
            ]
        );
    }

    #[test]
    fn the_inputs_of_an_object_are_in_the_order_of_its_keys() {
        let mut heap = Heap::new();
        let inputs = inputs_of(r#"{"b": 1, "a": 2, "b": 3}"#, &mut heap).unwrap();

        assert_eq!(
            inputs,
            [
                ("a".to_string(), Value::Number(2.0)),
                ("b".to_string(), Value::Number(3.0)),
            ]
        );
    }

    #[test]
    fn objects_with_the_same_keys_share_a_table() {
        let mut heap = Heap::new();
        let document = r#"[
            {"id": 1, "name": "a", "tags": [], "created_at": 0, "more": {"x": 1}},
            {"name": "b", "id": 2, "more": {"x": 2}, "tags": [], "created_at": 1},
            {"id": 3, "name": "c", "tags": [], "created_at": 2, "more": {"x": 3, "y": 4}},
            {"id": 4, "name": "d"}
        ]"#;
        let inputs = inputs_of(document, &mut heap).unwrap();

        let Value::List(pointer) = inputs[0].1 else {
            panic!("an array is read as a list");
        };
        let records: Vec<&Record> = pointer
            .reify(&heap)
            .as_list()
            .unwrap()
            .iter()
            .map(|item| match item {
                Value::Record(pointer) => pointer.reify(&heap).as_record().unwrap(),
                other => panic!("an object is read as a record, not as {other:?}"),
            })
            .collect();

        assert!(Rc::ptr_eq(records[0].table(), records[1].table()));
        assert!(Rc::ptr_eq(records[0].table(), records[2].table()));
        assert!(!Rc::ptr_eq(records[0].table(), records[3].table()));

        let inner = |record: &Record| match record.get("more") {
            Some(Value::Record(pointer)) => {
                Rc::clone(pointer.reify(&heap).as_record().unwrap().table())
            }
            other => panic!("an object is read as a record, not as {other:?}"),
        };

        assert!(Rc::ptr_eq(&inner(records[0]), &inner(records[1])));
        assert!(!Rc::ptr_eq(&inner(records[0]), &inner(records[2])));
    }

    #[test]
    fn a_record_is_written_in_the_order_of_its_keys() {
        let mut heap = Heap::new();

        let mut inner = RecordBuilder::with_capacity(2);
        inner.insert("y", Value::Bool(true));
        inner.insert("x", Value::Null);
        let inner = heap.insert_built_record(inner);

        // Two records of one table, and one that has only its first keys.
        let records: Vec<Value> = [3, 3, 2]
            .into_iter()
            .map(|count| {
                let entries = [
                    ("b", Value::Number(1.0)),
                    ("a", inner),
                    ("C", Value::Number(2.5)),
                ];
                let mut builder = RecordBuilder::with_capacity(count);
                for (key, value) in &entries[..count] {
                    builder.insert(key, *value);
                }

                heap.insert_built_record(builder)
            })
            .collect();
        let list = heap.insert_list(records);

        assert_eq!(
            to_text(&list, &heap).unwrap(),
            concat!(
                r#"[{"C":2.5,"a":{"x":null,"y":true},"b":1.0},"#,
                r#"{"C":2.5,"a":{"x":null,"y":true},"b":1.0},"#,
                r#"{"a":{"x":null,"y":true},"b":1.0}]"#,
            )
        );
    }

    #[test]
    fn a_number_that_json_has_no_text_for_is_written_as_zero() {
        let mut heap = Heap::new();
        let numbers = [
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MAX,
            -0.0,
            1.0,
        ];
        let list = heap.insert_list(numbers.map(Value::Number).to_vec());

        assert_eq!(
            to_text(&list, &heap).unwrap(),
            "[0,0,0,1.7976931348623157e308,-0.0,1.0]"
        );
    }
}
