//! Records: ordered collections of values under string keys.
//!
//! A record is stored as its *shape* (the keys, in insertion order) and one value per key.
//! Every record built by the same record literal shares one shape, so creating it costs a
//! single allocation for the values, and a field read that has seen the shape before finds
//! the field by position without looking at any key.

use crate::values::Value;
use indexmap::{IndexMap, IndexSet};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// Shapes with at most this many keys are searched linearly, which beats hashing the key.
const LINEAR_SEARCH_KEYS: usize = 8;

thread_local! {
    /// The canonical shape of every key list that was interned on this thread.
    static INTERNED: RefCell<HashMap<Vec<String>, Rc<Shape>>> = RefCell::new(HashMap::new());
}

/// The keys of a record, in insertion order.
#[derive(Debug, Default)]
pub struct Shape {
    keys: IndexSet<String>,

    /// Whether this is the canonical shape of its key list (see [`Shape::interned`]).
    interned: bool,
}

impl Shape {
    pub fn new(keys: IndexSet<String>) -> Self {
        Self {
            keys,
            interned: false,
        }
    }

    /// The canonical shape for `keys`: the same one for every call with an equal list of keys.
    /// A canonical shape stays alive for as long as its thread does, so it can be recognised
    /// by its address.
    pub fn interned(keys: IndexSet<String>) -> Rc<Shape> {
        INTERNED.with_borrow_mut(|shapes| {
            let list: Vec<String> = keys.iter().cloned().collect();
            if let Some(shape) = shapes.get(&list) {
                return Rc::clone(shape);
            }

            let shape = Rc::new(Shape {
                keys,
                interned: true,
            });
            shapes.insert(list, Rc::clone(&shape));

            shape
        })
    }

    pub fn is_interned(&self) -> bool {
        self.interned
    }

    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// The key at `position`.
    #[inline]
    pub fn key(&self, position: usize) -> Option<&String> {
        self.keys.get_index(position)
    }

    /// The position of `key`.
    #[inline]
    pub fn position(&self, key: &str) -> Option<usize> {
        if self.keys.len() <= LINEAR_SEARCH_KEYS {
            return self.keys.iter().position(|own| own == key);
        }

        self.keys.get_index_of(key)
    }

    pub fn keys(&self) -> indexmap::set::Iter<'_, String> {
        self.keys.iter()
    }
}

/// A collection of values under string keys that keeps the order the keys were added in.
#[derive(Debug, Clone)]
pub struct Record {
    shape: Rc<Shape>,

    /// One value per key of the shape, in the same order.
    values: Box<[Value]>,
}

impl Record {
    /// A record with the keys of `shape` and one value for each of them, in the same order.
    pub fn new(shape: Rc<Shape>, values: Box<[Value]>) -> Self {
        assert_eq!(shape.len(), values.len(), "a record has one value per key");

        Self { shape, values }
    }

    pub fn shape(&self) -> &Rc<Shape> {
        &self.shape
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.shape
            .position(key)
            .map(|position| &self.values[position])
    }

    /// The position, key and value of the entry under `key`.
    pub fn get_full(&self, key: &str) -> Option<(usize, &String, &Value)> {
        let position = self.shape.position(key)?;
        let (key, value) = self.get_index(position)?;

        Some((position, key, value))
    }

    /// The entry at `position`.
    pub fn get_index(&self, position: usize) -> Option<(&String, &Value)> {
        Some((self.shape.key(position)?, self.values.get(position)?))
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.shape.position(key).is_some()
    }

    pub fn keys(&self) -> indexmap::set::Iter<'_, String> {
        self.shape.keys()
    }

    pub fn values(&self) -> std::slice::Iter<'_, Value> {
        self.values.iter()
    }

    pub fn values_mut(&mut self) -> std::slice::IterMut<'_, Value> {
        self.values.iter_mut()
    }

    /// The values, in the order of the keys.
    pub fn as_slice(&self) -> &[Value] {
        &self.values
    }

    pub fn iter(&self) -> Iter<'_> {
        self.shape.keys().zip(self.values.iter())
    }
}

pub type Iter<'r> = std::iter::Zip<indexmap::set::Iter<'r, String>, std::slice::Iter<'r, Value>>;

impl<'r> IntoIterator for &'r Record {
    type Item = (&'r String, &'r Value);
    type IntoIter = Iter<'r>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Records are equal when they hold equal values under the same keys, in any order.
impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .all(|(key, value)| other.get(key) == Some(value))
    }
}

impl FromIterator<(String, Value)> for Record {
    fn from_iter<I: IntoIterator<Item = (String, Value)>>(entries: I) -> Self {
        let mut builder = RecordBuilder::default();
        for (key, value) in entries {
            builder.insert(key, value);
        }

        builder.finish()
    }
}

impl From<IndexMap<String, Value>> for Record {
    fn from(entries: IndexMap<String, Value>) -> Self {
        entries.into_iter().collect()
    }
}

/// A record that is still being assembled, one entry at a time.
#[derive(Debug, Default)]
pub struct RecordBuilder {
    keys: IndexSet<String>,
    values: Vec<Value>,
}

impl RecordBuilder {
    /// Add an entry. A key that is already present keeps its position and gets the new value.
    pub fn insert(&mut self, key: String, value: Value) {
        let (position, added) = self.keys.insert_full(key);

        if added {
            self.values.push(value);
        } else {
            self.values[position] = value;
        }
    }

    pub fn finish(self) -> Record {
        Record::new(
            Rc::new(Shape::new(self.keys)),
            self.values.into_boxed_slice(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn keys(names: &[&str]) -> IndexSet<String> {
        names.iter().map(|name| name.to_string()).collect()
    }

    fn record(entries: &[(&str, f64)]) -> Record {
        entries
            .iter()
            .map(|(key, number)| (key.to_string(), Value::Number(*number)))
            .collect()
    }

    #[test]
    fn interned_shapes_are_shared_per_key_list() {
        let first = Shape::interned(keys(&["x", "y"]));
        let again = Shape::interned(keys(&["x", "y"]));
        let reordered = Shape::interned(keys(&["y", "x"]));

        assert!(Rc::ptr_eq(&first, &again));
        assert!(!Rc::ptr_eq(&first, &reordered));
        assert!(first.is_interned());
        assert!(!Shape::new(keys(&["x", "y"])).is_interned());
    }

    #[test]
    fn positions_follow_insertion_order_for_small_and_large_shapes() {
        let names: Vec<String> = (0..40).map(|index| format!("key{}", index)).collect();

        for count in [0, 1, LINEAR_SEARCH_KEYS, LINEAR_SEARCH_KEYS + 1, 40] {
            let shape = Shape::new(names[..count].iter().cloned().collect());

            assert_eq!(shape.len(), count);
            for (position, name) in names[..count].iter().enumerate() {
                assert_eq!(shape.position(name), Some(position));
                assert_eq!(shape.key(position), Some(name));
            }
            assert_eq!(shape.position("missing"), None);
            assert_eq!(shape.key(count), None);
        }
    }

    #[test]
    fn repeated_keys_keep_their_position_and_take_the_last_value() {
        let record = record(&[("a", 1.0), ("b", 2.0), ("a", 3.0)]);

        assert_eq!(record.len(), 2);
        assert_eq!(record.keys().collect::<Vec<_>>(), ["a", "b"]);
        assert_eq!(record.get("a"), Some(&Value::Number(3.0)));
        assert_eq!(
            record.get_full("b"),
            Some((1, &"b".to_string(), &Value::Number(2.0)))
        );
        assert_eq!(record.get("c"), None);
        assert!(!record.contains_key("c"));
    }

    #[test]
    fn equality_ignores_the_order_of_the_keys() {
        let forwards = record(&[("a", 1.0), ("b", 2.0)]);
        let backwards = record(&[("b", 2.0), ("a", 1.0)]);

        assert!(forwards == backwards);
        assert!(forwards != record(&[("a", 1.0), ("b", 3.0)]));
        assert!(forwards != record(&[("a", 1.0)]));
        assert!(forwards != record(&[("a", 1.0), ("c", 2.0)]));
    }

    #[test]
    fn entries_iterate_in_insertion_order() {
        let record = record(&[("z", 1.0), ("a", 2.0), ("m", 3.0)]);
        let entries: Vec<(&str, f64)> = record
            .iter()
            .map(|(key, value)| (key.as_str(), value.as_number().unwrap()))
            .collect();

        assert_eq!(entries, [("z", 1.0), ("a", 2.0), ("m", 3.0)]);
        assert_eq!(record.get_index(1).map(|(key, _)| key.as_str()), Some("a"));
        assert!(record.get_index(3).is_none());
    }

    #[test]
    fn an_index_map_becomes_a_record_with_the_same_entries() {
        let mut entries = IndexMap::new();
        entries.insert("one".to_string(), Value::Number(1.0));
        entries.insert("two".to_string(), Value::Bool(true));

        let record = Record::from(entries);

        assert_eq!(record.as_slice(), [Value::Number(1.0), Value::Bool(true)]);
        assert!(!record.is_empty());
        assert!(!record.shape().is_interned());
    }
}
