//! Records: ordered collections of values under string keys.
//!
//! A record is stored as its keys, in the order they were added, and one value per key. The
//! keys are in a *table* that records share: a record has as many keys as it has values, and
//! they are the first so many keys of its table.
//!
//! - Every record built by the same record literal shares one table, so creating it costs a
//!   single allocation for the values, and a field read that has seen the table before finds
//!   the field by position without looking at any key.
//! - A record that is made from another one and a key that the other one does not have (a
//!   spread followed by an entry) gets a table that starts with the keys of the other one.
//!   That is the table of the other one where it can be: the key goes into the room the
//!   table has left behind its last key, or is found there already when the same key was
//!   added to the same keys before. A table that cannot take the key remembers the table
//!   that was made for it. So a record that is built one key at a time has its keys copied
//!   once per doubling of their number, and the records that a piece of code builds over
//!   and over share the tables of the first ones.

use crate::values::Value;
use indexmap::IndexMap;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt,
    ptr::NonNull,
    rc::{Rc, Weak},
};

/// A key of a record. The tables that have a key share its characters.
pub type Key = Rc<str>;

/// Tables with room for at most this many keys are searched linearly, which beats hashing the
/// key, and so are the keys of a record that has at most this many.
const LINEAR_SEARCH_KEYS: usize = 8;

/// The fewest keys that a table has room for when it was made by adding a key to others.
const MIN_GROWING_CAPACITY: usize = 4;

/// The number of tables made from a table that the table remembers.
const KEPT_FORKS: usize = 8;

thread_local! {
    /// The canonical table of every key list that was interned on this thread.
    static INTERNED: RefCell<HashMap<Vec<Key>, Rc<Keys>>> = RefCell::new(HashMap::new());

    /// The canonical table without keys, which every record that is built key by key starts
    /// from.
    static EMPTY: Rc<Keys> = Keys::interned(std::iter::empty());
}

/// The keys of records, in the order they were added.
///
/// Keys are only ever added to a table, behind the ones it has, and only where it has room
/// for them: the keys that are there stay where they are for as long as the table lives. A
/// record that has the first so many keys of a table therefore has them whatever is added.
/// No key is in a table twice.
pub struct Keys {
    /// The first key. `filled` keys are initialised, and there is room for `capacity`.
    start: NonNull<Key>,
    capacity: usize,
    filled: Cell<usize>,

    /// The number of keys that the room was allocated for, which is at least `capacity`.
    allocated: usize,

    /// Where the keys are, for tables with room for more than [`LINEAR_SEARCH_KEYS`]: the
    /// upper half of a slot is the upper half of the hash of a key, the lower half is the
    /// position of the key plus one. A key is in the first free slot from the one its hash
    /// picks, going around at the end; at most half of the slots are ever taken.
    slots: Box<[Cell<u64>]>,

    /// Whether this is the canonical table of its key list (see [`Keys::interned`]). Such a
    /// table has no room for more keys, so it stays the table of that list.
    interned: bool,

    /// The tables that were made from the first keys of this one and a key that could not
    /// be added to it, most recent last.
    forks: RefCell<Vec<Fork>>,
}

/// A table that was made from another one.
struct Fork {
    /// The number of keys that the table has in common with the one it was made from. The
    /// key behind them is the one that was added.
    shared: usize,
    table: Weak<Keys>,
}

impl Keys {
    /// A table without keys that has room for `capacity` of them.
    fn with_room(capacity: usize, interned: bool) -> Self {
        assert!(capacity < u32::MAX as usize, "a position fits half a slot");

        let mut room: Vec<Key> = Vec::with_capacity(capacity);
        let allocated = room.capacity();
        // SAFETY: the pointer to the elements of a vector is never null.
        let start = unsafe { NonNull::new_unchecked(room.as_mut_ptr()) };
        std::mem::forget(room);

        let slots = match capacity > LINEAR_SEARCH_KEYS {
            true => vec![Cell::new(0); (capacity * 2).next_power_of_two()],
            false => Vec::new(),
        };

        Self {
            start,
            capacity,
            filled: Cell::new(0),
            allocated,
            slots: slots.into_boxed_slice(),
            interned,
            forks: RefCell::new(Vec::new()),
        }
    }

    /// The canonical table of `keys`, which are distinct: the same one for every call with
    /// an equal list of keys. A canonical table stays alive for as long as its thread does.
    pub fn interned<'k>(keys: impl Iterator<Item = &'k str>) -> Rc<Keys> {
        // The lists are few and short, and only looked up when code is compiled, so a list
        // of keys is made to look a list up by.
        let list: Vec<Key> = keys.map(Key::from).collect();

        INTERNED.with_borrow_mut(|tables| {
            if let Some(table) = tables.get(&list) {
                return Rc::clone(table);
            }

            let table = Keys::with_room(list.len(), true);
            for key in &list {
                table.push(Rc::clone(key));
            }

            let table = Rc::new(table);
            tables.insert(list, Rc::clone(&table));

            table
        })
    }

    /// The canonical table without keys.
    pub fn empty() -> Rc<Keys> {
        EMPTY.with(Rc::clone)
    }

    pub fn is_interned(&self) -> bool {
        self.interned
    }

    /// The number of keys in the table.
    pub fn len(&self) -> usize {
        self.filled.get()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The first `count` keys, which the table has to have.
    #[inline]
    pub fn first(&self, count: usize) -> &[Key] {
        assert!(
            count <= self.filled.get(),
            "a table has the keys it is asked for"
        );

        // SAFETY: the first `filled` keys are initialised, and nothing writes to them for as
        // long as the table lives.
        unsafe { std::slice::from_raw_parts(self.start.as_ptr(), count) }
    }

    /// The key at `position`.
    #[inline]
    pub fn key(&self, position: usize) -> Option<&Key> {
        self.first(self.filled.get()).get(position)
    }

    /// The position of `key` among the first `count` keys, which the table has to have.
    #[inline]
    pub fn position(&self, count: usize, key: &str) -> Option<usize> {
        if count <= LINEAR_SEARCH_KEYS {
            return self.first(count).iter().position(|own| **own == *key);
        }

        self.find(key).filter(|position| *position < count)
    }

    /// The position of `key` in a table that has slots.
    fn find(&self, key: &str) -> Option<usize> {
        let hash = hash_key(key);
        let mask = self.slots.len() - 1;
        let mut slot = hash as usize & mask;

        loop {
            let entry = self.slots[slot].get();
            if entry == 0 {
                return None;
            }

            if entry >> 32 == hash >> 32 {
                let position = (entry as u32 - 1) as usize;
                // SAFETY: a slot holds the position of a key that the table has.
                if **unsafe { &*self.start.as_ptr().add(position) } == *key {
                    return Some(position);
                }
            }

            slot = (slot + 1) & mask;
        }
    }

    /// Add `key`, which the table has room for and does not have yet.
    fn push(&self, key: Key) {
        let position = self.filled.get();
        assert!(
            position < self.capacity,
            "a table has room for the key it is given"
        );

        if !self.slots.is_empty() {
            let hash = hash_key(&key);
            let mask = self.slots.len() - 1;
            let mut slot = hash as usize & mask;
            while self.slots[slot].get() != 0 {
                slot = (slot + 1) & mask;
            }

            self.slots[slot].set((hash >> 32) << 32 | (position as u64 + 1));
        }

        // SAFETY: the table has room for a key at `position`, which is behind every key
        // that anything reads.
        unsafe { self.start.as_ptr().add(position).write(key) };
        self.filled.set(position + 1);
    }

    /// The table that has the first `count` keys of this one and then `key`, which is not
    /// one of them. It is this table when the key is where it has to be or can be put there.
    pub fn followed_by(self: &Rc<Self>, count: usize, key: &str) -> Rc<Keys> {
        let filled = self.filled.get();
        assert!(
            count <= filled,
            "a table has the keys that a key is added to"
        );

        if count < filled {
            // SAFETY: the table has a key at `count`.
            if **unsafe { &*self.start.as_ptr().add(count) } == *key {
                return Rc::clone(self);
            }
        } else if filled < self.capacity {
            self.push(Key::from(key));
            return Rc::clone(self);
        }

        let mut forks = self.forks.borrow_mut();
        forks.retain(|fork| fork.table.strong_count() > 0);

        for fork in forks.iter() {
            if fork.shared == count
                && let Some(table) = fork.table.upgrade()
                && table.key(count).is_some_and(|added| **added == *key)
            {
                return table;
            }
        }

        // The table is likely to be added to again, so it gets room for as many keys as
        // it has: a record that grows key by key has its keys copied once per doubling.
        let capacity = ((count + 1) * 2).max(MIN_GROWING_CAPACITY);
        let table = Keys::with_room(capacity, false);
        for shared in self.first(count) {
            table.push(Rc::clone(shared));
        }
        table.push(Key::from(key));
        let table = Rc::new(table);

        if forks.len() == KEPT_FORKS {
            forks.remove(0);
        }
        forks.push(Fork {
            shared: count,
            table: Rc::downgrade(&table),
        });

        table
    }
}

impl Drop for Keys {
    fn drop(&mut self) {
        // SAFETY: the keys were allocated as a vector with this capacity, of which the
        // first `filled` are initialised, and nothing refers to them any more.
        drop(unsafe {
            Vec::from_raw_parts(self.start.as_ptr(), self.filled.get(), self.allocated)
        });
    }
}

impl fmt::Debug for Keys {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.first(self.len())).finish()
    }
}

/// The hash of a key: every character counts, eight at a time.
pub(crate) fn hash_key(key: &str) -> u64 {
    let bytes = key.as_bytes();
    let mut hash = bytes.len() as u64;

    let mut words = bytes.chunks_exact(8);
    for word in &mut words {
        let word = u64::from_le_bytes(word.try_into().expect("a chunk of eight bytes"));
        hash = (hash.rotate_left(5) ^ word).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }

    let rest = words.remainder();
    if !rest.is_empty() {
        let mut word = [0; 8];
        word[..rest.len()].copy_from_slice(rest);
        hash = (hash.rotate_left(5) ^ u64::from_le_bytes(word)).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }

    // Spread every bit over all of the hash (the finalizer of MurmurHash3): the lower bits
    // pick the slot, the upper ones tell keys in the same slot apart.
    hash ^= hash >> 33;
    hash = hash.wrapping_mul(0xFF51_AFD7_ED55_8CCD);
    hash ^= hash >> 33;
    hash = hash.wrapping_mul(0xC4CE_B9FE_1A85_EC53);
    hash ^= hash >> 33;

    hash
}

/// A collection of values under string keys that keeps the order the keys were added in.
#[derive(Debug, Clone)]
pub struct Record {
    /// The table that starts with the keys of the record.
    keys: Rc<Keys>,

    /// One value per key, in the same order.
    values: Box<[Value]>,
}

impl Record {
    /// A record with the first keys of `keys`: one for each of the `values`, in the same
    /// order.
    pub fn new(keys: Rc<Keys>, values: Box<[Value]>) -> Self {
        assert!(values.len() <= keys.len(), "a record has one key per value");

        Self { keys, values }
    }

    /// The table that starts with the keys of the record.
    pub fn table(&self) -> &Rc<Keys> {
        &self.keys
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// The position of the entry under `key`.
    #[inline]
    pub fn position(&self, key: &str) -> Option<usize> {
        self.keys.position(self.values.len(), key)
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.position(key).map(|position| &self.values[position])
    }

    /// The position, key and value of the entry under `key`.
    pub fn get_full(&self, key: &str) -> Option<(usize, &Key, &Value)> {
        let position = self.position(key)?;
        let (key, value) = self.get_index(position)?;

        Some((position, key, value))
    }

    /// The entry at `position`.
    #[inline]
    pub fn get_index(&self, position: usize) -> Option<(&Key, &Value)> {
        let value = self.values.get(position)?;

        Some((&self.keys.first(self.values.len())[position], value))
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.position(key).is_some()
    }

    /// The keys, in the order they were added.
    pub fn keys(&self) -> std::slice::Iter<'_, Key> {
        self.keys.first(self.values.len()).iter()
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
        self.keys().zip(self.values.iter())
    }
}

pub type Iter<'r> = std::iter::Zip<std::slice::Iter<'r, Key>, std::slice::Iter<'r, Value>>;

impl<'r> IntoIterator for &'r Record {
    type Item = (&'r Key, &'r Value);
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

impl<K: AsRef<str>> FromIterator<(K, Value)> for Record {
    fn from_iter<I: IntoIterator<Item = (K, Value)>>(entries: I) -> Self {
        let mut builder = RecordBuilder::default();
        for (key, value) in entries {
            builder.insert(key.as_ref(), value);
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
#[derive(Debug)]
pub struct RecordBuilder {
    /// The table that starts with the keys of the entries so far.
    keys: Rc<Keys>,
    values: Vec<Value>,
}

impl Default for RecordBuilder {
    fn default() -> Self {
        Self {
            keys: Keys::empty(),
            values: Vec::new(),
        }
    }
}

impl RecordBuilder {
    /// A builder that is going to get about `entries` entries.
    pub fn with_capacity(entries: usize) -> Self {
        Self {
            keys: Keys::empty(),
            values: Vec::with_capacity(entries),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Add an entry. A key that is already present keeps its position and gets the new value.
    pub fn insert(&mut self, key: &str, value: Value) {
        match self.keys.position(self.values.len(), key) {
            Some(position) => self.values[position] = value,
            None => {
                self.keys = self.keys.followed_by(self.values.len(), key);
                self.values.push(value);
            }
        }
    }

    /// Add the entries of `record`, in their order.
    pub fn extend(&mut self, record: &Record) {
        // The entries of the first record are the entries so far, under the keys of its
        // table.
        if self.values.is_empty() {
            self.keys = Rc::clone(&record.keys);
            self.values.extend_from_slice(&record.values);
            return;
        }

        for (key, value) in record {
            self.insert(key, *value);
        }
    }

    pub fn finish(self) -> Record {
        Record::new(self.keys, self.values.into_boxed_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn record(entries: &[(&str, f64)]) -> Record {
        entries
            .iter()
            .map(|(key, number)| (*key, Value::Number(*number)))
            .collect()
    }

    fn names(record: &Record) -> Vec<&str> {
        record.keys().map(|key| &**key).collect()
    }

    /// The names `prefix0`, `prefix1`, and so on.
    fn numbered(prefix: &str, count: usize) -> Vec<String> {
        (0..count).map(|index| format!("{prefix}{index}")).collect()
    }

    #[test]
    fn interned_tables_are_shared_per_key_list() {
        let first = Keys::interned(["x", "y"].into_iter());
        let again = Keys::interned(["x", "y"].into_iter());
        let reordered = Keys::interned(["y", "x"].into_iter());

        assert!(Rc::ptr_eq(&first, &again));
        assert!(!Rc::ptr_eq(&first, &reordered));
        assert!(first.is_interned());
        assert!(Rc::ptr_eq(&Keys::empty(), &Keys::interned([].into_iter())));
        assert!(Keys::empty().is_empty());
    }

    #[test]
    fn positions_follow_insertion_order_for_small_and_large_tables() {
        let all = numbered("key", 40);

        for count in [0, 1, LINEAR_SEARCH_KEYS, LINEAR_SEARCH_KEYS + 1, 40] {
            let table = Keys::interned(all[..count].iter().map(String::as_str));

            assert_eq!(table.len(), count);
            for (position, name) in all[..count].iter().enumerate() {
                assert_eq!(table.position(count, name), Some(position));
                assert_eq!(table.key(position).map(|key| &**key), Some(name.as_str()));
            }
            assert_eq!(table.position(count, "missing"), None);
            assert!(table.key(count).is_none());
        }
    }

    #[test]
    fn a_key_is_only_found_among_the_keys_that_are_asked_for() {
        let all = numbered("key", 40);
        let table = Keys::interned(all.iter().map(String::as_str));

        for count in [0, 1, 5, LINEAR_SEARCH_KEYS, LINEAR_SEARCH_KEYS + 1, 20, 40] {
            for (position, name) in all.iter().enumerate() {
                let expected = (position < count).then_some(position);
                assert_eq!(table.position(count, name), expected, "{name} in {count}");
            }
        }
    }

    #[test]
    fn keys_that_hash_to_the_same_slot_are_told_apart() {
        // Many more keys than a table of this size has slots for one probe each.
        let all = numbered("", 3000);
        let table = Keys::interned(all.iter().map(String::as_str));

        for (position, name) in all.iter().enumerate() {
            assert_eq!(table.position(3000, name), Some(position));
        }
        for name in numbered("x", 3000) {
            assert_eq!(table.position(3000, &name), None);
        }
    }

    #[test]
    fn repeated_keys_keep_their_position_and_take_the_last_value() {
        let record = record(&[("a", 1.0), ("b", 2.0), ("a", 3.0)]);

        assert_eq!(record.len(), 2);
        assert_eq!(names(&record), ["a", "b"]);
        assert_eq!(record.get("a"), Some(&Value::Number(3.0)));
        assert_eq!(
            record.get_full("b"),
            Some((1, &Key::from("b"), &Value::Number(2.0)))
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
            .map(|(key, value)| (&**key, value.as_number().unwrap()))
            .collect();

        assert_eq!(entries, [("z", 1.0), ("a", 2.0), ("m", 3.0)]);
        assert_eq!(record.get_index(1).map(|(key, _)| &**key), Some("a"));
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
        assert!(!record.table().is_interned());
    }

    #[test]
    fn records_with_the_same_keys_in_the_same_order_share_a_table() {
        let first = record(&[("share-a", 1.0), ("share-b", 2.0), ("share-c", 3.0)]);
        let second = record(&[("share-a", 4.0), ("share-b", 5.0), ("share-c", 6.0)]);
        let shorter = record(&[("share-a", 7.0), ("share-b", 8.0)]);
        let other = record(&[("share-a", 7.0), ("share-x", 8.0)]);

        assert!(Rc::ptr_eq(first.table(), second.table()));
        assert!(Rc::ptr_eq(first.table(), shorter.table()));
        assert!(!Rc::ptr_eq(first.table(), other.table()));

        // A record has the keys it was given, whatever else its table has.
        assert_eq!(names(&shorter), ["share-a", "share-b"]);
        assert_eq!(shorter.get("share-c"), None);
        assert_eq!(shorter.get_index(2), None);
        assert_eq!(names(&other), ["share-a", "share-x"]);
        assert_eq!(other.get("share-b"), None);
        assert_eq!(first.get("share-c"), Some(&Value::Number(3.0)));
    }

    #[test]
    fn a_table_that_is_added_to_keeps_the_keys_of_the_records_that_have_it() {
        let base = record(&[("grow-a", 1.0)]);
        let mut records = vec![base];

        // Each record has the keys of the one before it and one more.
        for index in 0..100 {
            let mut builder = RecordBuilder::default();
            builder.extend(records.last().unwrap());
            builder.insert(&format!("grow-{index}"), Value::Number(index as f64));
            records.push(builder.finish());
        }

        for (count, record) in records.iter().enumerate() {
            assert_eq!(record.len(), count + 1);
            assert_eq!(record.get("grow-a"), Some(&Value::Number(1.0)));

            for index in 0..100 {
                let expected = (index < count).then_some(Value::Number(index as f64));
                assert_eq!(record.get(&format!("grow-{index}")).copied(), expected);
            }
        }

        // The keys were copied once per doubling, not once per record.
        let mut tables: Vec<&Rc<Keys>> = Vec::new();
        for record in &records {
            if !tables.iter().any(|table| Rc::ptr_eq(table, record.table())) {
                tables.push(record.table());
            }
        }
        assert!(tables.len() <= 8, "{} tables", tables.len());
    }

    #[test]
    fn records_that_add_different_keys_to_the_same_keys_get_tables_of_their_own() {
        let base = record(&[("fork-a", 1.0), ("fork-b", 2.0)]);
        let with = |key: &str| {
            let mut builder = RecordBuilder::default();
            builder.extend(&base);
            builder.insert(key, Value::Number(3.0));
            builder.finish()
        };

        let first = with("fork-c");
        let second = with("fork-d");
        let third = with("fork-c");
        let fourth = with("fork-d");

        assert_eq!(names(&first), ["fork-a", "fork-b", "fork-c"]);
        assert_eq!(names(&second), ["fork-a", "fork-b", "fork-d"]);
        assert_eq!(names(&base), ["fork-a", "fork-b"]);
        assert!(Rc::ptr_eq(first.table(), third.table()));
        assert!(Rc::ptr_eq(second.table(), fourth.table()));
        assert!(!Rc::ptr_eq(first.table(), second.table()));
        assert_eq!(first.get("fork-d"), None);
        assert_eq!(second.get("fork-c"), None);
    }

    #[test]
    fn a_table_forgets_the_tables_that_no_record_has_any_more() {
        let base = record(&[("forget-a", 1.0)]);
        let with = |key: &str| {
            let mut builder = RecordBuilder::default();
            builder.extend(&base);
            builder.insert("forget-full-1", Value::Null);
            builder.insert("forget-full-2", Value::Null);
            builder.insert("forget-full-3", Value::Null);
            builder.insert(key, Value::Number(3.0));
            builder.finish()
        };

        // More tables than a table remembers, each of which is gone before the next.
        for index in 0..KEPT_FORKS * 3 {
            let record = with(&format!("forget-{index}"));
            assert_eq!(record.len(), 5);
            assert_eq!(
                record.get(&format!("forget-{index}")),
                Some(&Value::Number(3.0))
            );
        }

        // And as many that are all still there.
        let kept: Vec<Record> = (0..KEPT_FORKS * 3)
            .map(|index| with(&format!("forget-{index}")))
            .collect();
        for (index, record) in kept.iter().enumerate() {
            assert_eq!(record.len(), 5);
            assert_eq!(
                names(record)[4],
                format!("forget-{index}"),
                "{:?}",
                names(record)
            );
        }
    }

    #[test]
    fn the_first_record_that_a_builder_is_given_keeps_its_table() {
        let literal = Record::new(
            Keys::interned(["x", "y"].into_iter()),
            vec![Value::Number(1.0), Value::Number(2.0)].into_boxed_slice(),
        );

        let mut builder = RecordBuilder::with_capacity(2);
        assert!(builder.is_empty());
        builder.extend(&literal);
        builder.insert("y", Value::Number(3.0));
        builder.insert("x", Value::Number(4.0));
        let updated = builder.finish();

        assert!(Rc::ptr_eq(updated.table(), literal.table()));
        assert_eq!(updated.as_slice(), [Value::Number(4.0), Value::Number(3.0)]);
        assert_eq!(literal.as_slice(), [Value::Number(1.0), Value::Number(2.0)]);

        // A record that comes second is added entry by entry.
        let mut builder = RecordBuilder::default();
        builder.insert("y", Value::Number(5.0));
        builder.extend(&literal);
        let merged = builder.finish();

        assert_eq!(names(&merged), ["y", "x"]);
        assert_eq!(merged.as_slice(), [Value::Number(2.0), Value::Number(1.0)]);
    }
}
