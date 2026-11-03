//! The distinct values of a list (`unique`).
//!
//! Whether a value is equal to one that came before it is found by a hash of what makes
//! values equal, so that a list is gone through in time proportional to its length, not to
//! its length times the number of distinct values in it.

use crate::heap::{Heap, HeapPointer, IterablePointer};
use crate::values::Value;
use anyhow::Result;

/// Up to this many distinct values are compared with one another directly, which is cheaper
/// than hashing them.
const COMPARED_DIRECTLY: usize = 8;

/// A hash takes in what is at most this many lists and records deep in a value. What is
/// deeper only counts by its kind and its length, so that a hash never goes deeper into a
/// value than the stack has room for, where comparing two values stops at their first
/// difference.
const HASHED_DEPTH: usize = 8;

const NULL: u64 = 0x6e75_6c6c;
const BOOL: u64 = 0x626f_6f6c;
const STRING: u64 = 0x7374_7269_6e67;
const LIST: u64 = 0x6c69_7374;
const RECORD: u64 = 0x7265_636f_7264;
const LAMBDA: u64 = 0x6c61_6d62_6461;
const BUILT_IN: u64 = 0x0062_7569_6c74_696e;
const SPREAD: u64 = 0x7370_7265_6164;

/// The values of `items` without those that are equal to one before them.
pub(crate) fn distinct(items: &[Value], heap: &Heap) -> Result<Vec<Value>> {
    let mut kept = Kept::default();

    for item in items {
        kept.keep(*item, heap)?;
    }

    Ok(kept.values)
}

/// The values that were kept, and where to find the ones that hash to a given number.
#[derive(Default)]
struct Kept {
    /// The values, in the order they were kept in.
    values: Vec<Value>,

    /// The hash of each value, once there are enough of them to look them up by it. A value
    /// that is equal to nothing has none, and is never looked for.
    hashes: Vec<Option<u64>>,

    /// One more than the position of a value in `values`, or zero where there is none. A
    /// value is in the first free slot from the one its hash picks, going around at the end.
    /// The number of slots is a power of two, and at least half of them are free.
    slots: Vec<usize>,
}

impl Kept {
    /// Keep `item`, unless a value that is equal to it was kept before.
    fn keep(&mut self, item: Value, heap: &Heap) -> Result<()> {
        if self.slots.is_empty() {
            for kept in &self.values {
                if item.equals(kept, heap)? {
                    return Ok(());
                }
            }

            self.values.push(item);

            if self.values.len() > COMPARED_DIRECTLY {
                self.hashes = self
                    .values
                    .iter()
                    .map(|kept| hash(kept, heap))
                    .collect::<Result<_>>()?;
                self.place_all(4 * COMPARED_DIRECTLY);
            }

            return Ok(());
        }

        let hash = hash(&item, heap)?;
        let mut free = None;

        if let Some(hash) = hash {
            let mut slot = self.slot(hash);
            while let Some(position) = self.slots[slot].checked_sub(1) {
                if self.hashes[position] == Some(hash)
                    && item.equals(&self.values[position], heap)?
                {
                    return Ok(());
                }
                slot = (slot + 1) & (self.slots.len() - 1);
            }

            free = Some(slot);
        }

        self.values.push(item);
        self.hashes.push(hash);

        if 2 * self.values.len() > self.slots.len() {
            self.place_all(2 * self.slots.len());
        } else if let Some(free) = free {
            self.slots[free] = self.values.len();
        }

        Ok(())
    }

    /// The slot that `hash` picks.
    fn slot(&self, hash: u64) -> usize {
        hash as usize & (self.slots.len() - 1)
    }

    /// Start over with `count` slots, and put every value that has a hash in the first free
    /// one from the slot that its hash picks.
    fn place_all(&mut self, count: usize) {
        self.slots.clear();
        self.slots.resize(count, 0);

        for position in 0..self.values.len() {
            let Some(hash) = self.hashes[position] else {
                continue;
            };

            let mut slot = self.slot(hash);
            while self.slots[slot] != 0 {
                slot = (slot + 1) & (self.slots.len() - 1);
            }

            self.slots[slot] = position + 1;
        }
    }
}

/// A number that is the same for values that are equal (see [`Value::equals`]), and that is
/// almost always different for values that are not. A value that is known to be equal to
/// nothing, not even to itself, has none: that is one with a number in it that is not a
/// number, no deeper than a hash takes in.
fn hash(value: &Value, heap: &Heap) -> Result<Option<u64>> {
    hash_within(value, heap, HASHED_DEPTH)
}

/// The hash of `value`, taking in what is at most `depth` lists and records deep in it.
fn hash_within(value: &Value, heap: &Heap, depth: usize) -> Result<Option<u64>> {
    Ok(Some(match value {
        Value::Number(number) if number.is_nan() => return Ok(None),
        // Zero is equal to its negative.
        Value::Number(number) if *number == 0.0 => mix(0),
        Value::Number(number) => mix(number.to_bits()),
        Value::Bool(bool) => mix(BOOL + u64::from(*bool)),
        Value::Null => mix(NULL),
        Value::String(pointer) => hash_string(pointer.reify(heap).as_string()?),
        Value::List(pointer) => {
            let items = pointer.reify(heap).as_list()?;

            let mut list = LIST ^ items.len() as u64;
            if let Some(depth) = depth.checked_sub(1) {
                for item in items {
                    let Some(item) = hash_within(item, heap, depth)? else {
                        return Ok(None);
                    };
                    list = mix(list.rotate_left(5) ^ item);
                }
            }

            mix(list)
        }
        Value::Record(pointer) => {
            let record = pointer.reify(heap).as_record()?;

            // Records are equal whatever order their keys are in, so the entries are added
            // up, which comes to the same in any order.
            let mut entries = record.len() as u64;
            if let Some(depth) = depth.checked_sub(1) {
                for (key, value) in record.iter() {
                    let Some(value) = hash_within(value, heap, depth)? else {
                        return Ok(None);
                    };
                    entries = entries.wrapping_add(mix(hash_string(key) ^ value.rotate_left(32)));
                }
            }

            mix(RECORD ^ entries)
        }
        // Functions are equal when they are written the same way. The number of their
        // parameters stands in for the rest: there is no telling two bodies apart by a
        // number.
        Value::Lambda(pointer) => {
            let lambda = pointer.reify(heap).as_lambda()?;

            mix(LAMBDA + lambda.args.len() as u64)
        }
        Value::BuiltIn(function) => mix(BUILT_IN + *function as u64),
        Value::Spread(pointer) => {
            let spread = match pointer {
                IterablePointer::List(pointer) => Value::List(*pointer),
                IterablePointer::String(pointer) => Value::String(*pointer),
                IterablePointer::Record(pointer) => Value::Record(*pointer),
            };
            let Some(spread) = hash_within(&spread, heap, depth)? else {
                return Ok(None);
            };

            mix(SPREAD ^ spread)
        }
    }))
}

fn hash_string(string: &str) -> u64 {
    let bytes = string.as_bytes();
    let mut hash = STRING ^ bytes.len() as u64;

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

    mix(hash)
}

/// Spread every bit of `bits` over all of the result (the finalizer of MurmurHash3), so that
/// numbers that differ in a few bits anywhere pick different slots.
fn mix(mut bits: u64) -> u64 {
    bits ^= bits >> 33;
    bits = bits.wrapping_mul(0xFF51_AFD7_ED55_8CCD);
    bits ^= bits >> 33;
    bits = bits.wrapping_mul(0xC4CE_B9FE_1A85_EC53);
    bits ^= bits >> 33;

    bits
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::functions::BuiltInFunction;
    use crate::record::Record;

    /// What `unique` did before it hashed anything: compare each value with all it kept.
    fn compared_with_each(items: &[Value], heap: &Heap) -> Vec<Value> {
        let mut kept: Vec<Value> = Vec::new();
        for item in items {
            if !kept.iter().any(|kept| item.equals(kept, heap).unwrap()) {
                kept.push(*item);
            }
        }

        kept
    }

    fn record(heap: &mut Heap, entries: &[(&str, Value)]) -> Value {
        let record: Record = entries
            .iter()
            .map(|(key, value)| (key.to_string(), *value))
            .collect();

        heap.insert_record(record)
    }

    /// Values of every kind, many of which are equal to another without being the same.
    fn assorted(heap: &mut Heap) -> Vec<Value> {
        let mut values = vec![
            Value::Null,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(0.0),
            Value::Number(-0.0),
            Value::Number(1.0),
            Value::Number(-1.0),
            Value::Number(0.1 + 0.2),
            Value::Number(0.3),
            Value::Number(1e300),
            Value::Number(f64::INFINITY),
            Value::Number(f64::NEG_INFINITY),
            Value::Number(f64::NAN),
            Value::Number(-f64::NAN),
            Value::Number(f64::MIN_POSITIVE),
            Value::Number(5e-324),
            Value::Number(9_007_199_254_740_992.0),
            Value::Number(9_007_199_254_740_993.0),
            Value::BuiltIn(BuiltInFunction::Sqrt),
            Value::BuiltIn(BuiltInFunction::Sin),
            Value::BuiltIn(BuiltInFunction::Unique),
        ];

        for number in 0..40 {
            values.push(Value::Number(f64::from(number % 13)));
            values.push(Value::Number(f64::from(number) * 0.5));
        }

        for text in [
            "",
            "a",
            "b",
            "ab",
            "ba",
            "abcdefg",
            "abcdefgh",
            "abcdefghi",
            "abcdefgh\0",
            "0",
            "1",
            "true",
            "null",
            "é",
            "e\u{301}",
            "日本語",
            "a longer string that takes a few words",
            "a longer string that takes a few words!",
        ] {
            values.push(heap.insert_string(text.to_string()));
            values.push(heap.insert_string(text.to_string()));
        }

        let (one, two, nan) = (
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(f64::NAN),
        );
        let (a, b) = (
            heap.insert_string("a".to_string()),
            heap.insert_string("a".to_string()),
        );

        for items in [
            vec![],
            vec![],
            vec![one],
            vec![one],
            vec![two],
            vec![one, two],
            vec![two, one],
            vec![one, two],
            vec![one, one],
            vec![Value::Null],
            vec![Value::Bool(false)],
            vec![Value::Number(0.0)],
            vec![Value::Number(-0.0)],
            vec![nan],
            vec![nan],
            vec![one, nan],
            vec![a],
            vec![b],
            vec![a, b],
            vec![b, a],
        ] {
            values.push(heap.insert_list(items));
        }

        let inner = heap.insert_list(vec![one, two]);
        let same = heap.insert_list(vec![one, two]);
        let empty = heap.insert_list(vec![]);
        for items in [
            vec![inner],
            vec![same],
            vec![inner, same],
            vec![empty],
            vec![empty, empty],
            vec![inner, empty],
        ] {
            values.push(heap.insert_list(items));
        }

        let entries: [&[(&str, Value)]; 14] = [
            &[],
            &[],
            &[("a", one)],
            &[("a", one)],
            &[("a", two)],
            &[("b", one)],
            &[("a", one), ("b", two)],
            &[("b", two), ("a", one)],
            &[("a", two), ("b", one)],
            &[("a", one), ("b", one)],
            &[("b", one), ("a", one)],
            &[("a", nan)],
            &[("a", inner), ("b", a)],
            &[("b", b), ("a", same)],
        ];
        for entries in entries {
            values.push(record(heap, entries));
        }

        let nested = record(heap, &[("x", one)]);
        let again = record(heap, &[("x", one)]);
        values.push(heap.insert_list(vec![nested]));
        values.push(heap.insert_list(vec![again]));
        values.push(record(heap, &[("r", nested), ("l", inner)]));
        values.push(record(heap, &[("l", same), ("r", again)]));

        values
    }

    /// The lambdas that `code` defines, one per line.
    fn lambdas(heap: &std::rc::Rc<std::cell::RefCell<Heap>>, code: &[&str]) -> Vec<Value> {
        use crate::environment::Environment;
        use crate::expressions::evaluate_ast;
        use crate::fast_parser::{Statement, parse_program};
        use std::rc::Rc;

        let bindings = Rc::new(Environment::new());

        code.iter()
            .map(|code| {
                let Some(Statement::Expression(expr)) = parse_program(code).unwrap().pop() else {
                    panic!("an expression: {code}");
                };

                evaluate_ast(
                    &expr,
                    Rc::clone(heap),
                    Rc::clone(&bindings),
                    0,
                    (*code).into(),
                )
                .unwrap()
            })
            .collect()
    }

    fn hash(value: &Value, heap: &Heap) -> Option<u64> {
        super::hash(value, heap).unwrap()
    }

    #[test]
    fn equal_values_have_the_same_hash() {
        let mut heap = Heap::new();
        let values = assorted(&mut heap);

        let mut equal = 0;
        for one in &values {
            for other in &values {
                if one.equals(other, &heap).unwrap() {
                    equal += 1;
                    assert_eq!(
                        hash(one, &heap),
                        hash(other, &heap),
                        "{} and {}",
                        one.stringify_internal(&heap),
                        other.stringify_internal(&heap)
                    );
                    assert!(hash(one, &heap).is_some());
                }
            }
        }

        // Well over the number of values: many are equal to more than themselves.
        assert!(equal > 2 * values.len(), "{equal} equal pairs");
    }

    #[test]
    fn a_value_without_a_hash_is_equal_to_nothing() {
        let mut heap = Heap::new();
        let values = assorted(&mut heap);

        let mut without = 0;
        for one in &values {
            if hash(one, &heap).is_none() {
                without += 1;
                for other in &values {
                    assert!(!one.equals(other, &heap).unwrap());
                    assert!(!other.equals(one, &heap).unwrap());
                }
            }
        }

        assert!(without >= 6, "{without} values without a hash");
    }

    #[test]
    fn values_that_differ_almost_always_have_different_hashes() {
        let mut heap = Heap::new();
        let values = assorted(&mut heap);

        let mut clashes = 0;
        for one in &values {
            for other in &values {
                if !one.equals(other, &heap).unwrap()
                    && hash(one, &heap).is_some()
                    && hash(one, &heap) == hash(other, &heap)
                {
                    clashes += 1;
                }
            }
        }

        assert_eq!(clashes, 0);
    }

    #[test]
    fn functions_that_are_written_the_same_way_have_the_same_hash() {
        let heap = std::rc::Rc::new(std::cell::RefCell::new(Heap::new()));
        let functions = lambdas(
            &heap,
            &[
                "x => x + 1",
                "x => x + 1",
                "x => x + 2",
                "y => y + 1",
                "(x, y) => x + y",
                "(x, y) => x + y",
                "(x, y) => x * y",
                "() => 1",
            ],
        );
        let heap = heap.borrow();

        let mut equal = 0;
        for one in &functions {
            for other in &functions {
                if one.equals(other, &heap).unwrap() {
                    equal += 1;
                    assert_eq!(hash(one, &heap), hash(other, &heap));
                }
            }
        }
        assert_eq!(equal, functions.len() + 4);

        assert_eq!(
            distinct(&functions, &heap).unwrap(),
            compared_with_each(&functions, &heap)
        );
        assert_eq!(distinct(&functions, &heap).unwrap().len(), 6);
    }

    #[test]
    fn the_distinct_values_are_the_ones_that_comparing_with_each_finds() {
        let mut heap = Heap::new();
        let values = assorted(&mut heap);

        // Every rotation puts other values first, and so keeps other ones of those that
        // are equal.
        for start in 0..values.len() {
            let mut items = values[start..].to_vec();
            items.extend_from_slice(&values[..start]);
            items.extend_from_slice(&values);

            let found = distinct(&items, &heap).unwrap();
            let expected = compared_with_each(&items, &heap);

            assert_eq!(found.len(), expected.len(), "from {start}");
            for (found, expected) in found.iter().zip(&expected) {
                // The very same value, not just an equal one.
                assert_eq!(
                    format!("{found:?}"),
                    format!("{expected:?}"),
                    "from {start}"
                );
            }
        }
    }

    /// `value` in `depth` lists, each in the next.
    fn nested(heap: &mut Heap, value: Value, depth: usize) -> Value {
        (0..depth).fold(value, |inner, _| heap.insert_list(vec![inner]))
    }

    #[test]
    fn values_that_differ_deeper_than_a_hash_takes_in_are_told_apart() {
        let mut heap = Heap::new();

        let mut items = Vec::new();
        for _ in 0..3 {
            for number in 0..40 {
                items.push(nested(
                    &mut heap,
                    Value::Number(f64::from(number)),
                    HASHED_DEPTH + 3,
                ));
            }
            // These are equal to nothing, however deep what makes them so is.
            items.push(nested(&mut heap, Value::Number(f64::NAN), HASHED_DEPTH + 3));
            items.push(nested(&mut heap, Value::Number(f64::NAN), HASHED_DEPTH - 1));
        }

        let hashes: Vec<_> = items.iter().map(|item| hash(item, &heap)).collect();
        assert_eq!(hashes[0], hashes[1]);
        assert_eq!(hashes[0], hashes[40]);
        assert_eq!(hashes[41], None);

        let found = distinct(&items, &heap).unwrap();
        assert_eq!(found, compared_with_each(&items, &heap));
        assert_eq!(found.len(), 40 + 3 * 2);
    }

    #[test]
    fn a_hash_goes_no_deeper_than_the_stack_has_room_for() {
        let mut heap = Heap::new();

        // Deeper than a comparison of two of these that are equal could go, but they differ
        // right at the top.
        let deep = nested(&mut heap, Value::Null, 500_000);
        let items: Vec<Value> = (0..3 * COMPARED_DIRECTLY)
            .map(|number| heap.insert_list(vec![Value::Number(number as f64), deep]))
            .collect();

        assert_eq!(distinct(&items, &heap).unwrap(), items);

        let records: Vec<Value> = (0..3 * COMPARED_DIRECTLY)
            .map(|number| {
                record(
                    &mut heap,
                    &[("number", Value::Number(number as f64)), ("deep", deep)],
                )
            })
            .collect();

        assert_eq!(distinct(&records, &heap).unwrap(), records);
    }

    #[test]
    fn a_few_values_are_told_apart_without_slots() {
        let heap = Heap::new();
        let numbers = |count: usize| -> Vec<Value> {
            (0..3 * count)
                .map(|number| Value::Number((number % count) as f64))
                .collect()
        };

        for count in 0..=40 {
            let items = numbers(count);
            let mut kept = Kept::default();
            for item in &items {
                kept.keep(*item, &heap).unwrap();
            }

            assert_eq!(kept.values, items[..count]);
            assert_eq!(kept.slots.is_empty(), count <= COMPARED_DIRECTLY, "{count}");
            assert!(kept.slots.len() >= 2 * count || kept.slots.is_empty());
            assert!(kept.slots.len().is_power_of_two() || kept.slots.is_empty());
            assert_eq!(
                kept.slots.iter().filter(|slot| **slot != 0).count(),
                if kept.slots.is_empty() { 0 } else { count }
            );
        }
    }

    #[test]
    fn many_numbers_are_told_apart_in_one_pass() {
        let heap = Heap::new();

        let items: Vec<Value> = (0..200_000)
            .map(|number| Value::Number(f64::from(number * 7 % 50_021)))
            .collect();
        let found = distinct(&items, &heap).unwrap();

        assert_eq!(found.len(), 50_021);
        assert_eq!(found[..50_021], items[..50_021]);

        // Numbers that nothing is equal to are all kept, and do not slow the others down.
        let mut items = vec![Value::Number(f64::NAN); 100_000];
        items.extend((0..100_000).map(|number| Value::Number(f64::from(number % 10))));
        let found = distinct(&items, &heap).unwrap();

        assert_eq!(found.len(), 100_010);
        assert_eq!(found[100_009], Value::Number(9.0));
    }

    #[test]
    fn many_strings_are_told_apart_in_one_pass() {
        let mut heap = Heap::new();

        let items: Vec<Value> = (0..60_000)
            .map(|number| heap.insert_string(format!("name-{}", number % 20_011)))
            .collect();
        let found = distinct(&items, &heap).unwrap();

        assert_eq!(found.len(), 20_011);
        assert_eq!(found[..20_011], items[..20_011]);
    }
}
