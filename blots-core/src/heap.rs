use crate::environment::Environment;
use crate::values::{LambdaDef, PrimitiveValue, ReifiedIterableValue, Value};
use anyhow::{Result, anyhow};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use std::sync::LazyLock;

/// Compaction is skipped while the estimated heap payload is smaller than this; tiny heaps
/// aren't worth a pass.
const COMPACT_MIN_BYTES: usize = 1 << 20;

/// Base growth factor: after a productive pass, the next pass waits until the estimated heap
/// payload is at least this many times the surviving payload.
const COMPACT_GROWTH_FACTOR: usize = 2;

/// Cap for the adaptive back-off factor applied after unproductive passes (ones that reclaim
/// less than half of the heap), so heaps that are mostly live data are not repeatedly
/// re-traversed as they keep growing.
const COMPACT_MAX_GROWTH_FACTOR: usize = 64;

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
    /// List elements are shared via `Rc` so that iterating a list outside of a heap borrow
    /// (e.g. higher-order builtins and broadcasting loops) clones a pointer, not the elements.
    List(Rc<Vec<Value>>),
    String(String),
    Record(IndexMap<String, Value>),

    /// Lambda definitions are shared via `Rc` so that resolving a callee for a call expression
    /// hands out a pointer to the definition instead of copying it field by field.
    Lambda(Rc<LambdaDef>),
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

    /// Cheap shared handle to a list's elements, for callers that need to iterate the list
    /// without holding a borrow of the heap (the elements are not copied).
    pub fn as_list_rc(&self) -> Result<Rc<Vec<Value>>> {
        match self {
            HeapValue::List(list) => Ok(Rc::clone(list)),
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
            HeapValue::Lambda(lambda) => Ok(lambda.as_ref()),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    /// Cheap shared handle to a lambda's definition, for callers that need to keep the
    /// definition (e.g. to invoke it) without holding a borrow of the heap or copying it.
    pub fn as_lambda_rc(&self) -> Result<Rc<LambdaDef>> {
        match self {
            HeapValue::Lambda(lambda) => Ok(Rc::clone(lambda)),
            _ => Err(anyhow!(
                "expected a function, but got a {}",
                self.get_type()
            )),
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

    /// Estimated bytes held by heap values (slot structs plus element/field payloads).
    /// Maintained incrementally on insert and recomputed from the survivors during compaction.
    approx_bytes: usize,

    /// Estimated payload size at which [`Heap::compact_if_grown`] should actually compact.
    /// Recomputed after every pass: gently when the pass reclaimed a lot, aggressively (via
    /// `compact_growth_factor`) when the heap turned out to be mostly live.
    compact_threshold_bytes: usize,

    /// Growth factor used to derive `compact_threshold_bytes` from the surviving payload size;
    /// doubles after each unproductive pass (up to a cap) and resets after a productive one.
    compact_growth_factor: usize,
}

impl Heap {
    pub fn new() -> Self {
        let mut s = Self {
            values: Vec::new(),
            approx_bytes: 0,
            compact_threshold_bytes: COMPACT_MIN_BYTES,
            compact_growth_factor: COMPACT_GROWTH_FACTOR,
        };
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
        self.approx_bytes += approx_heap_value_bytes(&value);
        self.values.push(value);

        self.values.len() - 1
    }

    pub fn insert_list(&mut self, list: Vec<Value>) -> Value {
        Value::List(ListPointer::new(
            self.insert(HeapValue::List(Rc::new(list))),
        ))
    }

    pub fn insert_string(&mut self, string: String) -> Value {
        Value::String(StringPointer::new(self.insert(HeapValue::String(string))))
    }

    pub fn insert_record(&mut self, record: IndexMap<String, Value>) -> Value {
        Value::Record(RecordPointer::new(self.insert(HeapValue::Record(record))))
    }

    pub fn insert_lambda(&mut self, lambda: LambdaDef) -> Value {
        Value::Lambda(LambdaPointer::new(
            self.insert(HeapValue::Lambda(Rc::new(lambda))),
        ))
    }

    pub fn get(&self, id: usize) -> Option<&HeapValue> {
        self.values.get(id)
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut HeapValue> {
        self.values.get_mut(id)
    }

    /// Number of slots currently in the heap: live values plus garbage not yet compacted away.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// True only before the constants record has been inserted (i.e. never for a built `Heap`).
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Reclaim heap slots that are no longer reachable from the root environment's bindings,
    /// `extra_roots`, or the pinned constants record at slot 0.
    ///
    /// This is a stop-and-copy pass intended to run between top-level statements: live values
    /// are moved to the front of the heap, every pointer they contain is rewritten, and the
    /// root bindings and `extra_roots` are rewritten in place. It must not run while any other
    /// `Value` (e.g. an intermediate result mid-evaluation) still refers to the heap, because
    /// such pointers would be left dangling or pointing at the wrong slot.
    ///
    /// `env` must be the root scope that owns its bindings; values reachable only from child
    /// scopes of an in-progress call are not kept alive.
    pub fn compact(&mut self, env: &Environment, extra_roots: &mut [&mut Value]) {
        let bytes_before = self.approx_bytes;
        let old_len = self.values.len();
        let mut reachable = vec![false; old_len];
        let mut worklist: Vec<usize> = Vec::new();

        fn mark(slot: usize, reachable: &mut [bool], worklist: &mut Vec<usize>) {
            if !reachable[slot] {
                reachable[slot] = true;
                worklist.push(slot);
            }
        }

        // The constants record is pinned at slot 0: identifier lookups of `constants` hard-code
        // that index, so it must always survive (and keep its position).
        if old_len > 0 {
            mark(0, &mut reachable, &mut worklist);
        }

        for value in env.flatten().values() {
            if let Some(slot) = value_slot(value) {
                mark(slot, &mut reachable, &mut worklist);
            }
        }

        for root in extra_roots.iter() {
            if let Some(slot) = value_slot(root) {
                mark(slot, &mut reachable, &mut worklist);
            }
        }

        // Trace reachability: a slot's children are the values stored in its list elements,
        // record fields, or lambda captured scope.
        while let Some(slot) = worklist.pop() {
            match &self.values[slot] {
                HeapValue::List(items) => {
                    for item in items.iter() {
                        if let Some(child) = value_slot(item) {
                            mark(child, &mut reachable, &mut worklist);
                        }
                    }
                }
                HeapValue::Record(record) => {
                    for item in record.values() {
                        if let Some(child) = value_slot(item) {
                            mark(child, &mut reachable, &mut worklist);
                        }
                    }
                }
                HeapValue::Lambda(def) => {
                    for (_, item) in def.scope.iter() {
                        if let Some(child) = value_slot(item) {
                            mark(child, &mut reachable, &mut worklist);
                        }
                    }
                }
                HeapValue::String(_) => {}
            }
        }

        // When everything is still reachable there is nothing to move or rewrite (the mapping
        // would be the identity), so skip straight to retuning the trigger: the heap is pure
        // live data and should not be re-traversed again soon.
        let live_slots = reachable.iter().filter(|live| **live).count();
        if live_slots == old_len {
            self.retune_after_pass(bytes_before, bytes_before, old_len, old_len);
            return;
        }

        // Move live values down over dead ones, recording where each old slot moved. New indices
        // never exceed old ones, so the slot vector can be compacted in place; iterating in slot
        // order keeps the pinned constants record at index 0. Dead values are dropped (and their
        // payload buffers freed) by the truncate.
        let mut remap = vec![usize::MAX; old_len];
        let mut next_slot = 0usize;

        for old_slot in 0..old_len {
            if reachable[old_slot] {
                remap[old_slot] = next_slot;
                if next_slot != old_slot {
                    self.values.swap(next_slot, old_slot);
                }
                next_slot += 1;
            }
        }
        self.values.truncate(next_slot);

        // Rewrite every pointer held by the surviving heap values, re-measuring their size along
        // the way so the next-trigger bookkeeping below works from live data only.
        let mut live_bytes = 0usize;

        for value in self.values.iter_mut() {
            live_bytes += approx_heap_value_bytes(value);

            match value {
                HeapValue::List(items) => {
                    for item in Rc::make_mut(items).iter_mut() {
                        *item = remap_value(*item, &remap);
                    }
                }
                HeapValue::Record(record) => {
                    for item in record.values_mut() {
                        *item = remap_value(*item, &remap);
                    }
                }
                HeapValue::Lambda(def) => {
                    // The heap is the only holder of this Rc during compaction (callers do not
                    // retain `FunctionDef`s across statements), so `make_mut` rewrites in place.
                    Rc::make_mut(def)
                        .scope
                        .rewrite_values(&mut |item| remap_value(item, &remap));
                }
                HeapValue::String(_) => {}
            }
        }

        // Rewrite the roots so they point at the slots their values moved to.
        env.rewrite_binding_values(&mut |value| remap_value(value, &remap));
        for root in extra_roots.iter_mut() {
            **root = remap_value(**root, &remap);
        }

        self.retune_after_pass(bytes_before, live_bytes, old_len, self.values.len());
    }

    /// Update the size estimate and the next compaction trigger after a pass.
    ///
    /// A pass that reclaimed at least half of the heap keeps the trigger close (the program is
    /// producing mostly garbage, so frequent passes stay productive). A pass that reclaimed
    /// little means the heap is mostly live data, so the trigger backs off geometrically:
    /// otherwise a large, growing, mostly-live program would pay for a full re-traversal every
    /// time its size doubles.
    fn retune_after_pass(
        &mut self,
        bytes_before: usize,
        live_bytes: usize,
        slots_before: usize,
        live_slots: usize,
    ) {
        if std::env::var_os("BLOTS_COMPACT_DEBUG").is_some() {
            eprintln!(
                "[compact] slots {} -> {}, approx bytes {} -> {}",
                slots_before, live_slots, bytes_before, live_bytes
            );
        }

        let reclaimed_bytes = bytes_before.saturating_sub(live_bytes);
        self.compact_growth_factor = if reclaimed_bytes >= bytes_before / 2 {
            COMPACT_GROWTH_FACTOR
        } else if reclaimed_bytes < bytes_before / 10 {
            // Nearly nothing was garbage; back off hard so the next pass only happens once the
            // heap has grown several times over.
            (self.compact_growth_factor * 4).min(COMPACT_MAX_GROWTH_FACTOR)
        } else {
            (self.compact_growth_factor * 2).min(COMPACT_MAX_GROWTH_FACTOR)
        };
        self.approx_bytes = live_bytes;
        self.compact_threshold_bytes = live_bytes
            .saturating_mul(self.compact_growth_factor)
            .max(COMPACT_MIN_BYTES);
    }

    /// Run [`Heap::compact`] only when the estimated heap payload has grown past the adaptive
    /// threshold set by the previous pass, so compaction is worthwhile. Returns whether
    /// compaction ran (callers holding copies of root values then know those copies were
    /// rewritten).
    pub fn compact_if_grown(&mut self, env: &Environment, extra_roots: &mut [&mut Value]) -> bool {
        if self.approx_bytes < self.compact_threshold_bytes {
            return false;
        }

        self.compact(env, extra_roots);
        true
    }
}

/// Rough estimate of the memory a heap value holds (slot struct plus payload). Only used to
/// decide when compaction is worthwhile, so it favors being cheap over being exact: string text
/// and record keys are approximated, and a list payload shared by several slots is counted once
/// per referencing slot.
fn approx_heap_value_bytes(value: &HeapValue) -> usize {
    use std::mem::size_of;

    let payload = match value {
        HeapValue::List(items) => items.len() * size_of::<Value>(),
        HeapValue::String(string) => string.len(),
        HeapValue::Record(record) => record.len() * (size_of::<String>() + size_of::<Value>() + 16),
        HeapValue::Lambda(def) => 64 + def.scope.len() * (size_of::<Value>() + size_of::<usize>()),
    };

    size_of::<HeapValue>() + payload
}

/// The heap slot a value points to, if it points into the heap at all.
fn value_slot(value: &Value) -> Option<usize> {
    match value {
        Value::List(p) => Some(p.index()),
        Value::String(p) => Some(p.index()),
        Value::Record(p) => Some(p.index()),
        Value::Lambda(p) => Some(p.index()),
        Value::Spread(IterablePointer::List(p)) => Some(p.index()),
        Value::Spread(IterablePointer::String(p)) => Some(p.index()),
        Value::Spread(IterablePointer::Record(p)) => Some(p.index()),
        Value::Number(_) | Value::Bool(_) | Value::Null | Value::BuiltIn(_) => None,
    }
}

/// The same value with its heap pointer (if any) remapped through `remap`, which maps old slot
/// indices to post-compaction indices.
fn remap_value(value: Value, remap: &[usize]) -> Value {
    match value {
        Value::List(p) => Value::List(ListPointer::new(remap[p.index()])),
        Value::String(p) => Value::String(StringPointer::new(remap[p.index()])),
        Value::Record(p) => Value::Record(RecordPointer::new(remap[p.index()])),
        Value::Lambda(p) => Value::Lambda(LambdaPointer::new(remap[p.index()])),
        Value::Spread(IterablePointer::List(p)) => {
            Value::Spread(IterablePointer::List(ListPointer::new(remap[p.index()])))
        }
        Value::Spread(IterablePointer::String(p)) => Value::Spread(IterablePointer::String(
            StringPointer::new(remap[p.index()]),
        )),
        Value::Spread(IterablePointer::Record(p)) => Value::Spread(IterablePointer::Record(
            RecordPointer::new(remap[p.index()]),
        )),
        Value::Number(_) | Value::Bool(_) | Value::Null | Value::BuiltIn(_) => value,
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
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
