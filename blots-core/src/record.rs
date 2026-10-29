//! Records: ordered collections of values under string keys.
//!
//! A record is stored as its keys, in the order they were added, and one value per key. The
//! keys are in a *table* and the values in a *block*, both of which records share: a record
//! has the first so many keys of its table and the first so many values of its block.
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
//! - The values of such a record start with the values of the other one, which it reads
//!   where they are when its own value fits behind them and nothing else was put there
//!   (see [`RecordBuilder`]). Like the keys, the values of a record that is built one key
//!   at a time are copied once per doubling of their number.

use crate::{bytecode::Literal, list::AddressHasher, text::Text, values::Value};
use indexmap::IndexMap;
use std::{
    alloc::{Layout, alloc, dealloc, handle_alloc_error},
    cell::{Cell, OnceCell, RefCell},
    collections::{HashMap, hash_map::Entry},
    fmt,
    hash::BuildHasherDefault,
    mem::ManuallyDrop,
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

    /// The head of every key (see [`short_head`]), in the order of the keys, and
    /// [`NO_KEY`] where the table has room for a key that it does not have yet: there is
    /// room for `capacity` heads, and for [`LINEAR_SEARCH_KEYS`] at least. A key is looked
    /// for among the heads. Its characters are only read when it is a long one, to make
    /// sure of it.
    heads: Box<[Cell<u64>]>,

    /// Where the keys are, for tables with room for more than [`LINEAR_SEARCH_KEYS`]: a
    /// slot holds the position of a key plus one, and zero while it is free. A key is in
    /// the first free slot from the one its hash picks, going around at the end; at most
    /// half of the slots are ever taken, and there is a power of two of them.
    slots: Box<[Cell<u32>]>,

    /// Whether this is the canonical table of its key list (see [`Keys::interned`]). Such a
    /// table has no room for more keys, so it stays the table of that list.
    interned: bool,

    /// The tables that were made from the first keys of this one and a key that could not
    /// be added to it, most recent last.
    forks: RefCell<Vec<Fork>>,

    /// What is remembered about the string that was made for each key, which a loop over
    /// the keys of a record finds again (see [`Keys::string_of`]). There is room for
    /// `capacity` of them once a string was made for a key.
    strings: OnceCell<Box<[Literal]>>,
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
            heads: vec![Cell::new(NO_KEY); capacity.max(LINEAR_SEARCH_KEYS)].into_boxed_slice(),
            slots: slots.into_boxed_slice(),
            interned,
            forks: RefCell::new(Vec::new()),
            strings: OnceCell::new(),
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
                table.push(Rc::clone(key), head_of(key.as_bytes()));
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

    /// What is remembered about the string that was made for the key at `position`, which
    /// the table has. Every record with that key there has it under the same characters,
    /// so the string that was made for one of them is the string of each.
    #[inline(always)]
    pub(crate) fn string_of(&self, position: usize) -> &Literal {
        let strings = match self.strings.get() {
            Some(strings) => strings,
            None => self.make_room_for_strings(),
        };

        &strings[position]
    }

    #[cold]
    #[inline(never)]
    fn make_room_for_strings(&self) -> &[Literal] {
        self.strings
            .get_or_init(|| (0..self.capacity).map(|_| Literal::new()).collect())
    }

    /// The position of `key` among the first `count` keys, which the table has to have.
    #[inline]
    pub fn position(&self, count: usize, key: &str) -> Option<usize> {
        self.position_by(count, key.as_bytes(), head_of(key.as_bytes()))
    }

    /// The position of `key`, whose head is `head`, among the first `count` keys, which the
    /// table has to have.
    #[inline]
    fn position_by(&self, count: usize, key: &[u8], head: u64) -> Option<usize> {
        match key.len() <= HEAD_BYTES {
            true => self.position_of_short(count, head),
            false => self.position_of_long(count, key, head),
        }
    }

    /// The position, among the first `count` keys, of the key of at most [`HEAD_BYTES`]
    /// bytes whose head is `head`. No other key has that head.
    #[inline(always)]
    fn position_of_short(&self, count: usize, head: u64) -> Option<usize> {
        self.position_where(count, head, |_| true)
    }

    /// The position of `key`, which has more than [`HEAD_BYTES`] bytes and whose head is
    /// `head`, among the first `count` keys, which the table has to have.
    fn position_of_long(&self, count: usize, key: &[u8], head: u64) -> Option<usize> {
        assert!(
            count <= self.filled.get(),
            "a table has the keys it is asked for"
        );

        // SAFETY: the table has a key wherever this is asked, as it has `count` of them.
        self.position_where(count, head, |position| unsafe {
            self.has_at(position, key)
        })
    }

    /// The position, among the first `count` keys, of the key with this `head` that
    /// `is_sought`. That is asked about the positions of keys that the table has, among
    /// the first `count` ones where there are no more keys than are searched linearly.
    #[inline(always)]
    fn position_where(
        &self,
        count: usize,
        head: u64,
        is_sought: impl Fn(usize) -> bool,
    ) -> Option<usize> {
        if count <= LINEAR_SEARCH_KEYS {
            // SAFETY: a table has room for the heads of as many keys as are searched
            // linearly, at least.
            let heads = unsafe {
                &*self
                    .heads
                    .as_ptr()
                    .cast::<[Cell<u64>; LINEAR_SEARCH_KEYS]>()
            };

            for (position, own) in heads.iter().enumerate() {
                if own.get() == head {
                    // The keys from here on are not among the first `count`, and neither
                    // is the one that is sought if it is one of them.
                    if position >= count {
                        return None;
                    }
                    if is_sought(position) {
                        return Some(position);
                    }
                }
            }

            return None;
        }

        // A table with more keys than that has room for them, and so has slots.
        let Some(mask) = self.slots.len().checked_sub(1) else {
            keys_are_missing();
        };
        let mut slot = mixed(head) as usize & mask;

        loop {
            // SAFETY: the table has a power of two of slots, which `mask` is one less
            // than, so it keeps `slot` among them.
            let entry = unsafe { self.slots.get_unchecked(slot) }.get();
            if entry == 0 {
                return None;
            }

            let position = entry as usize - 1;
            // SAFETY: a slot holds the position of a key that the table has, and with it
            // that of the head of the key.
            if unsafe { self.heads.get_unchecked(position) }.get() == head && is_sought(position) {
                return (position < count).then_some(position);
            }

            slot = (slot + 1) & mask;
        }
    }

    /// Whether the key at `position` is `key`, which has more than [`HEAD_BYTES`] bytes.
    ///
    /// # Safety
    /// The table must have a key at `position`.
    #[inline(always)]
    unsafe fn has_at(&self, position: usize, key: &[u8]) -> bool {
        // SAFETY: the table has a key at `position`, which nothing writes to.
        is_long_key(
            unsafe { &*self.start.as_ptr().add(position) }.as_bytes(),
            key,
        )
    }

    /// Add `key`, whose head is `head` and which the table has room for and does not have
    /// yet.
    fn push(&self, key: Key, head: u64) {
        let position = self.filled.get();
        assert!(
            position < self.capacity,
            "a table has room for the key it is given"
        );

        self.heads[position].set(head);

        if !self.slots.is_empty() {
            let mask = self.slots.len() - 1;
            let mut slot = mixed(head) as usize & mask;
            while self.slots[slot].get() != 0 {
                slot = (slot + 1) & mask;
            }

            // The position fits: a table has room for fewer keys than a slot counts.
            self.slots[slot].set(position as u32 + 1);
        }

        // SAFETY: the table has room for a key at `position`, which is behind every key
        // that anything reads.
        unsafe { self.start.as_ptr().add(position).write(key) };
        self.filled.set(position + 1);
    }

    /// The table that has the first `count` keys of this one and then `key`, which is not
    /// one of them. It is this table when the key is where it has to be or can be put there.
    pub fn followed_by(self: &Rc<Self>, count: usize, key: &str) -> Rc<Keys> {
        self.followed_by_key(count, key, head_of(key.as_bytes()))
    }

    /// As [`Keys::followed_by`], for a key whose head is `head`.
    fn followed_by_key(self: &Rc<Self>, count: usize, key: &str, head: u64) -> Rc<Keys> {
        let filled = self.filled.get();
        assert!(
            count <= filled,
            "a table has the keys that a key is added to"
        );

        if count < filled {
            // A key of at most as many bytes as its head holds is the only one with that
            // head. A longer one is compared with the key that has the head.
            let is_there = self.heads[count].get() == head
                && match key.len() <= HEAD_BYTES {
                    true => true,
                    // SAFETY: the table has a key at `count`.
                    false => unsafe { self.has_at(count, key.as_bytes()) },
                };

            if is_there {
                return Rc::clone(self);
            }
        } else if filled < self.capacity {
            self.push(Key::from(key), head);
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
        for (shared, head) in self.first(count).iter().zip(&self.heads) {
            table.push(Rc::clone(shared), head.get());
        }
        table.push(Key::from(key), head);
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

/// The word whose lowest bytes are `bytes`, the first one lowest, and whose other bytes are
/// zero. There are at most eight of them.
///
/// The bytes are read a few at a time from both ends, which takes no loop over them and no
/// call to copy them: most keys are a word at most, and are hashed and compared as one.
#[inline(always)]
fn word_of(bytes: &[u8]) -> u64 {
    let len = bytes.len();
    debug_assert!(len <= size_of::<u64>());
    let start = bytes.as_ptr();

    // SAFETY: every read is of bytes of the slice: four from its start and the four that
    // end it when it has at least four, its first, middle and last one otherwise.
    unsafe {
        if len >= 4 {
            let low = u32::from_le(start.cast::<u32>().read_unaligned()) as u64;
            let high = u32::from_le(start.add(len - 4).cast::<u32>().read_unaligned()) as u64;

            low | high << ((len - 4) * 8)
        } else if len > 0 {
            let first = *start as u64;
            let middle = *start.add(len / 2) as u64;
            let last = *start.add(len - 1) as u64;

            first | middle << (len / 2 * 8) | last << ((len - 1) * 8)
        } else {
            0
        }
    }
}

/// The number of bytes that the head of a key holds.
const HEAD_BYTES: usize = size_of::<u64>();

/// What the highest byte of the head of a key of fewer than [`HEAD_BYTES`] bytes has set,
/// next to the number of bytes of the key.
const SHORT_KEY: u64 = 0xF8;

/// The highest byte of the head of a key of more than [`HEAD_BYTES`] bytes.
const LONG_KEY: u64 = 0xF5;

/// The head of no key: that of a key without bytes that has one. It stands where a table
/// has room for the head of a key that it does not have yet.
const NO_KEY: u64 = SHORT_KEY << (u64::BITS - u8::BITS) | 1;

/// What the head of a key of so many bytes has next to the bytes of the key: its highest
/// byte for a key of fewer than [`HEAD_BYTES`] bytes, and nothing for one of just as many.
const HEAD_MARKS: [u64; HEAD_BYTES + 1] = {
    let mut marks = [0; HEAD_BYTES + 1];

    let mut len = 0;
    while len < HEAD_BYTES {
        marks[len] = (SHORT_KEY | len as u64) << (u64::BITS - u8::BITS);
        len += 1;
    }

    marks
};

/// The head of a key of `len` bytes, which are [`HEAD_BYTES`] at most and the lowest bytes
/// of `word`, the first one lowest. The other bytes of `word` are zero.
///
/// The head of a key is a word that tells it from the other keys of a table at a glance:
///
/// - a key of at most [`HEAD_BYTES`] bytes has its bytes there, and one of fewer bytes
///   also their number, in the highest byte, which is one that no character has (the bytes
///   of UTF-8 stay below [`LONG_KEY`]). No other key has that head;
/// - a longer key has a hash of its bytes there, under [`LONG_KEY`] as the highest byte.
///   Another long key may have the same head, which is rare enough: the bytes of a long
///   key are only compared with a key that is almost certain to be the same.
#[inline(always)]
pub(crate) fn short_head(word: u64, len: usize) -> u64 {
    debug_assert!(len <= HEAD_BYTES);

    word | HEAD_MARKS[len]
}

/// The head of `key` (see [`short_head`]).
#[inline(always)]
fn head_of(key: &[u8]) -> u64 {
    match key.len() {
        len if len < HEAD_BYTES => short_head(word_of(key), len),
        // SAFETY: the key has just the eight bytes that are read.
        HEAD_BYTES => unsafe { word_at(key, 0) },
        _ => long_head(key),
    }
}

/// The head of `key`, which has more than [`HEAD_BYTES`] bytes: every byte counts, eight at
/// a time, and so does their number.
fn long_head(key: &[u8]) -> u64 {
    let last = key.len() - HEAD_BYTES;

    // SAFETY: the key has eight bytes from every position up to `last`. The last eight
    // reach back over bytes that were counted already where fewer than eight are left.
    let hash = unsafe {
        let mut hash = mixed(word_at(key, 0) ^ key.len() as u64);

        let mut at = HEAD_BYTES;
        while at < last {
            hash = mixed(hash ^ word_at(key, at));
            at += HEAD_BYTES;
        }

        mixed(hash ^ word_at(key, last))
    };

    hash >> u8::BITS | LONG_KEY << (u64::BITS - u8::BITS)
}

/// The eight bytes of `bytes` from `at` on as a word, the first one lowest.
///
/// # Safety
/// The bytes must be there.
#[inline(always)]
unsafe fn word_at(bytes: &[u8], at: usize) -> u64 {
    debug_assert!(at + HEAD_BYTES <= bytes.len());

    // SAFETY: the eight bytes from `at` on are bytes of the slice.
    u64::from_le(unsafe { bytes.as_ptr().add(at).cast::<u64>().read_unaligned() })
}

/// Whether `own` is `key`, which has more than [`HEAD_BYTES`] bytes.
#[inline(always)]
fn is_long_key(own: &[u8], key: &[u8]) -> bool {
    debug_assert!(key.len() > HEAD_BYTES);

    if own.len() != key.len() {
        return false;
    }

    match key.len() {
        // SAFETY: both have at least eight bytes, which are read from their starts and
        // as the eight that end them.
        len if len <= 2 * HEAD_BYTES => unsafe {
            let last = len - HEAD_BYTES;

            word_at(own, 0) == word_at(key, 0) && word_at(own, last) == word_at(key, last)
        },
        _ => own == key,
    }
}

/// A word that every bit of `word` has a say in all over: the two halves of its product
/// with a constant, one over the other. The lower bits of what this makes of the head of
/// a key pick the slot of the key.
#[inline(always)]
fn mixed(word: u64) -> u64 {
    let product = u128::from(word) * 0x9E37_79B9_7F4A_7C15_u128;

    product as u64 ^ (product >> u64::BITS) as u64
}

/// A record that has more keys than its table, which the keys of a record are the first of,
/// is not one that was made here.
#[cold]
#[inline(never)]
fn keys_are_missing() -> ! {
    panic!("a table has the keys it is asked for")
}

/// The values of records that are made by adding to each other, in the order of their
/// keys.
///
/// Like the keys of a table, values are only ever added to a block, behind the ones it has
/// and only where it has room for them, and a record reads the first so many values of its
/// block. What is added behind them is nothing to it, so a record that is made from another
/// one and a key that the other one does not have can read the values of the other one
/// where they are, with its own value behind them.
///
/// The values follow the block in the same allocation.
#[repr(C, align(8))]
struct Block {
    /// The number of records and builders that read the block.
    readers: Cell<u32>,

    /// The number of values that are initialised.
    filled: Cell<u32>,

    /// The number of values that there is room for.
    capacity: u32,

    /// Whether the values of a record that filled the block were moved to a block with room
    /// to spare, for a value that did not fit behind them. That happens once per block:
    /// whatever else is made from that record is not what it grows into.
    outgrown: Cell<bool>,
}

// The first value is right behind the block.
const _: () = assert!(
    size_of::<Block>() % align_of::<Value>() == 0 && align_of::<Value>() <= align_of::<Block>()
);

/// The fewest values that a block has room for when it was made for a record that keeps
/// growing.
const MIN_GROWING_VALUES: usize = 4;

/// A hold on a block, which lives for as long as anything has a hold on it.
struct Values {
    block: NonNull<Block>,
}

impl Values {
    fn layout(capacity: usize) -> Layout {
        let values = Layout::array::<Value>(capacity).expect("the values of a record fit memory");

        Layout::new::<Block>()
            .extend(values)
            .expect("the values of a record fit memory")
            .0
    }

    /// A block without values that has room for `capacity` of them.
    fn with_room(capacity: usize) -> Self {
        assert!(
            capacity <= u32::MAX as usize,
            "a block counts its values in 32 bits"
        );

        let layout = Self::layout(capacity);
        // SAFETY: the layout is never empty, since the block itself is part of it.
        let block = unsafe { alloc(layout) }.cast::<Block>();
        let Some(block) = NonNull::new(block) else {
            handle_alloc_error(layout);
        };

        // SAFETY: the allocation starts with room for a block.
        unsafe {
            block.as_ptr().write(Block {
                readers: Cell::new(1),
                filled: Cell::new(0),
                capacity: capacity as u32,
                outgrown: Cell::new(false),
            });
        }

        Self { block }
    }

    /// A block that starts with `values` and has room for `capacity` of them.
    fn of(values: &[Value], capacity: usize) -> Self {
        assert!(values.len() <= capacity, "a block has room for its values");

        let block = Self::with_room(capacity);
        // SAFETY: the block has room for as many values, which nothing else refers to.
        unsafe {
            std::ptr::copy_nonoverlapping(values.as_ptr(), block.start(), values.len());
            block.set_filled(values.len());
        }

        block
    }

    /// The hold that was given up for the address of the first value of its block.
    ///
    /// # Safety
    /// `start` has to come from [`Values::into_start`], and stands for one hold.
    #[inline(always)]
    unsafe fn from_start(start: NonNull<Value>) -> Self {
        // SAFETY: the values follow their block in the same allocation.
        let block = unsafe { start.cast::<Block>().sub(1) };

        Self { block }
    }

    /// Give the hold up for the address of the first value of its block, which is all that
    /// [`Values::from_start`] takes to make it again.
    #[inline(always)]
    fn into_start(self) -> NonNull<Value> {
        // SAFETY: an address within an allocation is not null.
        let start = unsafe { NonNull::new_unchecked(self.start()) };
        std::mem::forget(self);

        start
    }

    #[inline(always)]
    fn block(&self) -> &Block {
        // SAFETY: a hold keeps its block alive.
        unsafe { self.block.as_ref() }
    }

    /// Where the first value is. It is derived from the pointer to the whole allocation,
    /// not from a reference to the block, so that it is valid for every value that the
    /// allocation has room for.
    #[inline(always)]
    fn start(&self) -> *mut Value {
        // SAFETY: the values follow the block in the same allocation.
        unsafe { self.block.as_ptr().add(1).cast() }
    }

    #[inline(always)]
    fn filled(&self) -> usize {
        self.block().filled.get() as usize
    }

    #[inline(always)]
    fn capacity(&self) -> usize {
        self.block().capacity as usize
    }

    /// Whether anything else reads the block.
    #[inline(always)]
    fn is_shared(&self) -> bool {
        self.block().readers.get() > 1
    }

    /// # Safety
    /// The first `filled` values have to be initialised, and nothing may read a value at
    /// `filled` or behind it.
    unsafe fn set_filled(&self, filled: usize) {
        debug_assert!(filled <= self.capacity());
        self.block().filled.set(filled as u32);
    }

    /// The first `count` values, which the block has to have.
    #[inline(always)]
    fn first(&self, count: usize) -> &[Value] {
        assert!(
            count <= self.filled(),
            "a block has the values it is asked for"
        );

        // SAFETY: the first `filled` values are initialised, and only the one hold that a
        // block has left writes to them.
        unsafe { std::slice::from_raw_parts(self.start(), count) }
    }

    /// Whether a value can be added in place by something that reads the first `count`
    /// values: nothing else has put a value behind them, and there is room for one.
    #[inline(always)]
    fn ends_with_room(&self, count: usize) -> bool {
        self.filled() == count && count < self.capacity()
    }

    /// Add `value` behind the values of the block, which has to have room for it.
    fn push(&self, value: Value) {
        let filled = self.filled();
        assert!(
            filled < self.capacity(),
            "a block has room for the value it is given"
        );

        // SAFETY: the block has room for a value at `filled`, which is behind every value
        // that anything reads.
        unsafe {
            self.start().add(filled).write(value);
            self.set_filled(filled + 1);
        }
    }
}

impl Clone for Values {
    fn clone(&self) -> Self {
        let block = self.block();
        let readers = block
            .readers
            .get()
            .checked_add(1)
            .expect("a block counts its readers in 32 bits");
        block.readers.set(readers);

        Self { block: self.block }
    }
}

impl Drop for Values {
    fn drop(&mut self) {
        let block = self.block();
        let readers = block.readers.get() - 1;
        block.readers.set(readers);

        if readers == 0 {
            let layout = Self::layout(self.capacity());
            // SAFETY: nothing reads the block any more, which was allocated with this
            // layout. Values are plain data.
            unsafe { dealloc(self.block.as_ptr().cast(), layout) };
        }
    }
}

/// Where the values of a record are, which is a matter of how the record was made.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Place {
    /// In an allocation that just fits them and that the record has to itself: the record
    /// was not made by adding to another one.
    Own,

    /// As [`Place::Own`], for a record that was made by adding to one that has its values
    /// to itself, or from the keys of a record that was made by adding to another one.
    Grown,

    /// At the start of a block that the record has a hold on: the record was made by adding
    /// to a record that was itself made by adding to another one.
    Shared,
}

/// How many values a record has and where they are: the number in the lower half of a
/// word, the [`Place`] in the upper half.
///
/// They are one word because a record is moved a word at a time. A word that is read back
/// right after its halves were written one by one has to wait for both of them to make it
/// to memory, which holds up every record that is made.
#[derive(Clone, Copy)]
struct Extent(u64);

impl Extent {
    #[inline(always)]
    fn new(len: usize, place: Place) -> Self {
        // A table has fewer keys than 32 bits count, and a record has a value per key.
        Self((place as u64) << 32 | len as u32 as u64)
    }

    #[inline(always)]
    fn len(self) -> usize {
        self.0 as u32 as usize
    }

    #[inline(always)]
    fn place(self) -> Place {
        match self.0 >> 32 {
            0 => Place::Own,
            1 => Place::Grown,
            _ => Place::Shared,
        }
    }
}

/// A collection of values under string keys that keeps the order the keys were added in.
pub struct Record {
    /// The table that starts with the keys of the record.
    keys: Rc<Keys>,

    /// The first value: there is one per key, in the same order.
    start: NonNull<Value>,
    extent: Extent,
}

impl Record {
    /// A record with the first keys of `keys`: one for each of the `values`, in the same
    /// order.
    pub fn new(keys: Rc<Keys>, values: Box<[Value]>) -> Self {
        Self::owning(keys, values, Place::Own)
    }

    /// A record that has `values` to itself.
    fn owning(keys: Rc<Keys>, values: Box<[Value]>, place: Place) -> Self {
        assert!(values.len() <= keys.len(), "a record has one key per value");
        debug_assert!(place != Place::Shared);

        let extent = Extent::new(values.len(), place);
        // SAFETY: the address of what a box holds is never null.
        let start = unsafe { NonNull::new_unchecked(Box::into_raw(values).cast::<Value>()) };

        Self {
            keys,
            start,
            extent,
        }
    }

    /// A record that reads the first `len` values of the block that `values` is a hold on.
    fn sharing(keys: Rc<Keys>, values: Values, len: usize) -> Self {
        assert!(len <= keys.len(), "a record has one key per value");
        assert!(
            len <= values.filled(),
            "a block has the values of its records"
        );

        Self {
            keys,
            start: values.into_start(),
            extent: Extent::new(len, Place::Shared),
        }
    }

    /// Where the record has its values.
    #[inline(always)]
    fn place(&self) -> Place {
        self.extent.place()
    }

    /// The hold that the record has on its block, to look at and not to give up, unless
    /// the record has its values to itself.
    #[inline(always)]
    fn held(&self) -> Option<ManuallyDrop<Values>> {
        // SAFETY: the address is that of the hold that the record was made with, which the
        // record keeps.
        (self.place() == Place::Shared)
            .then(|| ManuallyDrop::new(unsafe { Values::from_start(self.start) }))
    }

    /// The table that starts with the keys of the record.
    pub fn table(&self) -> &Rc<Keys> {
        &self.keys
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.extent.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The position of the entry under `key`.
    #[inline]
    pub fn position(&self, key: &str) -> Option<usize> {
        self.keys.position(self.len(), key)
    }

    /// The value under the key that `text` spells.
    ///
    /// A text of a few bytes has them at hand as the head of the key, which is all that
    /// such a key takes to find. So this is how a key that was computed is looked up: for
    /// most keys it comes to no call and no look at the characters of any key.
    #[inline(never)]
    pub fn value_under(&self, text: &Text) -> Option<&Value> {
        let Some((word, len)) = text.as_word() else {
            return self.value_under_long(text);
        };

        let position = self
            .keys
            .position_of_short(self.len(), short_head(word, len))?;

        // SAFETY: the record has a value for each of its keys, which the key is one of.
        Some(unsafe { &*self.start.as_ptr().add(position) })
    }

    /// As [`Record::value_under`], for a key that is not all in its head.
    #[inline(never)]
    fn value_under_long(&self, key: &str) -> Option<&Value> {
        self.get(key)
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.position(key)
            .map(|position| &self.as_slice()[position])
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
        let value = self.as_slice().get(position)?;

        Some((&self.keys.first(self.len())[position], value))
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.position(key).is_some()
    }

    /// The keys, in the order they were added.
    pub fn keys(&self) -> std::slice::Iter<'_, Key> {
        self.keys.first(self.len()).iter()
    }

    pub fn values(&self) -> std::slice::Iter<'_, Value> {
        self.as_slice().iter()
    }

    /// The values, in the order of the keys.
    #[inline(always)]
    pub fn as_slice(&self) -> &[Value] {
        // SAFETY: a record has as many values as it says from `start` on, in an allocation
        // of its own or at the start of a block that it keeps alive. Nothing writes to them
        // while a borrow of the record is alive.
        unsafe { std::slice::from_raw_parts(self.start.as_ptr(), self.len()) }
    }

    pub fn iter(&self) -> Iter<'_> {
        self.keys().zip(self.values())
    }

    /// Whether the two records read their values from the same block.
    pub fn shares_values_with(&self, other: &Record) -> bool {
        self.place() == Place::Shared && other.place() == Place::Shared && self.start == other.start
    }

    /// For a compaction pass in which the record survives: the values that have to survive
    /// with it, apart from those that `shared` already had from other records of the same
    /// block.
    #[inline]
    pub(crate) fn unmarked(&self, shared: &mut SharedValues) -> &[Value] {
        match self.held() {
            Some(values) if values.is_shared() => {
                let known = shared.look_at(&values, self.len());

                &self.as_slice()[known..]
            }
            _ => self.as_slice(),
        }
    }

    /// For a compaction pass in which the record survives: replace every value that
    /// survives with it by what `replace` makes of it, and give up the values of its block
    /// that no record reads any more. Returns the number of values this accounted for,
    /// which is none when another record of the same block already did.
    ///
    /// The values are replaced where they are when only the records of the pass read them.
    /// A record or a builder that something else still holds on to keeps reading what it
    /// always read: the records of the pass then get a block of their own.
    ///
    /// # Safety
    /// Every record that survives the pass has to be rewritten in it, with the `shared`
    /// that [`Record::unmarked`] filled in for exactly those records, and every other
    /// record that the pass had a hold of has to be gone by then.
    #[inline]
    pub(crate) unsafe fn rewrite(
        &mut self,
        shared: &mut SharedValues,
        mut replace: impl FnMut(Value) -> Value,
    ) -> usize {
        if self.place() == Place::Shared {
            // SAFETY: as the caller guarantees.
            return unsafe { self.rewrite_shared(shared, &mut replace) };
        }

        // SAFETY: the record has its values to itself, and nothing has a reference to them
        // while it is borrowed like this.
        let values = unsafe { std::slice::from_raw_parts_mut(self.start.as_ptr(), self.len()) };
        for value in values.iter_mut() {
            *value = replace(*value);
        }

        values.len()
    }

    /// [`Record::rewrite`] for a record that has its values in a block.
    ///
    /// # Safety
    /// As for [`Record::rewrite`].
    #[inline(never)]
    unsafe fn rewrite_shared(
        &mut self,
        shared: &mut SharedValues,
        replace: &mut impl FnMut(Value) -> Value,
    ) -> usize {
        let values = self.held().expect("the values are in a block");

        // A record that is the only reader of its block by now has nothing to agree on
        // with any other.
        let state = match values.is_shared() {
            true => Some(
                shared
                    .blocks
                    .get_mut(&values.block.as_ptr().cast_const())
                    .expect("a pass has looked at every record that it rewrites"),
            ),
            false => None,
        };

        if let Some(Shared {
            rewritten: Some(moved),
            ..
        }) = &state
        {
            // Another record of the block has replaced the values, and may have moved them.
            if let Some(moved) = moved {
                self.settle(moved.clone());
            }

            return 0;
        }

        let (count, records) = match &state {
            Some(state) => (state.covered, state.records),
            None => (self.len(), 1),
        };

        // The records of the pass are all that is left of the heap, so any other reader is
        // outside of it and keeps the block as it is.
        let outside = values.block().readers.get() as usize > records;
        let moved = outside.then(|| Values::with_room(count));

        let from = values.start();
        let to = moved.as_ref().map_or(from, Values::start);

        // SAFETY: the block has `count` values, and so has the one they move to room for.
        // Where they stay, only the records of the pass read them, and no reference to any
        // of them is alive during the pass. What lies behind the last value that a record
        // of the pass reads is given up, so that the longest record ends where the values
        // do and can be added to in place again.
        unsafe {
            for position in 0..count {
                to.add(position).write(replace(from.add(position).read()));
            }

            moved.as_ref().unwrap_or(&values).set_filled(count);
        }

        if let Some(state) = state {
            state.rewritten = Some(moved.clone());
        }
        if let Some(moved) = moved {
            self.settle(moved);
        }

        count
    }

    /// Read the values from the block that `values` is a hold on from now on. It has to
    /// have them where the record's own block does.
    fn settle(&mut self, values: Values) {
        assert!(self.place() == Place::Shared && self.len() <= values.filled());

        // SAFETY: the address is that of the hold that the record was made with, which the
        // record gives up for the other one.
        let held = unsafe { Values::from_start(self.start) };
        self.start = values.into_start();
        drop(held);
    }
}

impl Drop for Record {
    fn drop(&mut self) {
        match self.place() {
            // SAFETY: the address is that of the hold that the record was made with.
            Place::Shared => drop(unsafe { Values::from_start(self.start) }),
            // SAFETY: the values are what the box held that the record was made from.
            Place::Own | Place::Grown => drop(unsafe {
                Box::from_raw(std::ptr::slice_from_raw_parts_mut(
                    self.start.as_ptr(),
                    self.len(),
                ))
            }),
        }
    }
}

/// What a compaction pass knows about a block that several records read from.
struct Shared {
    /// The number of values, from the first one on, that the surviving records seen so far
    /// read.
    covered: usize,

    /// The number of surviving records seen so far.
    records: usize,

    /// Set once the values have been rewritten: the block they were moved to, when they
    /// did not stay in theirs.
    rewritten: Option<Option<Values>>,
}

/// What a compaction pass knows about the blocks that several records read from.
#[derive(Default)]
pub(crate) struct SharedValues {
    blocks: HashMap<*const Block, Shared, BuildHasherDefault<AddressHasher>>,
}

impl SharedValues {
    /// Take note of a surviving record that reads the first `len` values of the block that
    /// `values` is a hold on. Returns how many of them the records before it read.
    fn look_at(&mut self, values: &Values, len: usize) -> usize {
        match self.blocks.entry(values.block.as_ptr().cast_const()) {
            Entry::Vacant(entry) => {
                entry.insert(Shared {
                    covered: len,
                    records: 1,
                    rewritten: None,
                });

                0
            }
            Entry::Occupied(mut entry) => {
                let shared = entry.get_mut();
                let known = shared.covered.min(len);

                shared.covered = shared.covered.max(len);
                shared.records += 1;

                known
            }
        }
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

/// A copy of a record has values of its own: a compaction pass rewrites the values of the
/// records in its heap where they are, which a record anywhere else must not see.
impl Clone for Record {
    fn clone(&self) -> Self {
        let place = match self.place() {
            Place::Own => Place::Own,
            Place::Grown | Place::Shared => Place::Grown,
        };

        Self::owning(Rc::clone(&self.keys), self.as_slice().into(), place)
    }
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Record")
            .field("keys", &self.keys.first(self.len()))
            .field("values", &self.as_slice())
            .finish()
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
        let entries = entries.into_iter();
        let mut builder = RecordBuilder::with_capacity(entries.size_hint().0);
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

/// Where the values of the entries of a builder are.
enum Room {
    /// In a vector that the builder has to itself.
    Own(Vec<Value>),

    /// At the start of a block: the first so many of its values.
    Shared(Values, usize),
}

/// A record that is still being assembled, one entry at a time.
///
/// The keys so far are the first keys of a table, like those of a record. The values so far
/// are the builder's own, and become those of the record, unless the builder starts with
/// the entries of a record that keeps growing.
///
/// Such a record is one that is made by adding a key to a record that was made by adding a
/// key to another one. Its values are in a block, and a builder that starts with its
/// entries reads them there. An entry under a new key goes behind them where the block has
/// room and nothing else was put there. Where it has not, the values move to another block:
/// one with room for as many values again when the record fills its block and is the first
/// to outgrow it, so that the values of a record that is built key by key are copied once
/// per doubling of their number, and not once per key. Only an entry that replaces one of
/// the record makes the builder copy the values for itself, since the record keeps reading
/// what it always read.
///
/// A record that is added to once or twice is as long as it has to be, then, and so is
/// every record that is not made by adding to another one.
pub struct RecordBuilder {
    /// The table that starts with the keys of the entries so far.
    keys: Rc<Keys>,
    values: Room,

    /// Where the record that the builder started with the entries of has its values, if it
    /// started with those of a record.
    followed: Option<Place>,

    /// Whether the builder was given an entry under a key that it did not have.
    added: bool,

    /// The number of entries that the builder is still going to be given one by one, as
    /// far as it was told.
    expected: usize,

    /// The number of values that the builder wrote to the block it reads.
    written: usize,
}

impl Default for RecordBuilder {
    fn default() -> Self {
        Self::with_capacity(0)
    }
}

impl RecordBuilder {
    /// A builder that is going to be given about `entries` entries one by one.
    pub fn with_capacity(entries: usize) -> Self {
        Self {
            keys: Keys::empty(),
            values: Room::Own(Vec::new()),
            followed: None,
            added: false,
            expected: entries,
            written: 0,
        }
    }

    /// The number of entries so far.
    fn len(&self) -> usize {
        match &self.values {
            Room::Own(values) => values.len(),
            Room::Shared(_, len) => *len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The number of values that the builder wrote so far. The other values of its record
    /// are those of a record that it was given, read where that record has them.
    pub fn written(&self) -> usize {
        match &self.values {
            Room::Own(values) => values.len(),
            Room::Shared(..) => self.written,
        }
    }

    /// Add an entry. A key that is already present keeps its position and gets the new value.
    pub fn insert(&mut self, key: &str, value: Value) {
        self.expected = self.expected.saturating_sub(1);
        self.put(key, value, self.expected);
    }

    /// Add the entries of `record`, in their order.
    pub fn extend(&mut self, record: &Record) {
        // The entries of the first record are the entries so far, under the keys of its
        // table.
        if self.is_empty() {
            self.start_with(record);
            return;
        }

        let mut more = record.len() + self.expected;
        for (key, value) in record {
            more -= 1;
            self.put(key, *value, more);
        }
    }

    /// Make the entries of `record` the entries so far.
    fn start_with(&mut self, record: &Record) {
        self.keys = Rc::clone(&record.keys);
        self.followed = Some(record.place());
        self.values = match record.held() {
            // The values of a block are read where they are.
            Some(values) => Room::Shared(Values::clone(&values), record.len()),
            // The others are copied, with room for the entries that are known to follow.
            None => {
                let mut values = Vec::with_capacity(record.len() + self.expected);
                values.extend_from_slice(record.as_slice());

                Room::Own(values)
            }
        };
    }

    /// Add an entry that at most `more` entries follow.
    fn put(&mut self, key: &str, value: Value, more: usize) {
        let len = self.len();
        let head = head_of(key.as_bytes());

        match self.keys.position_by(len, key.as_bytes(), head) {
            Some(position) => self.replace(position, value, more),
            None => {
                self.keys = self.keys.followed_by_key(len, key, head);
                self.push(value, more);
            }
        }
    }

    /// Put `value` in the place of the value at `position`. At most `more` values follow.
    fn replace(&mut self, position: usize, value: Value, more: usize) {
        match &mut self.values {
            Room::Own(values) => values[position] = value,
            Room::Shared(values, len) => {
                assert!(position < *len, "a value that is replaced is there");

                if values.is_shared() {
                    // A record reads the values, and keeps reading what it always read.
                    let mut own = Vec::with_capacity(*len + more);
                    own.extend_from_slice(values.first(*len));
                    own[position] = value;

                    self.values = Room::Own(own);
                } else {
                    // SAFETY: the position is among the values of the block, which nothing
                    // else reads.
                    unsafe { values.start().add(position).write(value) };
                }
            }
        }
    }

    /// Put `value` behind the values so far. At most `more` values follow it.
    fn push(&mut self, value: Value, more: usize) {
        self.added = true;

        match &mut self.values {
            Room::Own(values) => {
                if values.len() == values.capacity() {
                    // Room for the entries that are known to follow. A vector that was
                    // given that before and still ran out has no idea, and gets room to
                    // spare.
                    match values.capacity() {
                        0 => values.reserve_exact(1 + more),
                        _ => values.reserve(1 + more),
                    }
                }

                values.push(value);
            }
            Room::Shared(values, len) => {
                let needed = *len + 1;

                if values.ends_with_room(*len) {
                    values.push(value);
                    self.written += 1;
                } else {
                    // A record that fills its block is growing, and gets room for as many
                    // values again. The other records that are made from it, and those that
                    // are made from a record that something was put behind already, are
                    // not what it grows into: they get what they are known to need.
                    let growing = values.filled() == *len && !values.block().outgrown.replace(true);
                    let capacity = match growing {
                        true => (needed + more).max(needed * 2).max(MIN_GROWING_VALUES),
                        false => needed + more,
                    };

                    let moved = Values::of(values.first(*len), capacity);
                    moved.push(value);

                    *values = moved;
                    self.written += needed;
                }

                *len = needed;
            }
        }
    }

    /// The values of the entries so far, in the order of their keys: what the builder
    /// refers to, for a compaction pass that it is to survive.
    pub(crate) fn values(&self) -> &[Value] {
        match &self.values {
            Room::Own(values) => values,
            Room::Shared(values, len) => values.first(*len),
        }
    }

    /// For a compaction pass that the builder survives: replace the value of every entry
    /// so far by what `replace` makes of it.
    ///
    /// A builder that reads the values of a block gets values of its own for that. The
    /// records of the pass that read the block have left it by then, with what the pass
    /// made of their values (see [`Record::rewrite`]): a hold that is not one of theirs
    /// keeps a block as it was, and the builder has one.
    pub(crate) fn rewrite(&mut self, replace: &mut impl FnMut(Value) -> Value) {
        if let Room::Shared(values, len) = &self.values {
            let mut own = Vec::with_capacity(*len + self.expected);
            own.extend_from_slice(values.first(*len));

            self.values = Room::Own(own);
        }

        let Room::Own(values) = &mut self.values else {
            unreachable!("the builder has its values to itself by now");
        };
        for value in values.iter_mut() {
            *value = replace(*value);
        }
    }

    pub fn finish(self) -> Record {
        let values = match self.values {
            Room::Shared(values, len) => return Record::sharing(self.keys, values, len),
            Room::Own(values) => values,
        };

        match (self.followed, self.added) {
            // Entries that were all given one by one, or those of a record that has its
            // values to itself with other values under its keys.
            (None, _) | (Some(Place::Own), false) => {
                Record::owning(self.keys, values.into_boxed_slice(), Place::Own)
            }
            // A record that was added to for the first time, or the keys of one that was
            // made by adding to another one.
            (Some(Place::Own), true) | (Some(_), false) => {
                Record::owning(self.keys, values.into_boxed_slice(), Place::Grown)
            }
            // A record that was made by adding to another one, and was added to again: its
            // values go where the next record can add to them.
            (Some(_), true) => {
                let len = values.len();

                Record::sharing(self.keys, Values::of(&values, len), len)
            }
        }
    }
}

impl fmt::Debug for RecordBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let values = match &self.values {
            Room::Own(values) => values.as_slice(),
            Room::Shared(values, len) => values.first(*len),
        };

        f.debug_struct("RecordBuilder")
            .field("keys", &self.keys.first(values.len()))
            .field("values", &values)
            .finish()
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

    fn numbers(range: std::ops::Range<usize>) -> Vec<Value> {
        range.map(|number| Value::Number(number as f64)).collect()
    }

    /// The record that has the entries of `base` and then `entries`, the way a record
    /// literal that starts with a spread makes it.
    fn extended(base: &Record, entries: &[(&str, f64)]) -> Record {
        let mut builder = RecordBuilder::with_capacity(entries.len());
        builder.extend(base);
        for (key, number) in entries {
            builder.insert(key, Value::Number(*number));
        }

        builder.finish()
    }

    /// A record of the numbers from zero up to `len` that was added to three times, one
    /// entry at a time, so that its values have room behind them. Its keys start with
    /// `prefix`.
    fn roomy(prefix: &str, len: usize) -> Record {
        assert!(
            len >= 3,
            "a record that was added to three times has three entries"
        );

        let keys = numbered(prefix, len);
        let entries = || keys.iter().map(String::as_str).enumerate();
        let mut roomy: Record = entries()
            .take(len - 3)
            .map(|(position, key)| (key, Value::Number(position as f64)))
            .collect();
        for (position, key) in entries().skip(len - 3) {
            roomy = extended(&roomy, &[(key, position as f64)]);
        }

        assert!(capacity(&roomy) > len);

        roomy
    }

    /// The number of values that there is room for where `record` has its values.
    fn capacity(record: &Record) -> usize {
        match record.held() {
            Some(values) => values.capacity(),
            None => record.len(),
        }
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
    fn the_word_of_a_few_bytes_has_the_first_of_them_lowest() {
        let bytes = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88];

        for len in 0..=bytes.len() {
            let mut padded = [0; 8];
            padded[..len].copy_from_slice(&bytes[..len]);

            assert_eq!(word_of(&bytes[..len]), u64::from_le_bytes(padded), "{len}");
        }
    }

    /// Keys of every number of bytes up to those of a head and a few more, made of the
    /// characters with the lowest and the highest bytes that the characters of one, two,
    /// three and four bytes have.
    fn keys_of_a_few_characters() -> Vec<String> {
        let characters = [
            "\0",
            "\u{1}",
            "a",
            "\u{7f}",
            "\u{80}",
            "\u{7ff}",
            "\u{800}",
            "\u{ffff}",
            "\u{10000}",
            "\u{10ffff}",
        ];

        let mut keys = vec![String::new()];
        let mut longest = keys.clone();
        for _ in 0..4 {
            longest = longest
                .iter()
                .flat_map(|key| characters.map(|character| format!("{key}{character}")))
                .collect();
            keys.extend(longest.iter().cloned());
        }

        // The keys that are nothing but the lowest byte, and nothing but an ordinary one.
        for len in 5..=HEAD_BYTES + 2 {
            keys.push("\0".repeat(len));
            keys.push("a".repeat(len));
        }

        keys
    }

    #[test]
    fn the_head_of_a_key_of_a_word_at_most_is_that_of_no_other_key() {
        let keys = keys_of_a_few_characters();
        let mut seen: HashMap<u64, &str> = HashMap::new();

        for key in &keys {
            let head = head_of(key.as_bytes());
            let highest = head >> (u64::BITS - u8::BITS);

            assert_ne!(head, NO_KEY, "{key:?}");
            assert_eq!(key.len() > HEAD_BYTES, highest == LONG_KEY, "{key:?}");
            assert_eq!(key.len() < HEAD_BYTES, highest >= SHORT_KEY, "{key:?}");

            if key.len() <= HEAD_BYTES {
                let mut word = [0; HEAD_BYTES];
                word[..key.len()].copy_from_slice(key.as_bytes());
                assert_eq!(
                    head,
                    short_head(u64::from_le_bytes(word), key.len()),
                    "{key:?}"
                );

                if let Some(other) = seen.insert(head, key) {
                    panic!("{key:?} has the head of {other:?}");
                }
            }
        }

        assert!(seen.len() > 5000);
    }

    /// `key` with a byte that it does not have instead of the one at `at`.
    fn with_another_byte_at(key: &[u8], at: usize) -> Vec<u8> {
        let mut other = key.to_vec();
        other[at] = b'-';

        other
    }

    #[test]
    fn every_byte_of_a_long_key_and_their_number_count_for_its_head() {
        let all: Vec<u8> = (b'a'..=b'z').chain(b'A'..=b'Z').collect();

        for len in HEAD_BYTES + 1..=40 {
            let key = &all[..len];
            let head = long_head(key);

            assert_eq!(head, head_of(key), "{len}");
            assert_eq!(head >> (u64::BITS - u8::BITS), LONG_KEY, "{len}");
            assert_ne!(head, long_head(&all[..len + 1]), "{len}");
            assert_ne!(head, long_head(&all[1..len + 1]), "{len}");

            for at in 0..len {
                let other = with_another_byte_at(key, at);

                assert_ne!(head, long_head(&other), "{len}, {at}");
            }
        }

        // The same byte over and over is another key for every number of them.
        let lengths = HEAD_BYTES + 1..200;
        let heads: std::collections::HashSet<u64> = lengths
            .clone()
            .map(|len| long_head("a".repeat(len).as_bytes()))
            .collect();
        assert_eq!(heads.len(), lengths.len());
    }

    #[test]
    fn a_long_key_is_told_from_one_that_differs_in_any_of_its_bytes() {
        let all: Vec<u8> = (b'a'..=b'z').chain(b'A'..=b'Z').collect();

        for len in HEAD_BYTES + 1..=40 {
            let key = &all[..len];
            assert!(is_long_key(key, key), "{len}");

            // One byte longer, one byte shorter, and one that starts later.
            assert!(!is_long_key(&all[..len + 1], key), "{len}");
            assert!(!is_long_key(&all[..len - 1], key), "{len}");
            assert!(!is_long_key(&all[1..len + 1], key), "{len}");

            for at in 0..len {
                let other = with_another_byte_at(key, at);

                assert!(!is_long_key(&other, key), "{len}, {at}");
            }
        }
    }

    #[test]
    fn a_key_is_not_found_for_one_that_differs_in_any_of_its_bytes() {
        let all: String = ('a'..='z').chain('A'..='Z').collect();
        let others = numbered("another-key-", 2 * LINEAR_SEARCH_KEYS);

        for len in 0..=40 {
            let own = &all[..len];

            // On its own, and among enough keys to be looked up by its slot.
            for others in [&others[..0], &others[..]] {
                let keys = std::iter::once(own).chain(others.iter().map(String::as_str));
                let table = Keys::interned(keys);
                let count = table.len();

                assert_eq!(table.position(count, own), Some(0), "{len}");

                // One byte longer, one byte shorter, and one that starts later.
                assert_eq!(table.position(count, &all[..len + 1]), None, "{len}");
                if len > 0 {
                    assert_eq!(table.position(count, &all[..len - 1]), None, "{len}");
                    assert_eq!(table.position(count, &all[1..len + 1]), None, "{len}");
                }

                for at in 0..len {
                    let mut other = own.to_string();
                    other.replace_range(at..at + 1, "-");

                    assert_eq!(table.position(count, &other), None, "{len}, {at}");
                }
            }
        }
    }

    #[test]
    fn keys_of_a_few_characters_of_any_kind_are_found_where_they_are() {
        let keys = keys_of_a_few_characters();

        // A few of them at a time, which are searched one by one, and all of them.
        for table in keys
            .chunks(LINEAR_SEARCH_KEYS)
            .step_by(97)
            .chain([&keys[..]])
        {
            let record: Record = table
                .iter()
                .enumerate()
                .map(|(position, key)| (key.as_str(), Value::Number(position as f64)))
                .collect();
            assert_eq!(record.len(), table.len());

            for (position, key) in table.iter().enumerate() {
                let value = Value::Number(position as f64);

                assert_eq!(record.position(key), Some(position), "{key:?}");
                assert_eq!(record.value_under(&Text::new(key)), Some(&value), "{key:?}");
            }
        }

        // What a few of them do not have is not found among them.
        let few: Record = keys[..LINEAR_SEARCH_KEYS]
            .iter()
            .map(|key| (key.as_str(), Value::Null))
            .collect();
        for key in &keys[LINEAR_SEARCH_KEYS..] {
            assert_eq!(few.position(key), None, "{key:?}");
            assert_eq!(few.value_under(&Text::new(key)), None, "{key:?}");
        }
    }

    #[test]
    fn a_text_finds_the_value_under_the_key_that_it_spells() {
        // Keys of every length that a text has at hand, and of more.
        let all: String = ('a'..='z').chain('A'..='Z').collect();
        let names: Vec<&str> = (0..=40).map(|len| &all[..len]).collect();

        for count in [
            0,
            1,
            LINEAR_SEARCH_KEYS,
            LINEAR_SEARCH_KEYS + 1,
            names.len(),
        ] {
            let record: Record = names[..count]
                .iter()
                .enumerate()
                .map(|(position, name)| (*name, Value::Number(position as f64)))
                .collect();

            for (position, name) in names.iter().enumerate() {
                let expected = (position < count).then_some(Value::Number(position as f64));

                assert_eq!(
                    record.value_under(&Text::new(name)),
                    expected.as_ref(),
                    "{name} in {count}"
                );
                assert_eq!(record.get(name), expected.as_ref(), "{name} in {count}");
            }
        }
    }

    #[test]
    fn a_text_finds_no_value_under_a_key_that_only_the_table_of_a_record_has() {
        let all = numbered("k", 40);
        let table = Keys::interned(all.iter().map(String::as_str));

        for count in [0, 1, 5, LINEAR_SEARCH_KEYS, LINEAR_SEARCH_KEYS + 1, 20, 40] {
            let record = Record::new(Rc::clone(&table), numbers(0..count).into_boxed_slice());

            for (position, name) in all.iter().enumerate() {
                let expected = (position < count).then_some(Value::Number(position as f64));

                assert_eq!(
                    record.value_under(&Text::new(name)),
                    expected.as_ref(),
                    "{name} in {count}"
                );
                assert_eq!(record.get(name), expected.as_ref(), "{name} in {count}");
            }
        }
    }

    #[test]
    fn a_table_with_room_has_none_of_the_keys_that_it_has_room_for() {
        // A table that was made by adding a key to others has room for more keys.
        let base = Keys::interned(["room-a"].into_iter());
        let table = base.followed_by(1, "b");
        assert!(table.len() < table.capacity);

        for key in keys_of_a_few_characters() {
            let expected = match key.as_str() {
                "b" => Some(1),
                _ => None,
            };

            assert_eq!(table.position(table.len(), &key), expected, "{key:?}");
        }
    }

    #[test]
    fn keys_of_every_length_are_found_in_small_and_large_tables() {
        let all: String = ('a'..='z').chain('A'..='Z').collect();
        let names: Vec<&str> = (0..=40).map(|len| &all[..len]).collect();

        for count in [LINEAR_SEARCH_KEYS, names.len()] {
            let table = Keys::interned(names[..count].iter().copied());

            for (position, name) in names.iter().enumerate() {
                let expected = (position < count).then_some(position);
                assert_eq!(table.position(count, name), expected, "{name} in {count}");
            }
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

    #[test]
    fn a_record_that_adds_to_another_one_reads_the_values_of_the_other_one() {
        let base = record(&[("a", 0.0), ("b", 1.0)]);
        let first = extended(&base, &[("c", 2.0)]);
        let second = extended(&first, &[("d", 3.0)]);
        let third = extended(&second, &[("e", 4.0)]);
        let fourth = extended(&third, &[("f", 5.0), ("g", 6.0)]);

        // A record that is added to once or twice just fits its values. One that is added
        // to again gets room for more, which the ones after it put their values in.
        assert!(!first.shares_values_with(&base));
        assert!(!second.shares_values_with(&first));
        assert!(!third.shares_values_with(&second));
        assert!(fourth.shares_values_with(&third));
        assert_eq!(capacity(&base), 2);
        assert_eq!(capacity(&first), 3);
        assert_eq!(capacity(&second), 4);
        assert_eq!(capacity(&third), 10);

        assert_eq!(base.as_slice(), numbers(0..2));
        assert_eq!(first.as_slice(), numbers(0..3));
        assert_eq!(second.as_slice(), numbers(0..4));
        assert_eq!(third.as_slice(), numbers(0..5));
        assert_eq!(fourth.as_slice(), numbers(0..7));
        assert_eq!(names(&fourth), ["a", "b", "c", "d", "e", "f", "g"]);
        assert_eq!(second.get("e"), None);
        assert_eq!(third.get("f"), None);
        assert_eq!(fourth.get("e"), Some(&Value::Number(4.0)));
    }

    #[test]
    fn a_record_that_is_told_how_many_entries_follow_makes_room_for_them_at_once() {
        // A record that is added to for the first time just fits its values, however many
        // of them there are.
        let base = record(&[("a", 0.0), ("b", 1.0)]);
        let longer = extended(&base, &[("c", 2.0), ("d", 3.0), ("e", 4.0)]);

        assert_eq!(longer.as_slice(), numbers(0..5));
        assert_eq!(capacity(&longer), 5);

        // So does one that gets the entries of a second record.
        let mut builder = RecordBuilder::with_capacity(1);
        builder.extend(&base);
        builder.extend(&record(&[("c", 2.0), ("d", 3.0), ("a", 10.0)]));
        builder.insert("e", Value::Number(4.0));
        let merged = builder.finish();

        assert_eq!(names(&merged), ["a", "b", "c", "d", "e"]);
        assert_eq!(merged.as_slice()[1..], numbers(1..5));
        assert_eq!(merged.get("a"), Some(&Value::Number(10.0)));
        assert_eq!(capacity(&merged), 5);
        assert_eq!(base.as_slice(), numbers(0..2));

        // A record that is added to for the third time is taken to keep growing: it gets
        // room for as many values again, or for the entries that follow where those are
        // more.
        let twice = || extended(&extended(&base, &[("c", 2.0)]), &[("d", 3.0)]);
        let keys = numbered("more-", 30);
        let entries: Vec<(&str, f64)> = keys
            .iter()
            .enumerate()
            .map(|(number, key)| (key.as_str(), number as f64))
            .collect();

        let few = extended(&twice(), &entries[4..5]);
        let many = extended(&twice(), &entries[4..29]);

        assert_eq!(capacity(&twice()), 4);
        assert_eq!(capacity(&few), 10);
        assert_eq!(capacity(&many), 29);
        assert_eq!(few.as_slice(), numbers(0..5));
        assert_eq!(many.as_slice(), numbers(0..29));

        // The entries of a second record count like those that are spelled out.
        let mut builder = RecordBuilder::with_capacity(1);
        builder.extend(&twice());
        builder.extend(&record(&entries[4..24]));
        builder.insert("last", Value::Number(24.0));
        let merged = builder.finish();

        assert_eq!(capacity(&merged), 25);
        assert_eq!(merged.as_slice(), numbers(0..25));
    }

    #[test]
    fn records_that_add_to_the_same_record_do_not_show_up_in_each_other() {
        let base = roomy("same-", 3);
        let one = extended(&base, &[("d", 3.0)]);
        let other = extended(&base, &[("d", 30.0)]);
        let another = extended(&base, &[("e", 300.0)]);
        let longer = extended(&one, &[("e", 4.0)]);

        assert!(one.shares_values_with(&base));
        assert!(!other.shares_values_with(&base));
        assert!(!another.shares_values_with(&base));
        assert!(longer.shares_values_with(&one));

        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(one.as_slice(), numbers(0..4));
        assert_eq!(other.as_slice()[..3], numbers(0..3));
        assert_eq!(other.get("d"), Some(&Value::Number(30.0)));
        assert_eq!(another.as_slice()[..3], numbers(0..3));
        assert_eq!(another.get("e"), Some(&Value::Number(300.0)));
        assert_eq!(another.get("d"), None);
        assert_eq!(longer.as_slice(), numbers(0..5));
        assert_eq!(base.get("d"), None);

        // Only the record that ends its block is taken to be growing.
        assert_eq!(capacity(&other), 4);
    }

    #[test]
    fn an_entry_that_replaces_one_leaves_the_record_it_came_from_as_it_was() {
        let base = record(&[("a", 0.0), ("b", 1.0), ("c", 2.0)]);
        let updated = extended(&base, &[("b", 10.0), ("a", 20.0)]);
        let both = extended(&base, &[("d", 3.0), ("d", 30.0), ("a", 40.0)]);

        assert!(!updated.shares_values_with(&base));
        assert!(Rc::ptr_eq(updated.table(), base.table()));
        assert_eq!(capacity(&updated), 3);
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(
            updated.as_slice(),
            [Value::Number(20.0), Value::Number(10.0), Value::Number(2.0)]
        );
        assert_eq!(names(&both), ["a", "b", "c", "d"]);
        assert_eq!(
            both.as_slice(),
            [
                Value::Number(40.0),
                Value::Number(1.0),
                Value::Number(2.0),
                Value::Number(30.0)
            ]
        );
        assert_eq!(base.get("d"), None);

        // A value that was put behind those of a record and then replaced stays there, where
        // the record does not read it and nothing else is put.
        let base = roomy("replace-", 3);
        let twice = extended(&base, &[("d", 3.0), ("d", 30.0)]);
        let after = extended(&base, &[("d", 300.0)]);

        assert!(!twice.shares_values_with(&base));
        assert!(!after.shares_values_with(&base));
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(twice.get("d"), Some(&Value::Number(30.0)));
        assert_eq!(after.get("d"), Some(&Value::Number(300.0)));
        assert_eq!(twice.as_slice()[..3], numbers(0..3));
        assert_eq!(after.as_slice()[..3], numbers(0..3));
    }

    #[test]
    fn builders_that_start_with_the_same_record_do_not_write_over_each_other() {
        // The one that adds an entry first puts it behind the values of the record.
        let base = roomy("first-", 3);
        let mut outer = RecordBuilder::with_capacity(2);
        outer.extend(&base);
        outer.insert("x", Value::Number(10.0));

        let mut inner = RecordBuilder::with_capacity(1);
        inner.extend(&base);
        inner.insert("y", Value::Number(20.0));
        let inner = inner.finish();

        outer.insert("z", Value::Number(30.0));
        let outer = outer.finish();

        assert!(outer.shares_values_with(&base));
        assert!(!inner.shares_values_with(&base));
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(inner.as_slice()[..3], numbers(0..3));
        assert_eq!(inner.as_slice()[3..], [Value::Number(20.0)]);
        assert_eq!(outer.as_slice()[..3], numbers(0..3));
        assert_eq!(
            outer.as_slice()[3..],
            [Value::Number(10.0), Value::Number(30.0)]
        );
        assert_eq!(names(&inner)[3], "y");
        assert_eq!(names(&outer)[3..], ["x", "z"]);

        // Whichever one it is.
        let base = roomy("second-", 3);
        let mut outer = RecordBuilder::with_capacity(1);
        outer.extend(&base);

        let inner = extended(&base, &[("y", 20.0)]);

        outer.insert("x", Value::Number(10.0));
        let outer = outer.finish();

        assert!(inner.shares_values_with(&base));
        assert!(!outer.shares_values_with(&base));
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(inner.as_slice()[3..], [Value::Number(20.0)]);
        assert_eq!(outer.as_slice()[..3], numbers(0..3));
        assert_eq!(outer.as_slice()[3..], [Value::Number(10.0)]);
    }

    #[test]
    fn a_record_that_keeps_being_added_to_has_its_values_copied_once_per_doubling() {
        let keys = numbered("more-", 1000);
        let mut records = vec![record(&[])];

        for (position, key) in keys.iter().enumerate() {
            let longer = extended(records.last().unwrap(), &[(key.as_str(), position as f64)]);
            records.push(longer);
        }

        let blocks = 1 + records
            .windows(2)
            .filter(|pair| !pair[1].shares_values_with(&pair[0]))
            .count();
        assert!(blocks <= 12, "{blocks} blocks for 1000 values");
        assert!(capacity(&records[1000]) < 2000);

        // Every record has the values it was made with.
        for (len, record) in records.iter().enumerate() {
            assert_eq!(record.as_slice(), numbers(0..len));
        }
    }

    #[test]
    fn a_builder_counts_the_values_that_it_wrote() {
        let written = |base: Option<&Record>, entries: &[(&str, f64)]| {
            let mut builder = RecordBuilder::with_capacity(entries.len());
            if let Some(base) = base {
                builder.extend(base);
            }
            for (key, number) in entries {
                builder.insert(key, Value::Number(*number));
            }

            builder.written()
        };

        assert_eq!(written(None, &[]), 0);
        assert_eq!(written(None, &[("a", 0.0), ("b", 1.0), ("a", 2.0)]), 2);

        // The values of a record that has them to itself are copied, whatever follows.
        let base = record(&[("a", 0.0), ("b", 1.0)]);

        assert_eq!(written(Some(&base), &[]), 2);
        assert_eq!(written(Some(&base), &[("c", 2.0)]), 3);
        assert_eq!(written(Some(&base), &[("c", 2.0), ("d", 3.0)]), 4);
        assert_eq!(written(Some(&base), &[("b", 9.0), ("a", 8.0)]), 2);
        assert_eq!(written(Some(&base), &[("b", 9.0), ("c", 2.0)]), 3);

        // The values of a block are read where they are, and a value that fits behind
        // them is all that is written.
        let fresh = || roomy("count-", 4);

        assert_eq!(capacity(&fresh()), 8);
        assert_eq!(written(Some(&fresh()), &[]), 0);
        assert_eq!(written(Some(&fresh()), &[("e", 4.0)]), 1);
        assert_eq!(written(Some(&fresh()), &[("e", 4.0), ("f", 5.0)]), 2);

        // A value that does not fit behind the others, or that replaces one of them, has
        // all of them copied.
        let five = [("e", 4.0), ("f", 5.0), ("g", 6.0), ("h", 7.0), ("i", 8.0)];
        let taken = fresh();
        let _longer = extended(&taken, &[("e", 4.0)]);

        assert_eq!(written(Some(&fresh()), &five), 4 + 9);
        assert_eq!(written(Some(&taken), &[("f", 5.0)]), 5);
        assert_eq!(written(Some(&fresh()), &[("count-1", 9.0)]), 4);
        assert_eq!(written(Some(&fresh()), &[("e", 4.0), ("count-1", 9.0)]), 5);
    }

    #[test]
    fn a_record_that_nothing_is_added_to_has_the_entries_of_the_record() {
        // The values of a record that has them to itself are copied.
        let base = record(&[("a", 0.0), ("b", 1.0)]);
        let same = extended(&base, &[]);

        assert!(!same.shares_values_with(&base));
        assert!(Rc::ptr_eq(same.table(), base.table()));
        assert_eq!(same.as_slice(), numbers(0..2));
        assert_eq!(capacity(&same), 2);

        // The values of a block are read where they are, and whichever of the records is
        // added to first puts its value behind them.
        let base = roomy("nothing-", 3);
        let same = extended(&base, &[]);
        let longer = extended(&same, &[("d", 3.0)]);
        let other = extended(&base, &[("e", 4.0)]);

        assert!(same.shares_values_with(&base));
        assert!(longer.shares_values_with(&base));
        assert!(!other.shares_values_with(&base));
        assert!(Rc::ptr_eq(same.table(), base.table()));
        assert_eq!(same.as_slice(), numbers(0..3));
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(longer.as_slice(), numbers(0..4));
        assert_eq!(other.as_slice()[..3], numbers(0..3));
        assert_eq!(other.get("e"), Some(&Value::Number(4.0)));

        let nothing = RecordBuilder::default().finish();
        assert!(nothing.is_empty());
        assert!(nothing.as_slice().is_empty());
        assert_eq!(nothing.get("a"), None);
        assert!(extended(&nothing, &[]).is_empty());
        assert_eq!(extended(&nothing, &[("a", 0.0)]).as_slice(), numbers(0..1));
    }

    #[test]
    fn a_copy_of_a_record_has_values_of_its_own() {
        let original = roomy("copy-", 3);
        let copy = original.clone();
        let longer = extended(&original, &[("d", 3.0)]);

        assert!(!copy.shares_values_with(&original));
        assert!(longer.shares_values_with(&original));
        assert!(Rc::ptr_eq(copy.table(), original.table()));
        assert!(copy == original);
        assert_eq!(capacity(&copy), 3);
        assert_eq!(copy.as_slice(), numbers(0..3));
        assert_eq!(longer.as_slice(), numbers(0..4));
    }

    fn doubled(value: Value) -> Value {
        match value {
            Value::Number(number) => Value::Number(number * 2.0),
            other => other,
        }
    }

    /// A compaction pass that `records` survive and `garbage` does not: every number that
    /// is left is doubled. Returns the number of values that the pass accounted for.
    fn double(records: &mut [Record], garbage: Vec<Record>) -> usize {
        let mut shared = SharedValues::default();
        for record in records.iter() {
            let _ = record.unmarked(&mut shared);
        }
        drop(garbage);

        records
            .iter_mut()
            // SAFETY: every record that is left is rewritten.
            .map(|record| unsafe { record.rewrite(&mut shared, doubled) })
            .sum()
    }

    fn twice(range: std::ops::Range<usize>) -> Vec<Value> {
        numbers(range).into_iter().map(doubled).collect()
    }

    #[test]
    fn a_surviving_record_is_marked_and_rewritten_once() {
        let mut records = [record(&[("a", 1.0), ("b", 2.0), ("c", 3.0)])];
        let mut shared = SharedValues::default();

        assert_eq!(records[0].unmarked(&mut shared), numbers(1..4));
        assert_eq!(double(&mut records, Vec::new()), 3);
        assert_eq!(records[0].as_slice(), twice(1..4));
        assert_eq!(names(&records[0]), ["a", "b", "c"]);
    }

    #[test]
    fn records_that_share_a_block_mark_each_value_once() {
        let short = roomy("mark-", 3);
        let middle = extended(&short, &[("d", 3.0)]);
        let long = extended(&middle, &[("e", 4.0), ("f", 5.0)]);
        let mut shared = SharedValues::default();

        assert_eq!(middle.unmarked(&mut shared), numbers(0..4));
        assert!(short.unmarked(&mut shared).is_empty());
        assert_eq!(long.unmarked(&mut shared), numbers(4..6));
        assert_eq!(middle.unmarked(&mut SharedValues::default()), numbers(0..4));
    }

    #[test]
    fn records_that_share_a_block_are_rewritten_together() {
        let short = roomy("together-", 3);
        let middle = extended(&short, &[("d", 3.0)]);
        let long = extended(&middle, &[("e", 4.0), ("f", 5.0)]);
        let mut records = [middle, long, short];

        assert_eq!(double(&mut records, Vec::new()), 6);
        assert_eq!(records[0].as_slice(), twice(0..4));
        assert_eq!(records[1].as_slice(), twice(0..6));
        assert_eq!(records[2].as_slice(), twice(0..3));
        assert!(records[0].shares_values_with(&records[1]));
        assert!(records[0].shares_values_with(&records[2]));
        assert_eq!(names(&records[1])[3..], ["d", "e", "f"]);
    }

    #[test]
    fn the_longest_surviving_record_is_added_to_in_place_after_a_pass() {
        let short = roomy("after-", 3);
        let middle = extended(&short, &[("d", 3.0)]);
        let long = extended(&middle, &[("e", 4.0)]);
        let mut records = [short, middle];

        assert_eq!(double(&mut records, vec![long]), 4);
        assert_eq!(records[0].as_slice(), twice(0..3));
        assert_eq!(records[1].as_slice(), twice(0..4));

        // What the record that is gone had behind the values of the others is given up.
        let again = extended(&records[1], &[("g", 7.0)]);
        let aside = extended(&records[0], &[("h", 9.0)]);

        assert!(again.shares_values_with(&records[1]));
        assert!(!aside.shares_values_with(&records[0]));
        assert_eq!(again.as_slice()[..4], twice(0..4));
        assert_eq!(again.as_slice()[4..], [Value::Number(7.0)]);
        assert_eq!(names(&again)[3..], ["d", "g"]);
        assert_eq!(aside.as_slice()[..3], twice(0..3));
        assert_eq!(aside.as_slice()[3..], [Value::Number(9.0)]);
        assert_eq!(records[1].as_slice(), twice(0..4));

        // The same goes for a record that is left on its own.
        let base = roomy("alone-", 3);
        let longer = extended(&base, &[("d", 3.0), ("e", 4.0)]);
        let mut records = [base];

        assert_eq!(double(&mut records, vec![longer]), 3);

        let again = extended(&records[0], &[("f", 5.0)]);
        assert!(again.shares_values_with(&records[0]));
        assert_eq!(again.as_slice()[..3], twice(0..3));
        assert_eq!(again.as_slice()[3..], [Value::Number(5.0)]);
        assert_eq!(names(&again), ["alone-0", "alone-1", "alone-2", "f"]);
    }

    #[test]
    fn a_block_that_something_else_reads_is_left_as_it_is_by_a_pass() {
        let short = roomy("held-", 3);
        let long = extended(&short, &[("d", 3.0)]);
        let mut held = RecordBuilder::default();
        held.extend(&long);

        let mut records = [short, long];
        assert_eq!(double(&mut records, Vec::new()), 4);

        // The records of the pass read their values from a block of their own now.
        assert!(records[0].shares_values_with(&records[1]));
        assert_eq!(records[0].as_slice(), twice(0..3));
        assert_eq!(records[1].as_slice(), twice(0..4));

        held.insert("e", Value::Number(4.0));
        let outside = held.finish();

        assert!(!outside.shares_values_with(&records[1]));
        assert_eq!(outside.as_slice(), numbers(0..5));
        assert_eq!(records[1].as_slice(), twice(0..4));

        // They are added to like before.
        let longer = extended(&records[1], &[("f", 5.0)]);
        assert_eq!(longer.as_slice()[..4], twice(0..4));
        assert_eq!(longer.as_slice()[4..], [Value::Number(5.0)]);
        assert_eq!(records[1].as_slice(), twice(0..4));
    }
}
