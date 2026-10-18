//! Lists: sequences of values that share their storage where they can.
//!
//! The elements of a list never change, so two lists can read the same elements. A list is
//! therefore a run of elements in a *buffer* that it may share with other lists:
//!
//! - a part of a list (`tail`, `slice`) is another run in the same buffer;
//! - a list that ends where its buffer ends can be followed by more elements, which are
//!   written into the room the buffer has left behind it. The list that results covers the
//!   old elements and the new ones; every list that existed before still ends where it did,
//!   and so reads what it always read.
//!
//! The second point is what makes a list that is built one element at a time (the
//! accumulator of a `reduce`, an argument that grows from one recursive call to the next)
//! cost time in proportion to its length, not to the square of it: only when the room runs
//! out are the elements copied, into a buffer twice as long.

use crate::values::Value;
use std::{
    cell::UnsafeCell,
    collections::{HashMap, hash_map::Entry},
    fmt,
    hash::{BuildHasherDefault, Hasher},
    ops::{Deref, Range},
    ptr::NonNull,
    rc::Rc,
};

/// The fewest elements a buffer has room for when it was made for a list that keeps growing.
const MIN_GROWING_CAPACITY: usize = 4;

/// A buffer with room for at least this many elements, and for this many times the elements
/// that the lists surviving a compaction pass still cover, is replaced by one that just fits
/// them.
const TRIMMED_CAPACITY: usize = 64;
const TRIMMED_FACTOR: usize = 4;

/// The elements of one or more lists.
///
/// Only the elements up to the length of the vector are ever read, and a list only reads the
/// ones it covers. The vector never moves its elements while a list refers to it: it is added
/// to only where it has the capacity, and otherwise only changed by a compaction pass, which
/// tells every list that is left where its elements went.
struct Buffer {
    items: UnsafeCell<Vec<Value>>,

    /// Whether the buffer was made for a list that followed another one. Such a list is
    /// likely to be followed by more, so the buffer that takes over from this one when it is
    /// full has room to spare.
    extension: bool,
}

impl Buffer {
    fn new(items: Vec<Value>, extension: bool) -> Rc<Self> {
        Rc::new(Self {
            items: UnsafeCell::new(items),
            extension,
        })
    }
}

/// A sequence of values.
pub struct List {
    /// The first element. It is derived from the buffer's own pointer to its elements, not
    /// from a reference to some of them, so that it stays valid for everything the buffer
    /// holds, including what is added behind the list later on.
    start: NonNull<Value>,
    len: usize,
    buffer: Rc<Buffer>,
}

impl List {
    /// A list of `items`, which it takes over without copying them.
    pub fn new(items: Vec<Value>) -> Self {
        Self::of(Buffer::new(items, false))
    }

    /// The list of everything in `buffer`, which nothing else refers to yet.
    fn of(buffer: Rc<Buffer>) -> Self {
        // SAFETY: nothing else refers to the vector. Its element pointer is never null.
        let (start, len) = unsafe {
            let items = &mut *buffer.items.get();
            (NonNull::new_unchecked(items.as_mut_ptr()), items.len())
        };

        Self { start, len, buffer }
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[Value] {
        // SAFETY: a list covers `len` initialised elements of its buffer from `start` on,
        // which nothing writes to while a borrow of the list is alive.
        unsafe { std::slice::from_raw_parts(self.start.as_ptr(), self.len) }
    }

    /// The elements of the list at the positions in `range`, which have to exist. The result
    /// shares the elements with the list.
    pub fn slice(&self, range: Range<usize>) -> List {
        assert!(
            range.start <= range.end && range.end <= self.len,
            "a part of a list lies within the list"
        );

        List {
            // SAFETY: the position is within the list, or just behind its last element.
            start: unsafe { self.start.add(range.start) },
            len: range.end - range.start,
            buffer: Rc::clone(&self.buffer),
        }
    }

    /// The list followed by `items`. The result shares the elements of the list when the
    /// buffer ends with the list and has room for `items` behind it.
    pub fn followed_by(&self, items: &[Value]) -> List {
        self.followed_by_each(items.len(), |room| {
            // SAFETY: `room` has room for as many values as there are `items`, and is not
            // part of any list yet, so it does not overlap them.
            unsafe { std::ptr::copy_nonoverlapping(items.as_ptr(), room, items.len()) }
        })
    }

    /// The list followed by the `count` values that `write` puts where it is told to.
    fn followed_by_each(&self, count: usize, write: impl FnOnce(*mut Value)) -> List {
        // SAFETY: lists refer to the elements of the vector, never to the vector itself, so
        // nothing else has a reference to it.
        let buffer = unsafe { &mut *self.buffer.items.get() };

        // Neither pointer is read; they only tell whether the list ends where the buffer does.
        let end = self.start.as_ptr().wrapping_add(self.len);
        let buffer_end = buffer.as_ptr().wrapping_add(buffer.len());
        let ends_buffer = std::ptr::eq(end, buffer_end);

        if ends_buffer && buffer.capacity() - buffer.len() >= count {
            // The new elements go behind everything a list covers, into room the vector
            // already has, so the elements it holds stay where they are.
            // SAFETY: the vector has the capacity for `count` more elements, which `write`
            // initialises before they are made part of it.
            unsafe {
                write(buffer.as_mut_ptr().add(buffer.len()));
                buffer.set_len(buffer.len() + count);
            }

            return List {
                start: self.start,
                len: self.len + count,
                buffer: Rc::clone(&self.buffer),
            };
        }

        // A list that is followed for the first time is copied as it is. One that already
        // came from following another list, and filled the buffer it got, is growing: it
        // gets room for as many elements again, so that the elements are copied once per
        // doubling and not once per element.
        let len = self.len + count;
        let capacity = match ends_buffer && self.buffer.extension {
            true => len.saturating_mul(2).max(MIN_GROWING_CAPACITY),
            false => len,
        };

        let mut extended = Vec::with_capacity(capacity);
        extended.extend_from_slice(self);
        // SAFETY: the vector has the capacity for `count` more elements, which `write`
        // initialises before they are made part of it.
        unsafe {
            write(extended.as_mut_ptr().add(self.len));
            extended.set_len(len);
        }

        List::of(Buffer::new(extended, true))
    }

    /// Whether the two lists read their elements from the same buffer.
    pub fn shares_buffer_with(&self, other: &List) -> bool {
        Rc::ptr_eq(&self.buffer, &other.buffer)
    }

    /// The position of the list's first element in its buffer, whose elements start at
    /// `first`.
    fn position(&self, first: *const Value) -> usize {
        // SAFETY: `start` points into the elements of the list's buffer (or just behind them).
        unsafe { self.start.as_ptr().offset_from(first) as usize }
    }

    /// For a compaction pass in which the list survives: the elements that have to survive
    /// with it, apart from those that `shared` already had from other lists of the same
    /// buffer.
    ///
    /// Lists that share a buffer are taken to cover everything from the first element of the
    /// first of them to the last element of the last, so that the pass looks at every element
    /// once however many lists cover it.
    pub(crate) fn unmarked(&self, shared: &mut SharedBuffers) -> [&[Value]; 2] {
        if Rc::strong_count(&self.buffer) == 1 {
            return [self.as_slice(), &[]];
        }

        // SAFETY: nothing adds to a buffer during a compaction pass.
        let buffer = unsafe { &*self.buffer.items.get() };
        let position = self.position(buffer.as_ptr());
        let covered = position..position + self.len;

        match shared.buffers.entry(Rc::as_ptr(&self.buffer)) {
            Entry::Vacant(entry) => {
                entry.insert(Shared {
                    covered,
                    lists: 1,
                    rewritten: None,
                });

                [self.as_slice(), &[]]
            }
            Entry::Occupied(mut entry) => {
                let shared = entry.get_mut();
                let known = shared.covered.clone();
                let below = covered.start.min(known.start)..known.start;
                let above = known.end..covered.end.max(known.end);

                shared.covered = below.start..above.end;
                shared.lists += 1;

                [&buffer[below], &buffer[above]]
            }
        }
    }

    /// For a compaction pass in which the list survives: replace every element that survives
    /// with it by what `replace` makes of it, and give up the room the buffer has for
    /// elements that no list covers any more. Returns the number of elements this accounted
    /// for, which is none when another list of the same buffer already did.
    ///
    /// The elements are replaced where they are when only the lists of the pass read them.
    /// A list that something else still holds on to keeps reading what it always read: the
    /// lists of the pass then get a buffer of their own.
    ///
    /// # Safety
    /// Every list that survives the pass has to be rewritten in it, with the `shared` that
    /// [`List::unmarked`] filled in for exactly those lists, and every other list that the
    /// pass had a hold of has to be gone by then.
    pub(crate) unsafe fn rewrite(
        &mut self,
        shared: &mut SharedBuffers,
        replace: &mut impl FnMut(Value) -> Value,
    ) -> usize {
        let state = shared.buffers.get_mut(&Rc::as_ptr(&self.buffer));

        if let Some(Shared {
            rewritten: Some(moved),
            ..
        }) = state.as_deref()
        {
            // Another list of the buffer has rewritten the elements, and may have moved them.
            let position = (self.start.as_ptr() as usize - moved.from) / size_of::<Value>();
            if let Some(buffer) = &moved.buffer {
                self.buffer = Rc::clone(buffer);
            }

            // SAFETY: the buffer holds every element that a surviving list covers,
            // `moved.dropped` positions further to the front than they were.
            self.start = unsafe {
                let items = &mut *self.buffer.items.get();
                NonNull::new_unchecked(items.as_mut_ptr().add(position - moved.dropped))
            };

            return 0;
        }

        // SAFETY: nothing adds to a buffer during a compaction pass, and nothing else is
        // looking at this one.
        let buffer = unsafe { &mut *self.buffer.items.get() };
        let position = self.position(buffer.as_ptr());
        let from = buffer.as_ptr() as usize;
        let (covered, lists) = match state.as_deref() {
            Some(state) => (state.covered.clone(), state.lists),
            None => (position..position + self.len, 1),
        };

        let moved = if Rc::strong_count(&self.buffer) > lists {
            // Something outside of the pass reads the elements too.
            let items = buffer[covered.clone()]
                .iter()
                .map(|item| replace(*item))
                .collect();

            let replacement = List::of(Buffer::new(items, self.buffer.extension));
            // SAFETY: the list lies within the elements that were copied.
            self.start = unsafe { replacement.start.add(position - covered.start) };
            self.buffer = replacement.buffer;

            Moved {
                from,
                dropped: covered.start,
                buffer: Some(Rc::clone(&self.buffer)),
            }
        } else {
            for item in &mut buffer[covered.clone()] {
                *item = replace(*item);
            }

            // What lies behind the last list that is left is given back, so that the list
            // ends where the buffer does and can be followed in place again.
            buffer.truncate(covered.end);

            let mut dropped = 0;

            if buffer.capacity() >= TRIMMED_CAPACITY
                && buffer.capacity() / TRIMMED_FACTOR >= covered.len()
            {
                buffer.drain(..covered.start);
                buffer.shrink_to_fit();
                dropped = covered.start;
            }

            // SAFETY: the list lies within the elements that were kept.
            self.start =
                unsafe { NonNull::new_unchecked(buffer.as_mut_ptr().add(position - dropped)) };

            Moved {
                from,
                dropped,
                buffer: None,
            }
        };

        if let Some(state) = state {
            state.rewritten = Some(moved);
        }

        covered.len()
    }
}

/// What a compaction pass knows about a buffer that several lists read from.
struct Shared {
    /// The positions in the buffer that the surviving lists seen so far cover, together
    /// with whatever lies between them.
    covered: Range<usize>,

    /// The number of surviving lists seen so far.
    lists: usize,

    /// Set once the elements have been rewritten.
    rewritten: Option<Moved>,
}

/// Where the elements of a rewritten buffer went.
struct Moved {
    /// The address the elements of the buffer started at before.
    from: usize,

    /// The number of elements in front of the first one that was kept.
    dropped: usize,

    /// The buffer that the elements were copied to, when they did not stay in theirs.
    buffer: Option<Rc<Buffer>>,
}

/// What a compaction pass knows about the buffers that several lists read from.
#[derive(Default)]
pub(crate) struct SharedBuffers {
    buffers: HashMap<*const Buffer, Shared, BuildHasherDefault<AddressHasher>>,
}

/// Hashes the address of a buffer, which is all a pass looks buffers up by. Addresses differ
/// in their middle bits, which one multiplication spreads over the upper half of the hash;
/// folding that half down makes the lower bits, which pick the bucket, depend on them too.
#[derive(Default)]
struct AddressHasher(u64);

impl Hasher for AddressHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 = (self.0 ^ u64::from(byte)).wrapping_mul(0x100000001b3);
        }
    }

    fn write_usize(&mut self, address: usize) {
        let mixed = (address as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
        self.0 = mixed ^ (mixed >> 32);
    }
}

impl Deref for List {
    type Target = [Value];

    #[inline(always)]
    fn deref(&self) -> &[Value] {
        self.as_slice()
    }
}

impl<'l> IntoIterator for &'l List {
    type Item = &'l Value;
    type IntoIter = std::slice::Iter<'l, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

/// A copy of a list reads the same elements, in the same buffer.
impl Clone for List {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            len: self.len,
            buffer: Rc::clone(&self.buffer),
        }
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_slice().fmt(f)
    }
}

/// Lists are equal when their elements are, wherever they are stored.
impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialEq<Vec<Value>> for List {
    fn eq(&self, other: &Vec<Value>) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialEq<[Value]> for List {
    fn eq(&self, other: &[Value]) -> bool {
        self.as_slice() == other
    }
}

impl From<Vec<Value>> for List {
    fn from(items: Vec<Value>) -> Self {
        Self::new(items)
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(items: I) -> Self {
        Self::new(items.into_iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn numbers(range: Range<usize>) -> Vec<Value> {
        range.map(|number| Value::Number(number as f64)).collect()
    }

    /// A list of the numbers in `range`, in a buffer that has no room for more.
    fn list(range: Range<usize>) -> List {
        let mut items = numbers(range);
        items.shrink_to_fit();

        List::new(items)
    }

    /// A list of the numbers in `range`, in a buffer with room for `capacity` elements.
    fn roomy(range: Range<usize>, capacity: usize) -> List {
        let mut items = Vec::with_capacity(capacity);
        items.extend(numbers(range));

        List::new(items)
    }

    fn capacity(list: &List) -> usize {
        // SAFETY: nothing else refers to the vector of the buffer.
        unsafe { (*list.buffer.items.get()).capacity() }
    }

    #[test]
    fn a_list_holds_the_items_it_was_made_of() {
        assert_eq!(list(0..3).as_slice(), numbers(0..3));
        assert_eq!(list(0..0).len(), 0);
        assert_eq!(List::from(numbers(2..5))[1], Value::Number(3.0));
        assert_eq!(numbers(0..4).into_iter().collect::<List>().len(), 4);
    }

    #[test]
    fn lists_are_equal_when_their_elements_are() {
        let whole = list(0..5);

        assert_eq!(whole.slice(1..3), list(1..3));
        assert_eq!(list(1..3), numbers(1..3));
        assert_eq!(list(1..3), numbers(1..3)[..]);
        assert_ne!(list(1..3), list(1..4));
        assert_ne!(whole.slice(0..2), whole.slice(1..3));
    }

    #[test]
    fn a_part_of_a_list_shares_its_elements() {
        let whole = list(0..10);
        let part = whole.slice(2..5);

        assert_eq!(part.as_slice(), numbers(2..5));
        assert!(part.shares_buffer_with(&whole));
        assert_eq!(part.as_ptr(), whole[2..].as_ptr());
        assert_eq!(whole.slice(10..10).len(), 0);
        assert_eq!(part.slice(1..3).as_slice(), numbers(3..5));
    }

    #[test]
    #[should_panic(expected = "lies within the list")]
    fn a_part_of_a_list_does_not_reach_beyond_it() {
        list(0..3).slice(1..4);
    }

    #[test]
    fn a_list_without_room_is_copied_when_it_is_followed() {
        let short = list(0..3);
        let longer = short.followed_by(&numbers(3..4));

        assert_eq!(short.as_slice(), numbers(0..3));
        assert_eq!(longer.as_slice(), numbers(0..4));
        assert!(!longer.shares_buffer_with(&short));
    }

    #[test]
    fn a_list_that_is_followed_once_is_copied_without_room_to_spare() {
        let longer = list(0..100).followed_by(&numbers(100..101));

        assert_eq!(capacity(&longer), 101);
    }

    #[test]
    fn a_list_with_room_is_followed_in_place() {
        let first = roomy(0..3, 8);
        let second = first.followed_by(&numbers(3..6));
        let third = second.followed_by(&[]);
        let fourth = third.followed_by(&numbers(6..8));
        let fifth = fourth.followed_by(&numbers(8..9));

        assert!(second.shares_buffer_with(&first));
        assert!(third.shares_buffer_with(&first));
        assert!(fourth.shares_buffer_with(&first));
        assert!(!fifth.shares_buffer_with(&first));
        assert_eq!(first.as_slice(), numbers(0..3));
        assert_eq!(second.as_slice(), numbers(0..6));
        assert_eq!(third.as_slice(), numbers(0..6));
        assert_eq!(fourth.as_slice(), numbers(0..8));
        assert_eq!(fifth.as_slice(), numbers(0..9));
        assert_eq!(second.as_ptr(), first.as_ptr());
    }

    #[test]
    fn a_list_that_keeps_being_followed_gets_room_to_spare() {
        let first = list(0..10).followed_by(&numbers(10..11));
        let second = first.followed_by(&numbers(11..12));
        let third = second.followed_by(&numbers(12..13));

        assert!(!second.shares_buffer_with(&first));
        assert!(third.shares_buffer_with(&second));
        assert_eq!(capacity(&second), 24);
        assert_eq!(first.as_slice(), numbers(0..11));
        assert_eq!(second.as_slice(), numbers(0..12));
        assert_eq!(third.as_slice(), numbers(0..13));
    }

    #[test]
    fn a_list_that_was_followed_before_is_copied_the_second_time() {
        let base = roomy(0..3, 8);
        let one = base.followed_by(&[Value::Bool(true)]);
        let other = base.followed_by(&[Value::Bool(false)]);

        assert!(one.shares_buffer_with(&base));
        assert!(!other.shares_buffer_with(&base));
        assert_eq!(one[3], Value::Bool(true));
        assert_eq!(other[3], Value::Bool(false));
        assert_eq!(base.as_slice(), numbers(0..3));
        assert_eq!(other[..3], numbers(0..3));

        // Only the list that ends its buffer is taken to be growing.
        assert_eq!(capacity(&other), 4);
    }

    #[test]
    fn following_grows_the_buffer_in_proportion_to_the_list() {
        let mut list = list(0..0);
        let mut buffers = 1;

        for number in 0..1000 {
            let next = list.followed_by(&[Value::Number(number as f64)]);
            if !next.shares_buffer_with(&list) {
                buffers += 1;
            }
            list = next;
        }

        assert_eq!(list.as_slice(), numbers(0..1000));
        assert!(buffers <= 11, "{buffers} buffers for 1000 elements");
        assert!(capacity(&list) < 2000);
    }

    #[test]
    fn a_list_is_followed_by_its_own_elements() {
        let once = roomy(0..3, 6);
        let twice = once.followed_by(&once);
        let again = twice.followed_by(&twice);

        assert!(twice.shares_buffer_with(&once));
        assert!(!again.shares_buffer_with(&once));
        assert_eq!(twice[..3], numbers(0..3));
        assert_eq!(twice[3..], numbers(0..3));
        assert_eq!(again[..6], twice[..]);
        assert_eq!(again[6..], twice[..]);
    }

    #[test]
    fn a_part_that_ends_its_buffer_is_followed_in_place() {
        let whole = roomy(0..5, 8);
        let tail = whole.slice(2..5);
        let longer = tail.followed_by(&numbers(5..7));
        let middle = whole.slice(1..3).followed_by(&numbers(5..6));

        assert!(longer.shares_buffer_with(&whole));
        assert_eq!(longer.as_slice(), numbers(2..7));
        assert_eq!(whole.as_slice(), numbers(0..5));
        assert!(!middle.shares_buffer_with(&whole));
        assert_eq!(
            middle.as_slice(),
            [Value::Number(1.0), Value::Number(2.0), Value::Number(5.0)]
        );
    }

    #[test]
    fn an_empty_part_at_the_end_of_a_buffer_is_followed_in_place() {
        let whole = roomy(0..3, 8);
        let nothing = whole.slice(3..3);
        let more = nothing.followed_by(&numbers(7..9));

        assert!(more.shares_buffer_with(&whole));
        assert_eq!(more.as_slice(), numbers(7..9));
        assert_eq!(whole.as_slice(), numbers(0..3));
    }

    #[test]
    fn a_queue_that_drops_from_the_front_keeps_a_buffer_of_its_own_size() {
        let mut queue = list(0..8);

        for number in 8..10_000 {
            queue = queue
                .slice(1..queue.len())
                .followed_by(&[Value::Number(number as f64)]);

            assert_eq!(queue.len(), 8);
            assert!(
                capacity(&queue) <= 16,
                "{} elements of room",
                capacity(&queue)
            );
        }

        assert_eq!(queue.as_slice(), numbers(9_992..10_000));
    }

    fn doubled(value: Value) -> Value {
        match value {
            Value::Number(number) => Value::Number(number * 2.0),
            other => other,
        }
    }

    /// A compaction pass over `lists`, of which all survive: every number is doubled.
    fn double(lists: &mut [List]) -> usize {
        let mut shared = SharedBuffers::default();
        for list in lists.iter() {
            let _ = list.unmarked(&mut shared);
        }

        lists
            .iter_mut()
            // SAFETY: every list of the pass is rewritten.
            .map(|list| unsafe { list.rewrite(&mut shared, &mut doubled) })
            .sum()
    }

    #[test]
    fn a_surviving_list_is_marked_and_rewritten_once() {
        let mut lists = [list(1..4)];
        let mut shared = SharedBuffers::default();

        assert_eq!(lists[0].unmarked(&mut shared), [&numbers(1..4)[..], &[]]);
        assert_eq!(double(&mut lists), 3);
        assert_eq!(
            lists[0].as_slice(),
            [Value::Number(2.0), Value::Number(4.0), Value::Number(6.0)]
        );
    }

    #[test]
    fn lists_that_share_a_buffer_mark_each_element_once() {
        let whole = list(0..10);
        let (front, back, middle) = (whole.slice(0..2), whole.slice(7..9), whole.slice(3..5));
        let mut shared = SharedBuffers::default();

        assert_eq!(back.unmarked(&mut shared), [&numbers(7..9)[..], &[]]);
        assert_eq!(front.unmarked(&mut shared), [&numbers(0..7)[..], &[]]);
        assert_eq!(middle.unmarked(&mut shared), [&[][..], &[]]);
        assert_eq!(whole.unmarked(&mut shared), [&[][..], &numbers(9..10)[..]]);
    }

    #[test]
    fn lists_that_share_a_buffer_are_rewritten_together() {
        let whole = list(0..6);
        let mut lists = [whole.slice(1..3), whole.slice(2..5), whole.slice(4..4)];
        drop(whole);

        assert_eq!(double(&mut lists), 4);
        assert_eq!(
            lists[0].as_slice(),
            [Value::Number(2.0), Value::Number(4.0)]
        );
        assert_eq!(
            lists[1].as_slice(),
            [Value::Number(4.0), Value::Number(6.0), Value::Number(8.0)]
        );
        assert_eq!(lists[2].len(), 0);
        assert!(lists[0].shares_buffer_with(&lists[1]));
        assert!(lists[0].shares_buffer_with(&lists[2]));
    }

    #[test]
    fn the_last_surviving_list_is_followed_in_place_after_a_pass() {
        let base = roomy(0..4, 16);
        let longer = base.followed_by(&numbers(4..6));
        let mut lists = [base];
        drop(longer);

        assert_eq!(double(&mut lists), 4);

        let again = lists[0].followed_by(&[Value::Null]);
        assert!(again.shares_buffer_with(&lists[0]));
        assert_eq!(again.len(), 5);
        assert_eq!(again[4], Value::Null);
        assert_eq!(again[3], Value::Number(6.0));
    }

    #[test]
    fn a_buffer_that_is_mostly_unused_is_trimmed_to_what_is_left() {
        let whole = list(0..1000);
        let mut lists = [whole.slice(500..510), whole.slice(505..520)];
        drop(whole);

        assert_eq!(double(&mut lists), 20);
        assert_eq!(lists[0][0], Value::Number(1000.0));
        assert_eq!(lists[0].len(), 10);
        assert_eq!(lists[1][14], Value::Number(1038.0));
        assert_eq!(lists[1].as_ptr(), lists[0][5..].as_ptr());
        assert!(
            capacity(&lists[0]) < 100,
            "{} elements of room for 20",
            capacity(&lists[0])
        );
    }

    #[test]
    fn a_trimmed_buffer_of_a_growing_list_grows_again() {
        let grown = list(0..0)
            .followed_by(&numbers(0..1))
            .followed_by(&numbers(1..100));
        let mut lists = [grown.slice(0..10)];
        drop(grown);

        assert_eq!(double(&mut lists), 10);
        assert_eq!(capacity(&lists[0]), 10);

        let longer = lists[0].followed_by(&[Value::Null]);
        assert_eq!(capacity(&longer), 22);
    }

    #[test]
    fn a_list_that_is_held_outside_of_a_pass_keeps_its_elements() {
        let whole = list(0..6);
        let held = whole.slice(1..5);
        let mut lists = [whole.slice(0..2), whole.slice(3..6), whole.slice(1..1)];
        drop(whole);

        assert_eq!(double(&mut lists), 6);

        assert_eq!(held.as_slice(), numbers(1..5));
        assert_eq!(
            lists[0].as_slice(),
            [Value::Number(0.0), Value::Number(2.0)]
        );
        assert_eq!(
            lists[1].as_slice(),
            [Value::Number(6.0), Value::Number(8.0), Value::Number(10.0)]
        );
        assert_eq!(lists[2].len(), 0);
        assert!(!lists[0].shares_buffer_with(&held));
        assert!(lists[0].shares_buffer_with(&lists[1]));
        assert!(lists[0].shares_buffer_with(&lists[2]));
        assert_eq!(lists[2].as_ptr(), lists[0][1..].as_ptr());
    }

    #[test]
    fn a_single_list_that_is_held_outside_of_a_pass_keeps_its_elements() {
        let held = roomy(0..3, 8);
        let mut lists = [held.clone()];

        assert_eq!(double(&mut lists), 3);

        assert_eq!(held.as_slice(), numbers(0..3));
        assert_eq!(
            lists[0].as_slice(),
            [Value::Number(0.0), Value::Number(2.0), Value::Number(4.0)]
        );
        assert!(!lists[0].shares_buffer_with(&held));

        // Both can still be followed without one showing up in the other.
        let one = held.followed_by(&[Value::Bool(true)]);
        let other = lists[0].followed_by(&[Value::Bool(false)]);
        assert_eq!(one[3], Value::Bool(true));
        assert_eq!(other[3], Value::Bool(false));
        assert_eq!(held.len(), 3);
    }
}
