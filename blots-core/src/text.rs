//! Text: the characters of a string, stored where they take the least to get at.
//!
//! The text of a string never changes, so two strings can read the same bytes. A text of a
//! few bytes is kept right where the string is, which takes no allocation to make and no
//! pointer to follow to read. A longer one is a run of bytes in a *buffer* that it may share
//! with other texts:
//!
//! - a part of a text is another run in the same buffer;
//! - a text that ends where its buffer ends can be followed by more text, which is written
//!   into the room the buffer has left behind it. The text that results covers the old
//!   bytes and the new ones; every text that existed before still ends where it did, and so
//!   reads what it always read.
//!
//! The second point is what makes a string that is built one piece at a time (the
//! accumulator of a `reduce`, an argument that grows from one recursive call to the next)
//! cost time in proportion to its length, not to the square of it: only when the room runs
//! out are the bytes copied, into a buffer twice as long.
//!
//! Room to spare is room that is wasted when nothing comes to fill it, so a text only gets
//! it once it shows that it keeps growing. An expression like `first + ", " + second` makes
//! texts that are as long as they have to be.

use std::{
    alloc::{self, Layout},
    borrow::Borrow,
    cell::Cell,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, Range},
    ptr::NonNull,
};

/// The most bytes a text can have to be kept where its string is: as many as make a short
/// text take the room a long one takes anyway.
pub const INLINE_CAPACITY: usize = 2 * size_of::<usize>() + 6;

/// The fewest bytes a buffer has room for when it was made for a text that keeps growing.
const MIN_GROWING_CAPACITY: usize = 64;

/// A text keeps growing once it was followed this many times in a row, each time as the
/// text that the one before resulted in.
const GROWING_AFTER: u8 = 3;

/// A part of a text shares the buffer of the text when the buffer has room for at most this
/// many times the bytes of the part. A shorter part gets a buffer of its own, so that it
/// does not keep one alive that is mostly somebody else's.
const SHARED_PART_FACTOR: usize = 4;

/// What a buffer starts with. Its bytes follow right behind.
#[repr(C)]
struct Header {
    /// The number of texts that read from the buffer.
    holders: Cell<usize>,

    /// The number of bytes that were written to the buffer. Nothing behind them is ever
    /// read, and a text only reads the ones it covers. They never change once written:
    /// the buffer is added to only where it has the room.
    len: Cell<usize>,

    /// The number of bytes the buffer has room for.
    capacity: usize,

    /// The number of times in a row that a text was followed to get to the one this buffer
    /// was made for, as far as that matters: up to [`GROWING_AFTER`]. A text that was
    /// followed that often is likely to be followed by more, so its buffer has room to
    /// spare, and so has every buffer that takes over from it.
    followed: u8,
}

/// The bytes of one or more texts.
#[derive(Clone, Copy)]
struct Buffer {
    /// The header, as a pointer that may be used for everything that was allocated: the
    /// header and the bytes behind it.
    start: NonNull<Header>,
}

impl Buffer {
    /// The memory a buffer with room for `capacity` bytes takes.
    fn layout(capacity: usize) -> Layout {
        size_of::<Header>()
            .checked_add(capacity)
            .and_then(|size| Layout::from_size_align(size, align_of::<Header>()).ok())
            .expect("a text fits into memory")
    }

    /// A buffer with room for `capacity` bytes that holds none yet, for one text to read
    /// from.
    fn new(capacity: usize, followed: u8) -> Self {
        let layout = Self::layout(capacity);

        // SAFETY: the layout is not empty, as it covers the header.
        let Some(start) = NonNull::new(unsafe { alloc::alloc(layout) }.cast::<Header>()) else {
            alloc::handle_alloc_error(layout)
        };

        // SAFETY: the memory was allocated for a header (and more).
        unsafe {
            start.write(Header {
                holders: Cell::new(1),
                len: Cell::new(0),
                capacity,
                followed,
            });
        }

        Self { start }
    }

    #[inline(always)]
    fn header(&self) -> &Header {
        // SAFETY: a buffer is alive for as long as a text reads from it, and nothing but
        // its cells ever changes in the header.
        unsafe { self.start.as_ref() }
    }

    /// Where the byte at `position` is, which has to be within the room of the buffer or
    /// right behind it.
    #[inline(always)]
    unsafe fn at(&self, position: usize) -> *mut u8 {
        // SAFETY: the bytes follow the header in the same allocation.
        unsafe {
            self.start
                .as_ptr()
                .cast::<u8>()
                .add(size_of::<Header>() + position)
        }
    }

    /// Write `text` behind the bytes that were written before.
    ///
    /// # Safety
    /// The buffer must have room for `text`.
    unsafe fn push(&self, text: &str) {
        let header = self.header();
        let len = header.len.get();
        debug_assert!(header.capacity - len >= text.len());

        // SAFETY: the room behind the written bytes is not part of any text, so it does
        // not overlap `text` even when that is read from this buffer.
        unsafe { std::ptr::copy_nonoverlapping(text.as_ptr(), self.at(len), text.len()) };
        header.len.set(len + text.len());
    }

    /// One more text reads from the buffer.
    #[inline(always)]
    fn hold(&self) {
        let header = self.header();
        header.holders.set(header.holders.get() + 1);
    }

    /// One text fewer reads from the buffer, which goes with the last one.
    #[inline(always)]
    fn release(&self) {
        let header = self.header();
        let holders = header.holders.get() - 1;
        header.holders.set(holders);

        if holders == 0 {
            let layout = Self::layout(header.capacity);

            // SAFETY: the buffer was allocated with this layout, and nothing reads from
            // it any more.
            unsafe { alloc::dealloc(self.start.as_ptr().cast(), layout) };
        }
    }
}

/// The characters of a string.
pub struct Text(Repr);

/// Where the bytes of a text are. A text of at most [`INLINE_CAPACITY`] bytes is always
/// kept inline, so two texts that are stored differently are never equal.
#[repr(u8)]
enum Repr {
    /// The first `len` of `bytes`. The others are zero.
    Inline {
        len: u8,
        bytes: [u8; INLINE_CAPACITY],
    },

    /// The `len` bytes of `buffer` from `start` on.
    Shared {
        /// Whether every character is known to be an ASCII character, which takes one byte.
        ascii: bool,
        start: u32,
        len: usize,
        buffer: Buffer,
    },
}

const _: () = assert!(size_of::<Text>() == INLINE_CAPACITY + 2);
const _: () = assert!(INLINE_CAPACITY <= u8::MAX as usize);

impl Text {
    /// The text without any characters.
    pub const EMPTY: Text = Text(Repr::Inline {
        len: 0,
        bytes: [0; INLINE_CAPACITY],
    });

    /// A text of the characters of `text`.
    #[inline]
    pub fn new(text: &str) -> Self {
        if text.len() > INLINE_CAPACITY {
            return Self::of(text, "", text.len(), 0);
        }

        // A short text is one piece, which is copied to where the text is.
        let mut bytes = [0; INLINE_CAPACITY];
        bytes[..text.len()].copy_from_slice(text.as_bytes());

        Text(Repr::Inline {
            len: text.len() as u8,
            bytes,
        })
    }

    /// `first` followed by `second`, as a text that shares its bytes with no other: a long
    /// one gets a buffer with room for `capacity` bytes.
    fn of(first: &str, second: &str, capacity: usize, followed: u8) -> Self {
        let len = first.len() + second.len();

        if len <= INLINE_CAPACITY {
            let mut bytes = [0; INLINE_CAPACITY];
            bytes[..first.len()].copy_from_slice(first.as_bytes());
            bytes[first.len()..len].copy_from_slice(second.as_bytes());

            return Text(Repr::Inline {
                len: len as u8,
                bytes,
            });
        }

        debug_assert!(capacity >= len);
        let buffer = Buffer::new(capacity, followed);

        // SAFETY: the buffer has room for both.
        unsafe {
            buffer.push(first);
            buffer.push(second);
        }

        Text(Repr::Shared {
            ascii: first.is_ascii() && second.is_ascii(),
            start: 0,
            len,
            buffer,
        })
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        // SAFETY: a text covers `len` bytes that were written, which nothing changes, and
        // which are whole characters.
        unsafe {
            str::from_utf8_unchecked(match &self.0 {
                Repr::Inline { len, bytes } => bytes.get_unchecked(..*len as usize),
                Repr::Shared {
                    start, len, buffer, ..
                } => std::slice::from_raw_parts(buffer.at(*start as usize), *len),
            })
        }
    }

    /// The number of bytes.
    #[inline(always)]
    pub fn len(&self) -> usize {
        match &self.0 {
            Repr::Inline { len, .. } => *len as usize,
            Repr::Shared { len, .. } => *len,
        }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The bytes of a text of at most eight bytes as one word, the first byte lowest and
    /// zeros for the bytes that it does not have, and the number of bytes that it has.
    #[inline(always)]
    pub(crate) fn as_word(&self) -> Option<(u64, usize)> {
        const WORD: usize = size_of::<u64>();
        const _: () = assert!(INLINE_CAPACITY >= WORD);

        match &self.0 {
            // The bytes behind those of the text are zero.
            Repr::Inline { len, bytes } if *len as usize <= WORD => {
                let mut word = [0; WORD];
                word.copy_from_slice(&bytes[..WORD]);

                Some((u64::from_le_bytes(word), *len as usize))
            }
            _ => None,
        }
    }

    /// Whether every character is known to take one byte, so that the characters are where
    /// their positions say. A text that this does not hold for may still be made of such
    /// characters only.
    #[inline(always)]
    fn is_plain(&self) -> bool {
        match &self.0 {
            Repr::Inline { bytes, .. } => bytes.is_ascii(),
            Repr::Shared { ascii, .. } => *ascii,
        }
    }

    /// The number of characters.
    pub fn char_count(&self) -> usize {
        match self.is_plain() {
            true => self.len(),
            false => self.chars().count(),
        }
    }

    /// The character at `position`, counting characters from the start.
    pub fn char(&self, position: usize) -> Option<char> {
        match self.is_plain() {
            true => self.as_bytes().get(position).map(|&byte| byte as char),
            false => self.chars().nth(position),
        }
    }

    /// The text followed by `more`. The result shares the bytes of the text when the
    /// buffer ends with the text and has room for `more` behind it.
    pub fn followed_by(&self, more: &str) -> Text {
        if more.is_empty() {
            return self.clone();
        }

        // How often in a row a text was followed to get to the result.
        let mut followed = 1;

        if let Repr::Shared {
            ascii,
            start,
            len,
            buffer,
        } = &self.0
        {
            let header = buffer.header();
            let ends_buffer = *start as usize + *len == header.len.get();

            if ends_buffer && header.capacity - header.len.get() >= more.len() {
                // The new bytes go behind everything a text covers, into room the buffer
                // already has, so the bytes it holds stay where and what they are.
                // SAFETY: the buffer has room for `more`.
                unsafe { buffer.push(more) };
                buffer.hold();

                return Text(Repr::Shared {
                    ascii: *ascii && more.is_ascii(),
                    start: *start,
                    len: *len + more.len(),
                    buffer: *buffer,
                });
            }

            // Only the text that its buffer ends with is the last of those that the
            // buffer was made for.
            if ends_buffer {
                followed = header.followed.saturating_add(1).min(GROWING_AFTER);
            }
        }

        // A text that is followed is copied as it is. One that keeps being followed, each
        // time filling the buffer it got, is growing: it gets room for as many bytes again,
        // so that the bytes are copied once per doubling and not once per piece.
        let len = self
            .len()
            .checked_add(more.len())
            .expect("a text fits into memory");
        let capacity = match followed >= GROWING_AFTER {
            true => len.saturating_mul(2).max(MIN_GROWING_CAPACITY),
            false => len,
        };

        Self::of(self, more, capacity, followed)
    }

    /// The part of the text that the bytes in `range` are, when they are one: the range
    /// lies within the text and cuts no character in two. A long part of a text shares the
    /// bytes of the text.
    pub fn part(&self, range: Range<usize>) -> Option<Text> {
        let part = self.as_str().get(range.clone())?;

        if let Repr::Shared {
            ascii,
            start,
            buffer,
            ..
        } = &self.0
            && part.len() > INLINE_CAPACITY
            && part.len() >= buffer.header().capacity / SHARED_PART_FACTOR
            && let Ok(start) = u32::try_from(*start as usize + range.start)
        {
            buffer.hold();

            return Some(Text(Repr::Shared {
                ascii: *ascii,
                start,
                len: part.len(),
                buffer: *buffer,
            }));
        }

        Some(Text::new(part))
    }

    /// The part of the text that `piece` is, which is a slice of the characters of the
    /// text. Any other piece is a text of its own.
    pub fn part_of(&self, piece: &str) -> Text {
        // Bytes that lie where the bytes of the text do are bytes of the text.
        let offset = piece.as_ptr().addr().wrapping_sub(self.as_ptr().addr());

        offset
            .checked_add(piece.len())
            .and_then(|end| self.part(offset..end))
            .unwrap_or_else(|| Text::new(piece))
    }

    /// Whether the two texts read their bytes from the same buffer.
    pub fn shares_buffer_with(&self, other: &Text) -> bool {
        match (&self.0, &other.0) {
            (Repr::Shared { buffer: own, .. }, Repr::Shared { buffer: other, .. }) => {
                own.start == other.start
            }
            _ => false,
        }
    }

    /// An estimate of the memory the text takes apart from the room it is kept in: its
    /// share of the buffer it reads from.
    pub(crate) fn held_bytes(&self) -> usize {
        match &self.0 {
            Repr::Inline { .. } => 0,
            Repr::Shared { buffer, .. } => {
                let header = buffer.header();
                let size = size_of::<Header>() + header.capacity;

                match header.holders.get() {
                    1 => size,
                    holders => size / holders,
                }
            }
        }
    }

    /// Whether the two texts are equal, when that shows without reading a buffer: it does
    /// unless both are long.
    #[inline(always)]
    pub(crate) fn equals_shortly(&self, other: &Text) -> Option<bool> {
        match (&self.0, &other.0) {
            (
                Repr::Inline { len, bytes },
                Repr::Inline {
                    len: other_len,
                    bytes: other_bytes,
                },
            ) => Some(len == other_len && bytes == other_bytes),
            (Repr::Shared { .. }, Repr::Shared { .. }) => None,
            _ => Some(false),
        }
    }
}

impl Drop for Text {
    #[inline]
    fn drop(&mut self) {
        if let Repr::Shared { buffer, .. } = &self.0 {
            buffer.release();
        }
    }
}

/// A copy of a text reads the same bytes, in the same buffer.
impl Clone for Text {
    #[inline]
    fn clone(&self) -> Self {
        Text(match &self.0 {
            Repr::Inline { len, bytes } => Repr::Inline {
                len: *len,
                bytes: *bytes,
            },
            Repr::Shared {
                ascii,
                start,
                len,
                buffer,
            } => {
                buffer.hold();

                Repr::Shared {
                    ascii: *ascii,
                    start: *start,
                    len: *len,
                    buffer: *buffer,
                }
            }
        })
    }
}

impl Default for Text {
    fn default() -> Self {
        Self::EMPTY
    }
}

impl Deref for Text {
    type Target = str;

    #[inline(always)]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// A text stands in for its characters where it is a key: it hashes and compares as they
/// do.
impl Borrow<str> for Text {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Hash for Text {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

/// Texts are equal when their characters are, wherever they are stored.
impl PartialEq for Text {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match self.equals_shortly(other) {
            Some(equal) => equal,
            None => self.as_bytes() == other.as_bytes(),
        }
    }
}

impl Eq for Text {}

impl PartialEq<str> for Text {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for Text {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<String> for Text {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialOrd for Text {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Text {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl From<&str> for Text {
    fn from(text: &str) -> Self {
        Self::new(text)
    }
}

impl From<String> for Text {
    fn from(text: String) -> Self {
        Self::new(&text)
    }
}

impl From<&String> for Text {
    fn from(text: &String) -> Self {
        Self::new(text)
    }
}

impl From<char> for Text {
    fn from(character: char) -> Self {
        let mut bytes = [0; INLINE_CAPACITY];
        let len = character.encode_utf8(&mut bytes).len();

        Text(Repr::Inline {
            len: len as u8,
            bytes,
        })
    }
}

impl From<Text> for String {
    fn from(text: Text) -> Self {
        text.as_str().to_string()
    }
}

/// The most bytes that a [`Draft`] holds where it is.
const DRAFT_ROOM: usize = 120;

/// A text that is being written, one piece behind the other. The pieces are kept where the
/// draft is for as long as they fit there, so a text that is put together from a few short
/// pieces takes no memory to write.
pub(crate) struct Draft(Pieces);

/// Where the pieces of a draft are.
enum Pieces {
    /// The first `len` of `bytes`.
    Here { len: usize, bytes: [u8; DRAFT_ROOM] },

    /// Pieces that outgrew the room a draft has for them.
    Elsewhere(String),
}

impl Draft {
    /// A draft without any pieces.
    #[inline(always)]
    pub(crate) fn new() -> Self {
        Draft(Pieces::Here {
            len: 0,
            bytes: [0; DRAFT_ROOM],
        })
    }

    /// Write `piece` behind the pieces that were written before.
    #[inline]
    pub(crate) fn push_str(&mut self, piece: &str) {
        match &mut self.0 {
            Pieces::Here { len, bytes } => match bytes.get_mut(*len..*len + piece.len()) {
                Some(room) => {
                    room.copy_from_slice(piece.as_bytes());
                    *len += piece.len();
                }
                None => self.outgrow(piece),
            },
            Pieces::Elsewhere(pieces) => pieces.push_str(piece),
        }
    }

    /// Write `piece` behind the pieces that were written before, which leave no room for it
    /// where the draft is.
    #[cold]
    #[inline(never)]
    fn outgrow(&mut self, piece: &str) {
        let written = self.as_str();
        let mut pieces = String::with_capacity((written.len() + piece.len()).max(2 * DRAFT_ROOM));

        pieces.push_str(written);
        pieces.push_str(piece);

        self.0 = Pieces::Elsewhere(pieces);
    }

    /// The pieces that were written, one behind the other.
    #[inline]
    pub(crate) fn as_str(&self) -> &str {
        match &self.0 {
            // SAFETY: the first `len` bytes are those of the pieces, in the order in which
            // they were written, and each piece is whole characters.
            Pieces::Here { len, bytes } => unsafe { str::from_utf8_unchecked(&bytes[..*len]) },
            Pieces::Elsewhere(pieces) => pieces,
        }
    }
}

impl From<&Draft> for Text {
    fn from(draft: &Draft) -> Self {
        Self::new(draft.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashMap, hash_map::DefaultHasher};

    /// `len` characters that count up from `first`, each of one byte.
    fn letters(first: usize, len: usize) -> String {
        (first..first + len)
            .map(|index| (b'a' + (index % 26) as u8) as char)
            .collect()
    }

    /// A text of `len` characters in a buffer that has no room for more.
    fn long(len: usize) -> Text {
        assert!(len > INLINE_CAPACITY);

        Text::new(&letters(0, len))
    }

    /// A text of `len` characters in a buffer that has room for `capacity` of them.
    fn roomy(len: usize, capacity: usize) -> Text {
        assert!(len > INLINE_CAPACITY && capacity >= len);
        let text = letters(0, len);

        Text::of(&text, "", capacity, 1)
    }

    /// `text` after it was followed by nothing `times` times in a row, as far as its
    /// buffer goes: the result has the characters of `text`.
    fn followed(text: &str, times: u8) -> Text {
        assert!(text.len() > INLINE_CAPACITY);

        Text::of(text, "", text.len(), times)
    }

    fn capacity(text: &Text) -> usize {
        match &text.0 {
            Repr::Inline { .. } => panic!("the text has no buffer"),
            Repr::Shared { buffer, .. } => buffer.header().capacity,
        }
    }

    fn holders(text: &Text) -> usize {
        match &text.0 {
            Repr::Inline { .. } => panic!("the text has no buffer"),
            Repr::Shared { buffer, .. } => buffer.header().holders.get(),
        }
    }

    fn is_inline(text: &Text) -> bool {
        matches!(text.0, Repr::Inline { .. })
    }

    fn hash_of(value: &(impl Hash + ?Sized)) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);

        hasher.finish()
    }

    #[test]
    fn a_text_holds_the_characters_it_was_made_of() {
        for len in [
            0,
            1,
            5,
            INLINE_CAPACITY - 1,
            INLINE_CAPACITY,
            INLINE_CAPACITY + 1,
            100,
            5000,
        ] {
            let characters = letters(3, len);
            let text = Text::new(&characters);

            assert_eq!(text.as_str(), characters);
            assert_eq!(text.len(), len);
            assert_eq!(text.is_empty(), len == 0);
            assert_eq!(is_inline(&text), len <= INLINE_CAPACITY);
            assert_eq!(Text::from(characters.clone()), text);
            assert_eq!(Text::from(&characters), text);
            assert_eq!(String::from(text), characters);
        }

        assert_eq!(Text::EMPTY.as_str(), "");
        assert_eq!(Text::default().as_str(), "");
        assert_eq!(Text::from('x').as_str(), "x");
        assert_eq!(Text::from('é').as_str(), "é");
        assert_eq!(Text::from('😀').as_str(), "😀");
    }

    #[test]
    fn a_text_is_as_large_as_its_longest_short_form() {
        assert_eq!(size_of::<Text>(), INLINE_CAPACITY + 2);
        assert_eq!(size_of::<Text>() % align_of::<Text>(), 0);
    }

    #[test]
    fn texts_are_equal_when_their_characters_are() {
        let short = Text::new("abc");
        let long_text = long(40);
        let part = long(60).part(0..40).unwrap();

        assert_eq!(short, Text::new("abc"));
        assert_ne!(short, Text::new("abd"));
        assert_ne!(short, Text::new("ab"));
        assert_ne!(short, Text::new("abc\0"));
        assert_ne!(short, long_text);
        assert_ne!(long_text, short);
        assert_eq!(long_text, part);
        assert!(!is_inline(&part));
        assert_ne!(long_text, long(41));
        assert_ne!(long_text, Text::new(&letters(1, 40)));
        assert_eq!(short, "abc");
        assert_eq!(short, *"abc");
        assert_eq!(short, "abc".to_string());
        assert_eq!(Text::EMPTY, Text::new(""));
    }

    #[test]
    fn short_texts_are_compared_without_reading_a_buffer() {
        let short = Text::new("abc");
        let long_text = long(40);

        assert_eq!(short.equals_shortly(&Text::new("abc")), Some(true));
        assert_eq!(short.equals_shortly(&Text::new("abd")), Some(false));
        assert_eq!(short.equals_shortly(&Text::new("abcd")), Some(false));
        assert_eq!(short.equals_shortly(&long_text), Some(false));
        assert_eq!(long_text.equals_shortly(&short), Some(false));
        assert_eq!(long_text.equals_shortly(&long(40)), None);
    }

    #[test]
    fn a_text_stands_in_for_its_characters_as_a_key() {
        let mut numbers: HashMap<Text, usize> = HashMap::new();

        for len in [0, 3, INLINE_CAPACITY, INLINE_CAPACITY + 1, 300] {
            let characters = letters(0, len);
            let text = Text::new(&characters);

            assert_eq!(hash_of(&text), hash_of(characters.as_str()));
            numbers.insert(text, len);
            assert_eq!(numbers.get(characters.as_str()), Some(&len));
        }

        assert_eq!(numbers.get("missing"), None);
    }

    #[test]
    fn texts_are_ordered_as_their_characters_are() {
        let mut texts = [
            long(30),
            Text::new("b"),
            Text::new(""),
            Text::new("abd"),
            long(31),
            Text::new("é"),
        ];
        texts.sort();

        let mut characters: Vec<String> = texts.iter().map(|text| text.to_string()).collect();
        let sorted = characters.clone();
        characters.sort();

        assert_eq!(sorted, characters);
        assert!(Text::new("a") < Text::new("b"));
        assert_eq!(
            Text::new("a").partial_cmp(&Text::new("a")),
            Some(Ordering::Equal)
        );
    }

    #[test]
    fn a_text_shows_as_its_characters_do() {
        let text = Text::new("a \"b\"\n");

        assert_eq!(format!("{}", text), "a \"b\"\n");
        assert_eq!(format!("{:?}", text), format!("{:?}", "a \"b\"\n"));
        assert_eq!(format!("{:>5}", Text::new("ab")), "   ab");
    }

    #[test]
    fn a_short_text_that_is_followed_stays_short_while_it_can() {
        let mut text = Text::EMPTY;

        for len in 1..=INLINE_CAPACITY {
            text = text.followed_by("x");
            assert!(is_inline(&text));
            assert_eq!(text.len(), len);
        }

        let longer = text.followed_by("y");
        assert!(!is_inline(&longer));
        assert_eq!(longer.as_str(), "x".repeat(INLINE_CAPACITY) + "y");
        assert_eq!(text.as_str(), "x".repeat(INLINE_CAPACITY));
    }

    #[test]
    fn a_text_that_is_followed_by_nothing_stays_what_it_is() {
        let short = Text::new("abc");
        let long_text = long(50);

        assert_eq!(short.followed_by(""), short);
        assert!(long_text.followed_by("").shares_buffer_with(&long_text));
        assert_eq!(Text::EMPTY.followed_by("abc"), short);
        assert_eq!(Text::EMPTY.followed_by(&long_text), long_text);
    }

    #[test]
    fn a_text_without_room_is_copied_when_it_is_followed() {
        let text = long(30);
        let longer = text.followed_by("xyz");

        assert_eq!(text.as_str(), letters(0, 30));
        assert_eq!(longer.as_str(), letters(0, 30) + "xyz");
        assert!(!longer.shares_buffer_with(&text));
    }

    #[test]
    fn a_text_that_is_followed_is_copied_without_room_to_spare() {
        let once = long(100).followed_by("x");
        let twice = once.followed_by("y");

        assert_eq!(capacity(&once), 101);
        assert_eq!(capacity(&twice), 102);
        assert_eq!(twice.as_str(), letters(0, 100) + "xy");
        assert_eq!(
            capacity(&Text::new("abc").followed_by(&letters(0, 100))),
            103
        );
    }

    #[test]
    fn a_text_with_room_is_followed_in_place() {
        let first = roomy(30, 80);
        let second = first.followed_by(&letters(30, 30));
        let third = second.followed_by("");
        let fourth = third.followed_by(&letters(60, 20));
        let fifth = fourth.followed_by("!");

        assert!(second.shares_buffer_with(&first));
        assert!(third.shares_buffer_with(&first));
        assert!(fourth.shares_buffer_with(&first));
        assert!(!fifth.shares_buffer_with(&first));
        assert_eq!(first.as_str(), letters(0, 30));
        assert_eq!(second.as_str(), letters(0, 60));
        assert_eq!(third.as_str(), letters(0, 60));
        assert_eq!(fourth.as_str(), letters(0, 80));
        assert_eq!(fifth.as_str(), letters(0, 80) + "!");
        assert_eq!(second.as_ptr(), first.as_ptr());
        assert_eq!(holders(&first), 4);
    }

    #[test]
    fn a_text_that_keeps_being_followed_gets_room_to_spare() {
        let first = long(100).followed_by("a");
        let second = first.followed_by("b");
        let third = second.followed_by("c");
        let fourth = third.followed_by("d");

        assert!(!second.shares_buffer_with(&first));
        assert!(!third.shares_buffer_with(&second));
        assert!(fourth.shares_buffer_with(&third));
        assert_eq!(capacity(&third), 206);
        assert_eq!(first.as_str(), letters(0, 100) + "a");
        assert_eq!(second.as_str(), letters(0, 100) + "ab");
        assert_eq!(third.as_str(), letters(0, 100) + "abc");
        assert_eq!(fourth.as_str(), letters(0, 100) + "abcd");
    }

    #[test]
    fn a_growing_text_keeps_getting_room_to_spare() {
        let text = followed(&letters(0, 100), GROWING_AFTER);
        let longer = text.followed_by("a");
        let full = longer.followed_by(&letters(0, 101));
        let next = full.followed_by("b");

        assert_eq!(capacity(&longer), 202);
        assert!(full.shares_buffer_with(&longer));
        assert!(!next.shares_buffer_with(&full));
        assert_eq!(capacity(&next), 406);

        // However long it has been growing.
        let mut text = followed(&letters(0, 100), u8::MAX);
        for _ in 0..3 {
            text = text.followed_by(&letters(0, text.len() + 1));
            assert_eq!(capacity(&text), 2 * text.len());
        }
    }

    #[test]
    fn a_growing_text_starts_out_with_some_room() {
        let text = Text::new(&"x".repeat(INLINE_CAPACITY))
            .followed_by("y")
            .followed_by("z")
            .followed_by("!");

        assert_eq!(capacity(&text), MIN_GROWING_CAPACITY);
        assert!(text.followed_by("?").shares_buffer_with(&text));
    }

    #[test]
    fn a_text_that_was_followed_before_is_copied_the_second_time() {
        let base = roomy(30, 80);
        let one = base.followed_by("one");
        let other = base.followed_by("other");

        assert!(one.shares_buffer_with(&base));
        assert!(!other.shares_buffer_with(&base));
        assert_eq!(one.as_str(), letters(0, 30) + "one");
        assert_eq!(other.as_str(), letters(0, 30) + "other");
        assert_eq!(base.as_str(), letters(0, 30));

        // Only the text that ends its buffer is taken to be growing.
        assert_eq!(capacity(&other), 35);
    }

    #[test]
    fn following_grows_the_buffer_in_proportion_to_the_text() {
        let mut text = Text::EMPTY;
        let mut buffers = 0;
        let mut expected = String::new();

        for number in 0..5000 {
            let piece = (number % 10).to_string();
            let next = text.followed_by(&piece);
            if !is_inline(&next) && !next.shares_buffer_with(&text) {
                buffers += 1;
            }
            text = next;
            expected.push_str(&piece);
        }

        assert_eq!(text.as_str(), expected);
        assert!(buffers <= 10, "{buffers} buffers for 5000 bytes");
        assert!(capacity(&text) < 10000);
    }

    #[test]
    fn a_text_is_followed_by_its_own_characters() {
        let once = roomy(30, 60);
        let twice = once.followed_by(&once);
        let again = twice.followed_by(&twice);

        assert!(twice.shares_buffer_with(&once));
        assert!(!again.shares_buffer_with(&once));
        assert_eq!(twice.as_str(), letters(0, 30).repeat(2));
        assert_eq!(again.as_str(), letters(0, 30).repeat(4));
    }

    #[test]
    fn a_long_part_of_a_text_shares_its_bytes() {
        let whole = long(100);
        let part = whole.part(20..70).unwrap();
        let inner = part.part(10..40).unwrap();

        assert_eq!(part.as_str(), letters(20, 50));
        assert!(part.shares_buffer_with(&whole));
        assert_eq!(part.as_ptr(), whole[20..].as_ptr());
        assert_eq!(inner.as_str(), letters(30, 30));
        assert!(inner.shares_buffer_with(&whole));
        assert_eq!(whole.part(0..100).unwrap(), whole);
        assert_eq!(holders(&whole), 3);
    }

    #[test]
    fn a_short_part_of_a_text_gets_room_of_its_own() {
        let whole = long(1000);
        let short = whole.part(500..510).unwrap();
        let small = whole
            .part(500..500 + 1000 / SHARED_PART_FACTOR - 1)
            .unwrap();
        let large = whole.part(500..500 + 1000 / SHARED_PART_FACTOR).unwrap();

        assert!(is_inline(&short));
        assert_eq!(short.as_str(), letters(500, 10));
        assert!(!small.shares_buffer_with(&whole));
        assert_eq!(small.as_str(), letters(500, 249));
        assert_eq!(capacity(&small), 249);
        assert!(large.shares_buffer_with(&whole));
        assert_eq!(whole.part(3..3).unwrap(), Text::EMPTY);
        assert_eq!(Text::new("abcdef").part(2..4).unwrap(), "cd");
    }

    #[test]
    fn a_part_lies_within_its_text_and_cuts_no_character() {
        let short = Text::new("aé😀b");
        let long_text = Text::new(&"é".repeat(40));

        assert_eq!(short.part(0..1).unwrap(), "a");
        assert_eq!(short.part(1..3).unwrap(), "é");
        assert_eq!(short.part(3..7).unwrap(), "😀");
        assert!(short.part(1..2).is_none());
        assert!(short.part(2..3).is_none());
        assert!(short.part(4..8).is_none());
        assert!(short.part(0..9).is_none());
        assert!(short.part(9..9).is_none());
        assert_eq!(short.part(8..8).unwrap(), "");
        #[allow(clippy::reversed_empty_ranges)]
        let backwards = short.part(3..1);
        assert!(backwards.is_none());

        assert_eq!(long_text.part(2..80).unwrap().as_str(), "é".repeat(39));
        assert!(long_text.part(1..80).is_none());
        assert!(long_text.part(0..79).is_none());
        assert!(long_text.part(0..82).is_none());
    }

    #[test]
    fn a_slice_of_a_text_is_the_part_it_reads() {
        let whole = long(100);
        let short = Text::new("  ab cd ");

        let part = whole.part_of(&whole[20..70]);
        assert!(part.shares_buffer_with(&whole));
        assert_eq!(part.as_str(), letters(20, 50));
        assert_eq!(part.as_ptr(), whole[20..].as_ptr());

        assert_eq!(whole.part_of(&whole[40..45]).as_str(), letters(40, 5));
        assert!(whole.part_of(&whole).shares_buffer_with(&whole));
        assert_eq!(whole.part_of(&whole[100..]), Text::EMPTY);
        assert_eq!(whole.part_of(&whole[..0]), Text::EMPTY);
        assert_eq!(short.part_of(short.trim()).as_str(), "ab cd");
        assert_eq!(short.part_of(&short[7..]).as_str(), " ");

        for piece in whole.split('e') {
            assert_eq!(whole.part_of(piece).as_str(), piece);
        }
    }

    #[test]
    fn a_piece_from_elsewhere_is_a_text_of_its_own() {
        let whole = long(100);
        let short = Text::new("abcdef");
        let other = letters(0, 100);

        let copy = whole.part_of(&other[20..70]);
        assert!(!copy.shares_buffer_with(&whole));
        assert_eq!(copy.as_str(), letters(20, 50));

        assert_eq!(whole.part_of("").as_str(), "");
        assert_eq!(whole.part_of("abc").as_str(), "abc");
        assert_eq!(short.part_of("cd").as_str(), "cd");
        assert_eq!(short.part_of(&other).as_str(), other);
        assert_eq!(short.part_of(&whole).as_str(), whole.as_str());
        assert_eq!(Text::EMPTY.part_of("abc").as_str(), "abc");
        assert_eq!(Text::EMPTY.part_of("").as_str(), "");

        // A slice that reaches beyond the text is not a part of it, even where it reads
        // the same buffer.
        let base = roomy(50, 80);
        let longer = base.followed_by("xyz");
        assert!(longer.shares_buffer_with(&base));
        assert_eq!(
            base.part_of(&longer[40..]).as_str(),
            letters(40, 10) + "xyz"
        );
        assert_eq!(base.part_of(&longer[50..]).as_str(), "xyz");
        assert_eq!(base.part_of(&longer[51..52]).as_str(), "y");
        assert!(longer.part_of(&base).shares_buffer_with(&base));
    }

    #[test]
    fn a_part_that_ends_its_buffer_is_followed_in_place() {
        let whole = roomy(50, 80);
        let end = whole.part(20..50).unwrap();
        let longer = end.followed_by("xyz");
        let middle = whole.part(10..40).unwrap().followed_by("!");

        assert!(longer.shares_buffer_with(&whole));
        assert_eq!(longer.as_str(), letters(20, 30) + "xyz");
        assert_eq!(whole.as_str(), letters(0, 50));
        assert!(!middle.shares_buffer_with(&whole));
        assert_eq!(middle.as_str(), letters(10, 30) + "!");

        // The text itself no longer ends the buffer.
        let other = whole.followed_by("123");
        assert!(!other.shares_buffer_with(&whole));
        assert_eq!(other.as_str(), letters(0, 50) + "123");
        assert_eq!(longer.as_str(), letters(20, 30) + "xyz");
    }

    #[test]
    fn a_text_that_drops_from_the_front_keeps_a_buffer_of_its_own_size() {
        let mut queue = long(40);

        for number in 0..10_000 {
            queue = queue
                .part(1..queue.len())
                .unwrap()
                .followed_by(&(number % 10).to_string());

            assert_eq!(queue.len(), 40);
            assert!(
                capacity(&queue) <= 160,
                "{} bytes of room",
                capacity(&queue)
            );
        }
    }

    #[test]
    fn the_characters_of_a_plain_text_are_where_their_positions_say() {
        for len in [0, 1, INLINE_CAPACITY, INLINE_CAPACITY + 1, 500] {
            let characters = letters(0, len);
            let text = Text::new(&characters);

            assert!(text.is_plain());
            assert_eq!(text.char_count(), len);
            for position in [0, 1, len / 2, len.saturating_sub(1), len, len + 1] {
                assert_eq!(text.char(position), characters.chars().nth(position));
            }
            assert_eq!(text.char(usize::MAX), None);
        }
    }

    #[test]
    fn the_characters_of_other_texts_are_counted() {
        for characters in [
            "é".to_string(),
            "aé😀b".to_string(),
            "日本語".repeat(20),
            letters(0, 40) + "é" + &letters(0, 40),
            "\u{7f}\u{80}".repeat(30),
        ] {
            let text = Text::new(&characters);
            let count = characters.chars().count();

            assert!(!text.is_plain());
            assert_eq!(text.char_count(), count);
            for position in 0..count + 2 {
                assert_eq!(text.char(position), characters.chars().nth(position));
            }
        }
    }

    #[test]
    fn a_text_stays_plain_only_while_everything_that_follows_is() {
        let plain = roomy(30, 80).followed_by("b").followed_by("c");
        let more = plain.followed_by("d");
        let other = more.followed_by("é");
        let after = other.followed_by("e");

        assert!(more.shares_buffer_with(&plain));
        assert!(other.shares_buffer_with(&plain));
        assert!(plain.is_plain() && more.is_plain());
        assert!(!other.is_plain() && !after.is_plain());
        assert_eq!(other.char(33), Some('é'));
        assert_eq!(after.char(34), Some('e'));
        assert_eq!(after.char_count(), 35);
        assert_eq!(more.char(32), Some('d'));

        // A part of a text that is not plain is not taken to be, wherever it lies.
        let part = after.part(0..30).unwrap();
        assert!(part.shares_buffer_with(&after));
        assert_eq!(part.char(29), letters(0, 30).chars().nth(29));
        assert_eq!(part.char_count(), 30);

        // Short texts tell by their bytes.
        assert!(Text::new("abc").is_plain());
        assert!(!Text::new("abé").is_plain());
        assert!(!Text::new("abc").followed_by("é").is_plain());
        assert!(!long(30).followed_by("é").is_plain());
        assert!(!Text::new("é").followed_by(&letters(0, 40)).is_plain());
    }

    #[test]
    fn a_text_accounts_for_its_share_of_its_buffer() {
        let text = long(100);
        assert_eq!(Text::new("abc").held_bytes(), 0);
        assert_eq!(text.held_bytes(), size_of::<Header>() + 100);

        let copy = text.clone();
        assert_eq!(text.held_bytes(), (size_of::<Header>() + 100) / 2);
        assert_eq!(copy.held_bytes(), text.held_bytes());
    }

    #[test]
    fn a_buffer_is_read_from_until_its_last_text_is_gone() {
        let whole = roomy(40, 100);
        let copy = whole.clone();
        let part = whole.part(5..35).unwrap();
        let longer = whole.followed_by("more");
        assert_eq!(holders(&whole), 4);

        drop(whole);
        assert_eq!(holders(&copy), 3);
        drop(copy);
        drop(longer);
        assert_eq!(holders(&part), 1);
        assert_eq!(part.as_str(), letters(5, 30));

        let again = part.clone();
        drop(part);
        assert_eq!(again.as_str(), letters(5, 30));
    }

    fn is_here(draft: &Draft) -> bool {
        matches!(draft.0, Pieces::Here { .. })
    }

    #[test]
    fn a_draft_holds_its_pieces_one_behind_the_other() {
        let draft = Draft::new();
        assert_eq!(draft.as_str(), "");
        assert!(is_here(&draft));
        assert_eq!(Text::from(&draft), Text::EMPTY);

        // Pieces of every length, up to where they leave the draft and beyond.
        for piece_len in [0, 1, 2, 7, 22, 23, 59, 60, 61, 119, 120, 121, 500] {
            let mut draft = Draft::new();
            let mut written = String::new();

            for index in 0..12 {
                let piece = letters(index, piece_len);

                draft.push_str(&piece);
                written.push_str(&piece);

                assert_eq!(draft.as_str(), written, "{piece_len} at a time");
                assert_eq!(is_here(&draft), written.len() <= DRAFT_ROOM);

                let text = Text::from(&draft);
                assert_eq!(text.as_str(), written);
                assert_eq!(is_inline(&text), written.len() <= INLINE_CAPACITY);
            }
        }
    }

    #[test]
    fn a_draft_that_is_full_takes_nothing_and_then_leaves_where_it_is() {
        let full = letters(0, DRAFT_ROOM);
        let mut draft = Draft::new();

        draft.push_str(&full);
        assert!(is_here(&draft));
        draft.push_str("");
        assert!(is_here(&draft));
        assert_eq!(draft.as_str(), full);

        draft.push_str("é");
        assert!(!is_here(&draft));
        assert_eq!(draft.as_str(), format!("{full}é"));

        draft.push_str("");
        draft.push_str(&full);
        assert_eq!(draft.as_str(), format!("{full}é{full}"));
    }

    #[test]
    fn a_draft_keeps_characters_of_several_bytes_whole() {
        // The characters cross the end of the room of a draft at every byte of theirs.
        for lead in 0..4 {
            let mut draft = Draft::new();
            let mut written = letters(0, DRAFT_ROOM - 8 + lead);
            draft.push_str(&written);

            for piece in ["é", "😀", "ü", "日本", "x"] {
                draft.push_str(piece);
                written.push_str(piece);

                assert_eq!(draft.as_str(), written);
                assert_eq!(Text::from(&draft).as_str(), written);
            }

            assert!(!is_here(&draft));
        }
    }
}
