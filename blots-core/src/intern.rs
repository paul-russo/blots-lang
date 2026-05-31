//! Interned identifier names.
//!
//! Binding names (identifiers, assignment targets, lambda parameters) are interned into
//! [`Symbol`]s at parse time, so evaluation never allocates or hashes full strings when it
//! creates call frames or looks up variables: a `Symbol` is a pointer to a unique, leaked
//! string, equality is a pointer comparison, and hashing is a single integer mix.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::sync::{LazyLock, Mutex, OnceLock};

/// Registry of every string that has been interned. Entries are leaked so that symbols can
/// hand out `&'static str` without any further locking; the set of distinct identifier names
/// in a program is small and bounded, so the leak is intentional and harmless.
static INTERNER: LazyLock<Mutex<HashSet<&'static str>>> =
    LazyLock::new(|| Mutex::new(HashSet::new()));

/// An interned identifier name.
///
/// Two symbols interned from the same text always wrap the same leaked allocation, so
/// equality and hashing operate on the pointer rather than the string contents. Symbols can
/// only be constructed through [`Symbol::intern`] (or deserialization), which preserves that
/// invariant.
#[derive(Clone, Copy)]
pub struct Symbol(&'static str);

impl Symbol {
    /// Intern `text`, returning the canonical symbol for it.
    pub fn intern(text: &str) -> Symbol {
        let mut interner = INTERNER.lock().unwrap();
        if let Some(existing) = interner.get(text) {
            return Symbol(existing);
        }

        let leaked: &'static str = Box::leak(text.to_owned().into_boxed_str());
        interner.insert(leaked);
        Symbol(leaked)
    }

    /// The interned text. Free of locks or lookups: the string is owned by the interner for
    /// the life of the process.
    pub fn as_str(&self) -> &'static str {
        self.0
    }
}

/// The interned name of the implicit `inputs` binding, cached because the function-call path
/// re-binds it on every call and should not pay for an interner lookup each time.
pub fn inputs_symbol() -> Symbol {
    static INPUTS: OnceLock<Symbol> = OnceLock::new();
    *INPUTS.get_or_init(|| Symbol::intern("inputs"))
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        // Interning guarantees one allocation per distinct string, so pointer identity is
        // equivalent to (and much cheaper than) comparing contents.
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl Eq for Symbol {}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Ordering is lexicographic (unlike equality, which is pointer-based) so that sorting
        // and comparing argument lists behaves the same as it did with plain strings.
        self.0.cmp(other.0)
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ptr() as usize);
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Serialize for Symbol {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.0)
    }
}

impl<'de> Deserialize<'de> for Symbol {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let text = String::deserialize(deserializer)?;
        Ok(Symbol::intern(&text))
    }
}

/// Hasher specialized for `Symbol` keys. A symbol is already a unique pointer, so a single
/// multiplicative mix of its address is enough; this avoids SipHash entirely on the hot
/// variable-binding and lookup paths.
#[derive(Default)]
pub struct SymbolHasher(u64);

impl Hasher for SymbolHasher {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        // Fallback for callers that hash arbitrary bytes through this hasher (FNV-1a); the
        // Symbol Hash impl itself always goes through write_usize below.
        for &byte in bytes {
            self.0 = (self.0 ^ u64::from(byte)).wrapping_mul(0x100000001b3);
        }
    }

    fn write_usize(&mut self, i: usize) {
        self.0 = (i as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    }
}

/// A hash map keyed by interned symbols, used for variable bindings and captured scopes.
pub type SymbolMap<V> = HashMap<Symbol, V, BuildHasherDefault<SymbolHasher>>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interning_is_idempotent() {
        let a = Symbol::intern("some_identifier");
        let b = Symbol::intern("some_identifier");
        let c = Symbol::intern("other_identifier");

        assert_eq!(a, b);
        assert_eq!(a.as_str().as_ptr(), b.as_str().as_ptr());
        assert_ne!(a, c);
    }

    #[test]
    fn ordering_is_lexicographic() {
        let a = Symbol::intern("apple");
        let b = Symbol::intern("banana");
        assert!(a < b);
    }

    #[test]
    fn serde_round_trips_as_string() {
        let symbol = Symbol::intern("x");
        let json = serde_json::to_string(&symbol).unwrap();
        assert_eq!(json, "\"x\"");

        let back: Symbol = serde_json::from_str(&json).unwrap();
        assert_eq!(back, symbol);
    }

    #[test]
    fn symbol_map_lookups_work() {
        let mut map: SymbolMap<i32> = SymbolMap::default();
        map.insert(Symbol::intern("x"), 1);
        map.insert(Symbol::intern("y"), 2);

        assert_eq!(map.get(&Symbol::intern("x")), Some(&1));
        assert_eq!(map.get(&Symbol::intern("y")), Some(&2));
        assert_eq!(map.get(&Symbol::intern("z")), None);
    }
}
