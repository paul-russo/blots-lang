use anyhow::{Result, anyhow};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{
    cell::{Cell, OnceCell, RefCell},
    fmt::Display,
    rc::Rc,
};

use crate::{
    ast::SpannedExpr,
    bytecode::Proto,
    expressions::truncate,
    functions::BuiltInFunction,
    heap::{
        Heap, HeapPointer, HeapValue, IterablePointer, LambdaPointer, ListPointer, RecordPointer,
        StringPointer,
    },
    intern::Symbol,
    list::List,
    record::Record,
    text::Text,
};

/// The most characters that [`whole_digits`] writes: the digits of the largest whole number
/// below 2^53, and the sign of a negative one.
pub(crate) const WHOLE_DIGITS: usize = 17;

/// What `Display` writes for `number`, when that is a whole number below 2^53 in magnitude
/// and not a zero with a sign: its digits, behind the sign of a negative one. They are
/// written to the end of `room`.
///
/// `Display` looks for the fewest digits that tell a number from the ones next to it, which
/// for such a number are all of its own: the numbers next to it are whole as well.
#[inline]
pub(crate) fn whole_digits(number: f64, room: &mut [u8; WHOLE_DIGITS]) -> Option<&str> {
    /// The first whole number that has numbers next to it that are more than one apart.
    const LIMIT: u64 = 1 << 53;

    let whole = truncate(number);
    let magnitude = whole.unsigned_abs();

    // A fraction, a zero with a sign and what is no number have other bits than the whole
    // number that they convert to.
    if magnitude >= LIMIT || (whole as f64).to_bits() != number.to_bits() {
        return None;
    }

    let mut start = room.len();
    let mut rest = magnitude;

    loop {
        start -= 1;
        room[start] = b'0' + (rest % 10) as u8;
        rest /= 10;

        if rest == 0 {
            break;
        }
    }

    if whole < 0 {
        start -= 1;
        room[start] = b'-';
    }

    // SAFETY: everything from `start` on was written, and is a digit or the sign.
    Some(unsafe { str::from_utf8_unchecked(&room[start..]) })
}

/// What `to_string` makes of a number, which is what `Display` writes for it.
pub(crate) fn plain_number(number: f64) -> String {
    match whole_digits(number, &mut [0; WHOLE_DIGITS]) {
        Some(digits) => digits.to_owned(),
        None => number.to_string(),
    }
}

/// Format a number for display output, matching the JS displayNumber function:
/// - Very small values (< 0.0001) or extremely large values (>= 1e15) use scientific notation
/// - Standard numbers preserve up to 15 significant digits with thousand separators
pub fn format_display_number(value: f64) -> String {
    // Handle special values
    if value.is_nan() {
        return "NaN".to_string();
    }
    if value.is_infinite() {
        return if value.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        };
    }
    // Handle zero specially to avoid "0e0" from scientific notation path
    if value == 0.0 {
        return value.to_string();
    }

    let abs_value = value.abs();

    // Very small values (< 0.0001) or extremely large values (>= 1e15) -> Scientific notation
    if !(0.0001..1e15).contains(&abs_value) {
        return format_scientific(value);
    }

    // Standard numbers - preserve 15 digits of precision with thousand separators
    format_standard(value)
}

/// Format a number in scientific notation with up to 15 significant digits
fn format_scientific(value: f64) -> String {
    // Format with up to 15 significant digits in scientific notation
    let formatted = format!("{:.14e}", value);

    // Parse and clean up the result
    if let Some((mantissa_str, exp_str)) = formatted.split_once('e') {
        let mantissa: f64 = mantissa_str.parse().unwrap_or(value);
        let exp: i32 = exp_str.parse().unwrap_or(0);

        // Format mantissa, removing trailing zeros
        let mantissa_formatted = format_mantissa(mantissa);

        format!("{}e{}", mantissa_formatted, exp)
    } else {
        formatted
    }
}

/// Format the mantissa part, removing unnecessary trailing zeros
fn format_mantissa(mantissa: f64) -> String {
    // Format with enough precision
    let s = format!("{:.14}", mantissa);

    // Remove trailing zeros after decimal point
    let s = s.trim_end_matches('0');
    // Remove trailing decimal point if no decimals left
    let s = s.trim_end_matches('.');

    s.to_string()
}

/// Format a standard number with thousand separators and up to 15 significant digits
fn format_standard(value: f64) -> String {
    // For integers that fit in i64 range, handle them directly without float rounding
    // This avoids floating point precision issues with large integers
    if value.fract() == 0.0 && value.abs() < 9007199254740992.0 {
        // Can be exactly represented as i64
        let int_val = value as i64;
        return format_integer_with_separators(int_val);
    }

    // Round to 15 significant figures for non-integers
    let rounded = round_to_significant_figures(value, 15);

    // Format the number, preserving significant digits
    let formatted = format_float_significant(rounded, 15);

    // Add thousand separators to the integer part
    add_thousand_separators(&formatted)
}

/// Round a number to n significant figures
fn round_to_significant_figures(value: f64, sig_figs: u32) -> f64 {
    if value == 0.0 {
        return 0.0;
    }

    let magnitude = value.abs().log10().floor() as i32;
    let scale = 10_f64.powi(sig_figs as i32 - 1 - magnitude);
    (value * scale).round() / scale
}

/// Format a float with up to n significant figures, removing trailing zeros
fn format_float_significant(value: f64, max_sig_figs: usize) -> String {
    // Determine how many decimal places we need
    let abs_value = value.abs();
    let magnitude = if abs_value >= 1.0 {
        abs_value.log10().floor() as i32 + 1
    } else {
        // For numbers < 1, count leading zeros
        -(abs_value.log10().floor() as i32)
    };

    // Calculate decimal places needed for significant figures
    let decimal_places = if abs_value >= 1.0 {
        (max_sig_figs as i32 - magnitude).max(0) as usize
    } else {
        // For numbers < 1, we need more decimal places
        (max_sig_figs as i32 + magnitude - 1).max(0) as usize
    };

    let formatted = format!("{:.prec$}", value, prec = decimal_places);

    // Remove trailing zeros after decimal point, but keep at least one decimal if there is a decimal point
    if formatted.contains('.') {
        let trimmed = formatted.trim_end_matches('0');
        if trimmed.ends_with('.') {
            trimmed.trim_end_matches('.').to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        formatted
    }
}

/// Format an integer with thousand separators (commas)
fn format_integer_with_separators(value: i64) -> String {
    let is_negative = value < 0;
    let abs_str = value.abs().to_string();

    let with_commas: String = abs_str
        .chars()
        .rev()
        .enumerate()
        .flat_map(|(i, c)| {
            if i > 0 && i % 3 == 0 {
                vec![',', c]
            } else {
                vec![c]
            }
        })
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect();

    if is_negative {
        format!("-{}", with_commas)
    } else {
        with_commas
    }
}

/// Add thousand separators to the integer part of a formatted float string
fn add_thousand_separators(s: &str) -> String {
    let is_negative = s.starts_with('-');
    let s = if is_negative { &s[1..] } else { s };

    let (int_part, dec_part) = if let Some(dot_pos) = s.find('.') {
        (&s[..dot_pos], Some(&s[dot_pos..]))
    } else {
        (s, None)
    };

    // Add commas to integer part
    let int_with_commas: String = int_part
        .chars()
        .rev()
        .enumerate()
        .flat_map(|(i, c)| {
            if i > 0 && i % 3 == 0 {
                vec![',', c]
            } else {
                vec![c]
            }
        })
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect();

    let result = if let Some(dec) = dec_part {
        format!("{}{}", int_with_commas, dec)
    } else {
        int_with_commas
    };

    if is_negative {
        format!("-{}", result)
    } else {
        result
    }
}

#[derive(Debug)]
pub enum FunctionArity {
    Exact(usize),
    AtLeast(usize),
    Between(usize, usize),
}

impl FunctionArity {
    pub fn can_accept(&self, n: usize) -> bool {
        match self {
            FunctionArity::Exact(expected) => n == *expected,
            FunctionArity::Between(min, max) => n >= *min && n <= *max,
            FunctionArity::AtLeast(min) => n >= *min,
        }
    }
}

/// A lambda parameter. The name is an interned [`Symbol`] so that binding arguments at call
/// time copies a pointer instead of cloning a `String`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum LambdaArg {
    Required(Symbol),
    Optional(Symbol),
    Rest(Symbol),
}

impl LambdaArg {
    pub fn get_name(&self) -> Symbol {
        match self {
            LambdaArg::Required(name) => *name,
            LambdaArg::Optional(name) => *name,
            LambdaArg::Rest(name) => *name,
        }
    }

    pub fn is_required(&self) -> bool {
        matches!(self, LambdaArg::Required(_))
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, LambdaArg::Optional(_))
    }

    pub fn is_rest(&self) -> bool {
        matches!(self, LambdaArg::Rest(_))
    }

    pub fn as_required(&self) -> Result<Symbol> {
        match self {
            LambdaArg::Required(name) => Ok(*name),
            _ => Err(anyhow!(
                "expected a required argument, but got a {} one",
                self.get_name()
            )),
        }
    }

    pub fn as_optional(&self) -> Result<Symbol> {
        match self {
            LambdaArg::Optional(name) => Ok(*name),
            _ => Err(anyhow!(
                "expected an optional argument, but got a {} one",
                self.get_name()
            )),
        }
    }

    pub fn as_rest(&self) -> Result<Symbol> {
        match self {
            LambdaArg::Rest(name) => Ok(*name),
            _ => Err(anyhow!(
                "expected a rest argument, but got a {} one",
                self.get_name()
            )),
        }
    }
}

impl Display for LambdaArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaArg::Required(name) => write!(f, "{}", name),
            LambdaArg::Optional(name) => write!(f, "{}?", name),
            LambdaArg::Rest(name) => write!(f, "...{}", name),
        }
    }
}

/// The variables captured by a closure, stored in the slot order of the lambda's parse-time
/// captures list and shared via `Rc` so that calling the closure never copies the captured
/// bindings.
///
/// A `None` slot means the name was not bound when the closure was created; references to it
/// fall back to a late-bound lookup through the call-site environment chain.
#[derive(Clone, Default)]
pub struct CapturedScope {
    names: Rc<Vec<Symbol>>,
    values: Rc<Vec<Option<Value>>>,

    /// The compiled body of the function this scope belongs to. It lives here rather than on
    /// [`LambdaDef`] so that definitions can still be built field by field; it is a cache that
    /// is filled in on the first call when the closure was not created by the VM itself, and
    /// it never takes part in comparisons.
    pub(crate) code: OnceCell<Rc<Proto>>,

    /// What a call of the function through its own value runs (see [`Entry`]).
    pub(crate) entry: Entry,
}

/// The code that a call of a function through its own value runs, once a VM has found that
/// out: the compiled body of the function, or the variant of it in which the function calls
/// itself directly (see [`crate::bytecode::SelfRef`]).
///
/// Which of the two it is depends on the name of the function, on what it captured and on
/// what it was compiled from, and whoever holds the definition can change those. So it only
/// counts during the run of the VM that found it out (see `Vm::run_number`), when nothing
/// but that VM gets at the definition, and the VM takes it back when it renames the
/// function. A copy of the definition starts without one.
pub(crate) struct Entry {
    /// Owned by the `code` next to the entry; not null when `run` is the number of a run.
    code: Cell<*const Proto>,

    /// The number of the run that found the code out, [`Entry::NO_RUN`] when none did.
    run: Cell<u64>,
}

impl Entry {
    /// Not the number of any run.
    pub(crate) const NO_RUN: u64 = 0;

    /// The entry of a closure that a VM just made from `code` in run `run`: the code itself
    /// when it has no variant that could take its place.
    fn of(code: &Rc<Proto>, run: u64) -> Self {
        match code.self_refs.is_empty() {
            true => Self {
                code: Cell::new(Rc::as_ptr(code)),
                run: Cell::new(run),
            },
            false => Self::default(),
        }
    }

    /// The number of the run in which [`Entry::code`] is the code to run.
    #[inline(always)]
    pub(crate) fn run(&self) -> u64 {
        self.run.get()
    }

    #[inline(always)]
    pub(crate) fn code(&self) -> *const Proto {
        self.code.get()
    }

    /// Record that run `run` found `code`, which the `code` next to the entry owns, to be
    /// what a call of the function through its own value runs.
    pub(crate) fn set(&self, code: *const Proto, run: u64) {
        self.code.set(code);
        self.run.set(run);
    }

    fn clear(&self) {
        self.run.set(Self::NO_RUN);
    }
}

impl Default for Entry {
    fn default() -> Self {
        Self {
            code: Cell::new(std::ptr::null()),
            run: Cell::new(Self::NO_RUN),
        }
    }
}

impl Clone for Entry {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl PartialEq for CapturedScope {
    fn eq(&self, other: &Self) -> bool {
        self.names == other.names && self.values == other.values
    }
}

impl std::fmt::Debug for CapturedScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CapturedScope")
            .field("names", &self.names)
            .field("values", &self.values)
            .finish()
    }
}

impl CapturedScope {
    /// Build a scope from capture names (in slot order) and the values, if any, that each name
    /// was bound to when the closure was created.
    pub fn new(names: Vec<Symbol>, values: Vec<Option<Value>>) -> Self {
        debug_assert_eq!(names.len(), values.len());
        Self {
            names: Rc::new(names),
            values: Rc::new(values),
            code: OnceCell::new(),
            entry: Entry::default(),
        }
    }

    /// Build the scope of a closure created by a VM in run `run`: the capture names are
    /// shared with the lambda expression the closure came from, and the compiled body is
    /// attached up front.
    pub(crate) fn compiled(
        names: Rc<Vec<Symbol>>,
        values: Vec<Option<Value>>,
        code: Rc<Proto>,
        run: u64,
    ) -> Self {
        debug_assert_eq!(names.len(), values.len());
        Self {
            names,
            values: Rc::new(values),
            entry: Entry::of(&code, run),
            code: OnceCell::from(code),
        }
    }

    /// The scope of a closure created by a VM in run `run` when none of its captures were
    /// bound: like [`CapturedScope::default`], but with the compiled body attached.
    pub(crate) fn unbound(code: Rc<Proto>, run: u64) -> Self {
        Self {
            entry: Entry::of(&code, run),
            code: OnceCell::from(code),
            ..Self::default()
        }
    }

    /// The function this scope belongs to got another name, which the code that a call of
    /// it runs depends on when the function may be calling itself by name.
    pub(crate) fn renamed(&self) {
        if self
            .code
            .get()
            .is_none_or(|code| !code.self_refs.is_empty())
        {
            self.entry.clear();
        }
    }

    /// Build a scope where every entry is bound, e.g. when reconstructing a deserialized
    /// function's captured bindings.
    pub fn from_bound_pairs(pairs: impl IntoIterator<Item = (Symbol, Value)>) -> Self {
        let (names, values): (Vec<_>, Vec<_>) = pairs
            .into_iter()
            .map(|(name, value)| (name, Some(value)))
            .unzip();

        Self::new(names, values)
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Whether `key` was captured and bound at closure creation time.
    pub fn contains_key(&self, key: Symbol) -> bool {
        self.get(key).is_some()
    }

    /// Look up a bound capture by name (used by dynamic, non-slot lookups).
    pub fn get(&self, key: Symbol) -> Option<Value> {
        self.names
            .iter()
            .position(|name| *name == key)
            .and_then(|index| self.values[index])
    }

    /// Look up a capture by its slot index; `None` if the slot was unbound at creation.
    #[inline]
    pub fn get_slot(&self, index: usize) -> Option<Value> {
        self.values.get(index).copied().flatten()
    }

    /// Like [`CapturedScope::get_slot`], but hands out the stored value in place.
    #[inline(always)]
    pub(crate) fn slot_ref(&self, index: usize) -> Option<&Value> {
        self.values.get(index).and_then(Option::as_ref)
    }

    /// Iterate over the bound captures as (name, value) pairs.
    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &Value)> {
        self.names
            .iter()
            .zip(self.values.iter())
            .filter_map(|(name, value)| value.as_ref().map(|value| (name, value)))
    }

    /// Rewrite every bound capture value in place (used by heap compaction to remap pointers).
    pub fn rewrite_values(&mut self, rewrite: &mut impl FnMut(Value) -> Value) {
        if self.values.is_empty() {
            return;
        }

        for value in Rc::make_mut(&mut self.values).iter_mut().flatten() {
            *value = rewrite(*value);
        }
    }
}

/// A function value. Cloning a `LambdaDef` is cheap (reference-count bumps only), which matters
/// because every call expression clones the definition out of the heap before invoking it.
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDef {
    pub name: Option<Symbol>,
    pub args: Rc<Vec<LambdaArg>>,
    pub body: Rc<SpannedExpr>,
    pub scope: CapturedScope,

    /// The source code string that the body's spans refer to (shared via Rc for efficiency)
    pub source: Rc<str>,
}

impl LambdaDef {
    pub fn set_name(&mut self, name: Symbol, _value: Value) {
        self.name = Some(name);
        self.scope.renamed();
    }

    /// Find out for run `run` what a call of the function through its own value runs, when
    /// that takes no more than a look at the code the function already has: code that was
    /// compiled from this body and these parameters and that has no variant. Returns whether
    /// the [`Entry`] now says so.
    #[cold]
    #[inline(never)]
    pub(crate) fn enters_own_code(&self, run: u64) -> bool {
        match self.scope.code.get() {
            Some(code) if code.self_refs.is_empty() && code.belongs_to(&self.body, &self.args) => {
                self.scope.entry.set(Rc::as_ptr(code), run);
                true
            }
            _ => false,
        }
    }

    pub fn get_arity(&self) -> FunctionArity {
        let has_rest = self.args.iter().any(|arg| arg.is_rest());
        let min = self.args.iter().filter(|arg| arg.is_required()).count();
        let max = self.args.len();

        if has_rest {
            FunctionArity::AtLeast(min)
        } else if min == max {
            FunctionArity::Exact(min)
        } else {
            FunctionArity::Between(min, max)
        }
    }
}

impl PartialOrd for LambdaDef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.args > other.args {
            Some(std::cmp::Ordering::Greater)
        } else if self.args < other.args {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SerializableLambdaDef {
    pub name: Option<String>,
    pub args: Vec<LambdaArg>,
    pub body: String,
    pub scope: Option<IndexMap<String, SerializableValue>>,
}

pub struct WithHeap<'h, T> {
    pub value: &'h T,
    pub heap: Rc<RefCell<Heap>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReifiedIterableValue<'h> {
    List(&'h [Value]),
    String(&'h str),
    Record(&'h Record),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReifiedIterableValueType {
    List,
    String,
    Record,
}

impl<'h> ReifiedIterableValue<'h> {
    pub fn with_heap(&'h self, heap: Rc<RefCell<Heap>>) -> WithHeap<'h, ReifiedIterableValue<'h>> {
        WithHeap { value: self, heap }
    }

    pub fn get_type(&self) -> ReifiedIterableValueType {
        match self {
            ReifiedIterableValue::List(_) => ReifiedIterableValueType::List,
            ReifiedIterableValue::String(_) => ReifiedIterableValueType::String,
            ReifiedIterableValue::Record(_) => ReifiedIterableValueType::Record,
        }
    }
}

impl<'h> IntoIterator for WithHeap<'h, ReifiedIterableValue<'h>> {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self.value {
            // Yields an iterator over the values in the list.
            ReifiedIterableValue::List(l) => Vec::from(*l).into_iter(),
            // Yields an iterator over the characters in the string.
            ReifiedIterableValue::String(s) => s
                .chars()
                .map(|c| self.heap.borrow_mut().insert_string(c))
                .collect::<Vec<Value>>()
                .into_iter(),
            // Yields an iterator over the [key, value] pairs of the record.
            ReifiedIterableValue::Record(r) => r
                .into_iter()
                .map(|(k, v)| {
                    let list = vec![self.heap.borrow_mut().insert_string(&**k), *v];
                    self.heap.borrow_mut().insert_list(list)
                })
                .collect::<Vec<Value>>()
                .into_iter(),
        }
    }
}

impl<'h> IntoIterator for WithHeap<'h, ReifiedValue<'h>> {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self.value {
            ReifiedValue::Spread(iterable, _) => iterable.with_heap(self.heap).into_iter(),
            _ => vec![(*self.value).into()].into_iter(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ValueType {
    Number,
    List,
    Spread,
    Bool,
    Lambda,
    BuiltIn,
    String,
    Record,
    Null,
}

impl ValueType {
    /// What the type is called.
    pub fn name(&self) -> &'static str {
        match self {
            ValueType::Number => "number",
            ValueType::List => "list",
            ValueType::Spread => "spread",
            ValueType::Bool => "boolean",
            ValueType::Lambda => "function",
            ValueType::BuiltIn => "built-in function",
            ValueType::String => "string",
            ValueType::Record => "record",
            ValueType::Null => "null",
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

/// A value, after it has been "reified" (borrowed) from a pointer.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReifiedValue<'h> {
    /// A number is a floating-point value.
    Number(f64),
    /// A boolean value is either true or false.
    Bool(bool),
    /// A null value represents the absence of a value.
    Null,
    /// A list is a sequence of values.
    List(&'h [Value], ListPointer),
    /// A string is a sequence of characters.
    String(&'h str, StringPointer),
    /// A record is a collection of key-value pairs.
    Record(&'h Record, RecordPointer),
    /// A lambda is a function definition.
    Lambda(&'h LambdaDef, LambdaPointer),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(ReifiedIterableValue<'h>, IterablePointer),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(BuiltInFunction),
}

impl<'h> ReifiedValue<'h> {
    pub fn with_heap(&'h self, heap: Rc<RefCell<Heap>>) -> WithHeap<'h, ReifiedValue<'h>> {
        WithHeap { value: self, heap }
    }
}

impl From<ReifiedValue<'_>> for Value {
    fn from(value: ReifiedValue) -> Self {
        match value {
            ReifiedValue::Number(n) => Value::Number(n),
            ReifiedValue::Bool(b) => Value::Bool(b),
            ReifiedValue::Null => Value::Null,
            ReifiedValue::List(_, p) => Value::List(p),
            ReifiedValue::String(_, p) => Value::String(p),
            ReifiedValue::Record(_, p) => Value::Record(p),
            ReifiedValue::Lambda(_, p) => Value::Lambda(p),
            ReifiedValue::Spread(_, p) => Value::Spread(p),
            ReifiedValue::BuiltIn(id) => Value::BuiltIn(id),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SerializableIterableValue {
    List(Vec<SerializableValue>),
    String(String),
    Record(IndexMap<String, SerializableValue>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum SerializableValue {
    Number(f64),
    Bool(bool),
    Null,
    List(Vec<SerializableValue>),
    String(String),
    Record(IndexMap<String, SerializableValue>),
    Lambda(SerializableLambdaDef),
    BuiltIn(String),
}

impl SerializableValue {
    /// Convert from serde_json::Value to SerializableValue
    pub fn from_json(value: &serde_json::Value) -> SerializableValue {
        match value {
            serde_json::Value::Number(n) => SerializableValue::Number(n.as_f64().unwrap_or(0.0)),
            serde_json::Value::Bool(b) => SerializableValue::Bool(*b),
            serde_json::Value::Null => SerializableValue::Null,
            serde_json::Value::String(s) => SerializableValue::String(s.clone()),
            serde_json::Value::Array(arr) => {
                SerializableValue::List(arr.iter().map(Self::from_json).collect())
            }
            serde_json::Value::Object(obj) => {
                // Check if this is a function object
                if let Some(func_value) = obj.get("__blots_function")
                    && let Some(func_str) = func_value.as_str()
                {
                    // Try to parse as a built-in function first (just a name)
                    if crate::functions::BuiltInFunction::from_ident(func_str).is_some() {
                        return SerializableValue::BuiltIn(func_str.to_string());
                    }

                    // Otherwise, parse as a lambda function
                    // We need to parse the function source and convert it to a SerializableLambdaDef
                    if let Ok(lambda_def) = Self::parse_function_source(func_str) {
                        return SerializableValue::Lambda(lambda_def);
                    }
                }

                // Regular record
                let map: IndexMap<String, SerializableValue> = obj
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::from_json(v)))
                    .collect();
                SerializableValue::Record(map)
            }
        }
    }

    /// Parse a function source string into a SerializableLambdaDef
    fn parse_function_source(source: &str) -> Result<SerializableLambdaDef> {
        use crate::expressions::pairs_to_expr;
        use crate::parser::get_pairs;

        // Parse the source as an expression
        let pairs = get_pairs(source)?;

        // Extract the lambda expression from the parsed pairs
        for pair in pairs {
            if let crate::parser::Rule::statement = pair.as_rule()
                && let Some(inner_pair) = pair.into_inner().next()
                && let crate::parser::Rule::expression = inner_pair.as_rule()
            {
                // Parse the expression to get an AST
                let expr = pairs_to_expr(inner_pair.into_inner())?;

                // Check if it's a lambda
                if let crate::ast::Expr::Lambda { args, body, .. } = expr.node {
                    // Since the function is already inlined (no scope needed),
                    // we create a SerializableLambdaDef with the source as the body
                    return Ok(SerializableLambdaDef {
                        name: None,
                        args,
                        body: crate::ast_to_source::expr_to_source(&body),
                        scope: None, // Functions from JSON have no scope - they're already inlined
                    });
                }
            }
        }

        Err(anyhow!("Failed to parse function source: {}", source))
    }

    /// Convert SerializableValue to clean serde_json::Value
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            SerializableValue::Number(n) => serde_json::Value::Number(
                serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
            ),
            SerializableValue::Bool(b) => serde_json::Value::Bool(*b),
            SerializableValue::Null => serde_json::Value::Null,
            SerializableValue::String(s) => serde_json::Value::String(s.clone()),
            SerializableValue::List(items) => {
                serde_json::Value::Array(items.iter().map(|v| v.to_json()).collect())
            }
            SerializableValue::Record(fields) => {
                let map: serde_json::Map<String, serde_json::Value> = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_json()))
                    .collect();
                serde_json::Value::Object(map)
            }
            SerializableValue::Lambda(lambda_def) => {
                // We can't inline scope values from SerializableLambdaDef because the body is already a string
                // For now, output the function as a JSON object with the source code
                let mut map = serde_json::Map::new();

                // Build the function source with arguments
                let args_str: Vec<String> = lambda_def
                    .args
                    .iter()
                    .map(|arg| match arg {
                        LambdaArg::Required(name) => name.to_string(),
                        LambdaArg::Optional(name) => format!("{}?", name),
                        LambdaArg::Rest(name) => format!("...{}", name),
                    })
                    .collect();

                let function_source = format!("({}) => {}", args_str.join(", "), lambda_def.body);

                map.insert(
                    "__blots_function".to_string(),
                    serde_json::Value::String(function_source),
                );
                serde_json::Value::Object(map)
            }
            SerializableValue::BuiltIn(name) => {
                // Output built-in functions in the same format as lambdas
                let mut map = serde_json::Map::new();
                map.insert(
                    "__blots_function".to_string(),
                    serde_json::Value::String(name.clone()),
                );
                serde_json::Value::Object(map)
            }
        }
    }

    pub fn from_value(value: &Value, heap: &Heap) -> Result<SerializableValue> {
        match value {
            Value::Number(n) => Ok(SerializableValue::Number(*n)),
            Value::Bool(b) => Ok(SerializableValue::Bool(*b)),
            Value::Null => Ok(SerializableValue::Null),
            Value::List(p) => {
                let list = p.reify(heap).as_list()?;
                let serialized_list = list
                    .iter()
                    .map(|v| SerializableValue::from_value(v, heap))
                    .collect::<Result<Vec<SerializableValue>>>()?;
                Ok(SerializableValue::List(serialized_list))
            }
            Value::String(p) => {
                let string = p.reify(heap).as_string()?;
                Ok(SerializableValue::String(string.to_string()))
            }
            Value::Record(p) => {
                let record = p.reify(heap).as_record()?;
                let serialized_record = record
                    .iter()
                    .map(|(k, v)| Ok((k.to_string(), SerializableValue::from_value(v, heap)?)))
                    .collect::<Result<IndexMap<String, SerializableValue>>>()?;
                Ok(SerializableValue::Record(serialized_record))
            }
            Value::Lambda(p) => {
                let lambda = p.reify(heap).as_lambda()?;

                // Convert the scope to SerializableValues
                let serializable_scope: IndexMap<String, SerializableValue> = lambda
                    .scope
                    .iter()
                    .map(|(k, v)| {
                        SerializableValue::from_value(v, heap).map(|sv| (k.to_string(), sv))
                    })
                    .collect::<Result<IndexMap<String, SerializableValue>>>()?;

                // Generate the body with inlined scope values
                let body_with_inlined_scope = crate::ast_to_source::expr_to_source_with_scope(
                    &lambda.body,
                    &serializable_scope,
                );

                Ok(SerializableValue::Lambda(SerializableLambdaDef {
                    name: lambda.name.as_ref().map(|n| n.to_string()),
                    args: lambda.args.as_ref().clone(),
                    body: body_with_inlined_scope,
                    scope: Some(serializable_scope),
                }))
            }
            Value::BuiltIn(built_in) => Ok(SerializableValue::BuiltIn(built_in.name().to_string())),
            Value::Spread(_) => Err(anyhow!("cannot serialize a spread value")),
        }
    }

    pub fn to_value(&self, heap: &mut Heap) -> Result<Value> {
        match self {
            SerializableValue::Number(n) => Ok(Value::Number(*n)),
            SerializableValue::Bool(b) => Ok(Value::Bool(*b)),
            SerializableValue::Null => Ok(Value::Null),
            SerializableValue::List(list) => {
                let deserialized_list = list
                    .iter()
                    .map(|v| SerializableValue::to_value(v, heap))
                    .collect::<Result<Vec<Value>>>()?;

                Ok(heap.insert_list(deserialized_list))
            }
            SerializableValue::String(s) => Ok(heap.insert_string(s.as_str())),
            SerializableValue::Record(record) => {
                let deserialized_record = record
                    .iter()
                    .map(|(k, v)| Ok((k.to_string(), SerializableValue::to_value(v, heap)?)))
                    .collect::<Result<IndexMap<String, Value>>>()?;
                Ok(heap.insert_record(deserialized_record))
            }
            SerializableValue::Lambda(s_lambda) => {
                let scope = if let Some(scope) = s_lambda.scope.as_ref() {
                    let pairs = scope
                        .iter()
                        .map(|(k, v)| {
                            Ok((Symbol::intern(k), SerializableValue::to_value(v, heap)?))
                        })
                        .collect::<Result<Vec<(Symbol, Value)>>>()?;

                    CapturedScope::from_bound_pairs(pairs)
                } else {
                    CapturedScope::default()
                };

                // For now, parse the body string back to AST
                // In a real implementation, we'd want to serialize/deserialize the AST properly
                let body_ast = crate::expressions::pairs_to_expr(
                    crate::parser::get_pairs(&s_lambda.body)?
                        .next()
                        .unwrap()
                        .into_inner(),
                )?;

                let lambda = LambdaDef {
                    name: s_lambda.name.as_deref().map(Symbol::intern),
                    args: Rc::new(s_lambda.args.clone()),
                    body: Rc::new(body_ast),
                    scope,
                    source: Rc::from(""), // Deserialized lambdas don't have original source
                };

                Ok(heap.insert_lambda(lambda))
            }
            SerializableValue::BuiltIn(ident) => BuiltInFunction::from_ident(ident)
                .ok_or(anyhow!("built-in function with ident {} not found", ident))
                .map(Value::BuiltIn),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum PrimitiveValue {
    Number(f64),
    Bool(bool),
    Null,
}

impl From<PrimitiveValue> for Value {
    fn from(val: PrimitiveValue) -> Self {
        match val {
            PrimitiveValue::Number(n) => Value::Number(n),
            PrimitiveValue::Bool(b) => Value::Bool(b),
            PrimitiveValue::Null => Value::Null,
        }
    }
}

impl From<PrimitiveValue> for SerializableValue {
    fn from(val: PrimitiveValue) -> Self {
        match val {
            PrimitiveValue::Number(n) => SerializableValue::Number(n),
            PrimitiveValue::Bool(b) => SerializableValue::Bool(b),
            PrimitiveValue::Null => SerializableValue::Null,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Value {
    /// A number is a floating-point value.
    Number(f64),
    /// A boolean value is either true or false.
    Bool(bool),
    /// A null value represents the absence of a value.
    Null,
    /// A list is a sequence of values.
    List(ListPointer),
    /// A string is a sequence of characters.
    String(StringPointer),
    /// A record is a collection of key-value pairs.
    Record(RecordPointer),
    /// A lambda is a function definition.
    Lambda(LambdaPointer),
    /// A spread value is "spread" into its container when it is used in a list, record, or function call. (internal only)
    Spread(IterablePointer),
    /// A built-in function is a function that is implemented in Rust.
    BuiltIn(BuiltInFunction),
}

impl Value {
    /// Compare two values for equality by their actual content, not by reference
    pub fn equals(&self, other: &Value, heap: &Heap) -> Result<bool> {
        match (self, other) {
            // Primitive types - compare directly
            (Value::Number(a), Value::Number(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Null, Value::Null) => Ok(true),

            // String comparison - compare content
            (Value::String(a_ptr), Value::String(b_ptr)) => {
                let a_str = a_ptr.reify(heap).as_string()?;
                let b_str = b_ptr.reify(heap).as_string()?;
                Ok(a_str == b_str)
            }

            // List comparison - compare elements recursively
            (Value::List(a_ptr), Value::List(b_ptr)) => {
                let a_list = a_ptr.reify(heap).as_list()?;
                let b_list = b_ptr.reify(heap).as_list()?;

                if a_list.len() != b_list.len() {
                    return Ok(false);
                }

                for (a_elem, b_elem) in a_list.iter().zip(b_list.iter()) {
                    if !a_elem.equals(b_elem, heap)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }

            // Record comparison - compare keys and values
            (Value::Record(a_ptr), Value::Record(b_ptr)) => {
                let a_record = a_ptr.reify(heap).as_record()?;
                let b_record = b_ptr.reify(heap).as_record()?;

                if a_record.len() != b_record.len() {
                    return Ok(false);
                }

                for (key, a_value) in a_record.iter() {
                    match b_record.get(key) {
                        Some(b_value) => {
                            if !a_value.equals(b_value, heap)? {
                                return Ok(false);
                            }
                        }
                        None => return Ok(false),
                    }
                }
                Ok(true)
            }

            // Lambda comparison - compare by structure (AST equality)
            (Value::Lambda(a_ptr), Value::Lambda(b_ptr)) => {
                let a_lambda = a_ptr.reify(heap).as_lambda()?;
                let b_lambda = b_ptr.reify(heap).as_lambda()?;

                // Compare argument lists
                if a_lambda.args != b_lambda.args {
                    return Ok(false);
                }

                // Compare AST bodies
                if a_lambda.body != b_lambda.body {
                    return Ok(false);
                }

                // Note: We don't compare scopes because two functions with the same
                // definition but different closures would have different behavior
                // For now, we only compare structure, not captured variables
                Ok(true)
            }

            // Built-in functions - compare by ID
            (Value::BuiltIn(a), Value::BuiltIn(b)) => Ok(a == b),

            // Spread values - compare the underlying iterable
            (Value::Spread(a_ptr), Value::Spread(b_ptr)) => match (a_ptr, b_ptr) {
                (IterablePointer::List(a), IterablePointer::List(b)) => {
                    Value::List(*a).equals(&Value::List(*b), heap)
                }
                (IterablePointer::String(a), IterablePointer::String(b)) => {
                    Value::String(*a).equals(&Value::String(*b), heap)
                }
                (IterablePointer::Record(a), IterablePointer::Record(b)) => {
                    Value::Record(*a).equals(&Value::Record(*b), heap)
                }
                _ => Ok(false),
            },

            // Different types are never equal
            _ => Ok(false),
        }
    }

    /// Compare two values for ordering
    pub fn compare(&self, other: &Value, heap: &Heap) -> Result<Option<std::cmp::Ordering>> {
        match (self, other) {
            // Numbers have natural ordering
            (Value::Number(a), Value::Number(b)) => Ok(a.partial_cmp(b)),

            // Booleans: false < true
            (Value::Bool(a), Value::Bool(b)) => Ok(a.partial_cmp(b)),

            // Strings compare lexicographically
            (Value::String(a_ptr), Value::String(b_ptr)) => {
                let a_str = a_ptr.reify(heap).as_string()?;
                let b_str = b_ptr.reify(heap).as_string()?;
                Ok(a_str.partial_cmp(b_str))
            }

            // Lists compare lexicographically
            (Value::List(a_ptr), Value::List(b_ptr)) => {
                let a_list = a_ptr.reify(heap).as_list()?;
                let b_list = b_ptr.reify(heap).as_list()?;

                for (a_elem, b_elem) in a_list.iter().zip(b_list.iter()) {
                    match a_elem.compare(b_elem, heap)? {
                        Some(std::cmp::Ordering::Equal) => continue,
                        other => return Ok(other),
                    }
                }

                // If all compared elements are equal, compare lengths
                Ok(a_list.len().partial_cmp(&b_list.len()))
            }

            // Other types don't have a natural ordering
            _ => Ok(None),
        }
    }

    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::List(_) => ValueType::List,
            Value::Spread(_) => ValueType::Spread,
            Value::Bool(_) => ValueType::Bool,
            Value::Lambda(_) => ValueType::Lambda,
            Value::String(_) => ValueType::String,
            Value::Null => ValueType::Null,
            Value::BuiltIn(_) => ValueType::BuiltIn,
            Value::Record(_) => ValueType::Record,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }

    pub fn is_spread(&self) -> bool {
        matches!(self, Value::Spread(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self, Value::Lambda(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_built_in(&self) -> bool {
        matches!(self, Value::BuiltIn(_))
    }

    pub fn is_callable(&self) -> bool {
        matches!(self, Value::BuiltIn(_) | Value::Lambda(_))
    }

    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(anyhow!("expected a number, but got a {}", self.get_type())),
        }
    }

    pub fn as_list<'h>(&self, heap: &'h Heap) -> Result<&'h List> {
        match self {
            Value::List(l) => l.reify(heap).as_list(),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_spread<'h>(&self, heap: &'h Heap) -> Result<&'h HeapValue> {
        match self {
            Value::Spread(v) => Ok(v.reify(heap)),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(anyhow!("expected a boolean, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda<'h>(&self, heap: &'h Heap) -> Result<&'h LambdaDef> {
        match self {
            Value::Lambda(l) => l.reify(heap).as_lambda(),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_string<'h>(&self, heap: &'h Heap) -> Result<&'h str> {
        self.as_text(heap).map(Text::as_str)
    }

    /// The text of the string this is, which can be copied without copying its characters.
    pub fn as_text<'h>(&self, heap: &'h Heap) -> Result<&'h Text> {
        match self {
            Value::String(p) => p.reify(heap).as_text(),
            _ => Err(anyhow!("expected a string, but got a {}", self.get_type())),
        }
    }

    pub fn as_null(&self) -> Result<()> {
        match self {
            Value::Null => Ok(()),
            _ => Err(anyhow!("expected a null, but got a {}", self.get_type())),
        }
    }

    pub fn as_built_in(&self) -> Result<&str> {
        match self {
            Value::BuiltIn(built_in) => Ok(built_in.name()),
            _ => Err(anyhow!(
                "expected a built-in function, but got a {}",
                self.get_type()
            )),
        }
    }

    pub fn as_record<'h>(&self, heap: &'h Heap) -> Result<&'h Record> {
        match self {
            Value::Record(r) => r.reify(heap).as_record(),
            _ => Err(anyhow!("expected a record, but got a {}", self.get_type())),
        }
    }

    pub fn as_list_pointer(&self) -> Result<ListPointer> {
        match self {
            Value::List(p) => Ok(*p),
            _ => Err(anyhow!("expected a list, but got a {}", self.get_type())),
        }
    }

    pub fn as_string_pointer(&self) -> Result<StringPointer> {
        match self {
            Value::String(p) => Ok(*p),
            _ => Err(anyhow!("expected a string, but got a {}", self.get_type())),
        }
    }

    pub fn as_record_pointer(&self) -> Result<RecordPointer> {
        match self {
            Value::Record(p) => Ok(*p),
            _ => Err(anyhow!("expected a record, but got a {}", self.get_type())),
        }
    }

    pub fn as_lambda_pointer(&self) -> Result<LambdaPointer> {
        match self {
            Value::Lambda(p) => Ok(*p),
            _ => Err(anyhow!("expected a lambda, but got a {}", self.get_type())),
        }
    }

    pub fn as_iterable_pointer(&self) -> Result<IterablePointer> {
        match self {
            Value::Spread(p) => Ok(*p),
            _ => Err(anyhow!("expected a spread, but got a {}", self.get_type())),
        }
    }

    pub fn to_serializable_value(&self, heap: &Heap) -> Result<SerializableValue> {
        SerializableValue::from_value(self, heap)
    }

    pub fn reify<'h>(&self, heap: &'h Heap) -> Result<ReifiedValue<'h>> {
        match self {
            Value::Number(n) => Ok(ReifiedValue::Number(*n)),
            Value::List(p) => Ok(ReifiedValue::List(p.reify(heap).as_list()?, *p)),
            Value::Spread(p) => Ok(ReifiedValue::Spread(p.reify(heap).as_iterable()?, *p)),
            Value::Bool(b) => Ok(ReifiedValue::Bool(*b)),
            Value::Lambda(p) => Ok(ReifiedValue::Lambda(p.reify(heap).as_lambda()?, *p)),
            Value::String(p) => Ok(ReifiedValue::String(p.reify(heap).as_string()?, *p)),
            Value::Null => Ok(ReifiedValue::Null),
            Value::BuiltIn(id) => Ok(ReifiedValue::BuiltIn(*id)),
            Value::Record(p) => Ok(ReifiedValue::Record(p.reify(heap).as_record()?, *p)),
        }
    }

    /// Stringify the value. Returns the same thing as stringify, except for
    /// Value::String, which is returned without wrapping quotes. Use this for string
    /// concatenation, formatting, etc. Don't use this for displaying values to the user.
    pub fn stringify_internal(&self, heap: &Heap) -> String {
        self.stringify(heap, false, false)
    }

    pub fn stringify_external(&self, heap: &Heap) -> String {
        self.stringify(heap, true, false)
    }

    /// Stringify the value with display formatting (thousand separators, scientific notation).
    /// Use this for user-facing output like the format() function.
    pub fn stringify_for_display(&self, heap: &Heap) -> String {
        self.stringify(heap, false, true)
    }

    fn stringify(&self, heap: &Heap, wrap_strings: bool, display_format: bool) -> String {
        match self {
            Value::String(p) => p
                .reify(heap)
                .as_string()
                .map(|s| {
                    if wrap_strings {
                        format!("\"{}\"", s)
                    } else {
                        s.to_string()
                    }
                })
                .unwrap(),
            Value::List(p) => {
                let mut result = String::from("[");
                let list = p.reify(heap).as_list().unwrap();

                for (i, value) in list.iter().enumerate() {
                    result.push_str(&value.stringify(heap, wrap_strings, display_format));
                    if i < list.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            Value::Record(p) => {
                let mut result = String::from("{");
                let record = p.reify(heap).as_record().unwrap();

                for (i, (key, value)) in record.iter().enumerate() {
                    result.push_str(&format!(
                        "{}: {}",
                        key,
                        value.stringify(heap, wrap_strings, display_format)
                    ));
                    if i < record.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push('}');
                result
            }
            Value::Lambda(p) => {
                let lambda = p.reify(heap).as_lambda().unwrap();
                let mut result = String::from("(");
                for (i, arg) in lambda.args.iter().enumerate() {
                    result.push_str(&arg.to_string());
                    if i < lambda.args.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(") => ");

                // Convert scope to SerializableValues and inline them
                let serializable_scope: IndexMap<String, SerializableValue> = lambda
                    .scope
                    .iter()
                    .filter_map(|(k, v)| {
                        SerializableValue::from_value(v, heap)
                            .ok()
                            .map(|sv| (k.to_string(), sv))
                    })
                    .collect();

                result.push_str(&crate::ast_to_source::expr_to_source_with_scope(
                    &lambda.body,
                    &serializable_scope,
                ));
                result
            }
            Value::BuiltIn(built_in) => {
                format!("{} (built-in)", built_in.name())
            }
            Value::Spread(p) => match p {
                IterablePointer::List(l) => {
                    let list = l.reify(heap).as_list().unwrap();
                    let mut result = String::from("...");
                    result.push_str(
                        &list
                            .iter()
                            .map(|v| v.stringify(heap, wrap_strings, display_format))
                            .collect::<String>(),
                    );
                    result
                }
                IterablePointer::String(s) => {
                    let string = s.reify(heap).as_string().unwrap();
                    format!("...{}", string)
                }
                IterablePointer::Record(r) => {
                    let record = r.reify(heap).as_record().unwrap();
                    let mut result = String::from("...");
                    result.push('{');
                    for (i, (key, value)) in record.iter().enumerate() {
                        result.push_str(&format!(
                            "{}: {}",
                            key,
                            value.stringify(heap, wrap_strings, display_format)
                        ));
                        if i < record.len() - 1 {
                            result.push_str(", ");
                        }
                    }
                    result.push('}');
                    result
                }
            },
            Value::Number(n) => {
                if display_format {
                    format_display_number(*n)
                } else {
                    plain_number(*n)
                }
            }
            Value::Bool(b) => b.to_string(),
            Value::Null => "null".to_string(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", format_display_number(*n)),
            Value::List(p) => write!(f, "{}", p),
            Value::Spread(p) => write!(f, "...{}", p),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Lambda(p) => write!(f, "{}", p),
            Value::String(p) => write!(f, "{}", p),
            Value::Null => write!(f, "null"),
            Value::BuiltIn(built_in) => write!(f, "{} (built-in)", built_in.name()),
            Value::Record(p) => write!(f, "{}", p),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_display_number_zero() {
        assert_eq!(format_display_number(0.0), "0");
        assert_eq!(format_display_number(-0.0), "-0"); // -0 is preserved
    }

    #[test]
    fn test_format_display_number_integers() {
        assert_eq!(format_display_number(1.0), "1");
        assert_eq!(format_display_number(123.0), "123");
        assert_eq!(format_display_number(1234.0), "1,234");
        assert_eq!(format_display_number(1234567.0), "1,234,567");
        assert_eq!(format_display_number(-1234567.0), "-1,234,567");
    }

    #[test]
    fn test_format_display_number_large_integers() {
        assert_eq!(
            format_display_number(999999999999999.0),
            "999,999,999,999,999"
        );
        assert_eq!(
            format_display_number(100000000000000.0),
            "100,000,000,000,000"
        );
    }

    #[test]
    fn test_format_display_number_decimals() {
        assert_eq!(format_display_number(3.7432), "3.7432");
        assert_eq!(format_display_number(1234.56), "1,234.56");
        assert_eq!(format_display_number(0.5), "0.5");
        assert_eq!(format_display_number(0.0001), "0.0001");
    }

    #[test]
    fn test_format_display_number_scientific_small() {
        // Numbers < 0.0001 should use scientific notation
        assert_eq!(format_display_number(0.00001), "1e-5");
        assert_eq!(format_display_number(0.00001234), "1.234e-5");
        assert_eq!(format_display_number(1e-10), "1e-10");
    }

    #[test]
    fn test_format_display_number_scientific_large() {
        // Numbers >= 1e15 should use scientific notation
        assert_eq!(format_display_number(1e15), "1e15");
        assert_eq!(format_display_number(1.5e15), "1.5e15");
        assert_eq!(format_display_number(1e20), "1e20");
    }

    #[test]
    fn test_format_display_number_special_values() {
        assert_eq!(format_display_number(f64::NAN), "NaN");
        assert_eq!(format_display_number(f64::INFINITY), "Infinity");
        assert_eq!(format_display_number(f64::NEG_INFINITY), "-Infinity");
    }

    #[test]
    fn test_format_display_number_negative() {
        assert_eq!(format_display_number(-3.7432), "-3.7432");
        assert_eq!(format_display_number(-1234.56), "-1,234.56");
        assert_eq!(format_display_number(-0.00001), "-1e-5");
        assert_eq!(format_display_number(-1e15), "-1e15");
    }

    #[test]
    fn test_stringify_internal_vs_display() {
        use crate::heap::Heap;

        let heap = Heap::new();
        let value = Value::Number(1234567.89);

        // stringify_internal uses plain formatting (for to_string)
        assert_eq!(value.stringify_internal(&heap), "1234567.89");

        // stringify_for_display uses fancy formatting (for format)
        assert_eq!(value.stringify_for_display(&heap), "1,234,567.89");
    }

    #[test]
    fn test_stringify_negative_zero() {
        use crate::heap::Heap;

        let heap = Heap::new();
        let neg_zero = Value::Number(-0.0);

        // Both stringify methods should preserve -0
        assert_eq!(neg_zero.stringify_internal(&heap), "-0");
        assert_eq!(neg_zero.stringify_for_display(&heap), "-0");
    }

    /// The whole numbers that are the first and the last of every length, the ones around
    /// every power of two up to the first that has no whole digits, and a few thousand of
    /// every length in between.
    fn whole_numbers() -> Vec<f64> {
        let mut numbers = vec![0.0];

        let mut power = 1_u64;
        while power < 1 << 53 {
            numbers.extend([power - 1, power, power + 1].map(|whole| whole as f64));
            power *= 2;
        }

        let mut power = 10_u64;
        while power < 1 << 53 {
            numbers.extend([power - 1, power, power + 1].map(|whole| whole as f64));
            power *= 10;
        }

        let mut bits = 0x9E37_79B9_7F4A_7C15_u64;
        for _ in 0..4000 {
            bits ^= bits << 13;
            bits ^= bits >> 7;
            bits ^= bits << 17;
            numbers.push((bits >> (11 + bits % 53)) as f64);
        }

        numbers.push(((1_u64 << 53) - 1) as f64);
        numbers.retain(|number| *number < (1_u64 << 53) as f64);

        let negated = numbers.iter().map(|number| -number).collect::<Vec<_>>();
        numbers.extend(negated);

        numbers
    }

    #[test]
    fn the_digits_of_a_whole_number_are_what_display_writes_for_it() {
        let numbers = whole_numbers();
        assert!(numbers.len() > 8000);

        for number in numbers {
            let mut room = [0; WHOLE_DIGITS];
            let digits = whole_digits(number, &mut room);

            // A zero with a sign has no digits of its own: it is written with the sign.
            if number == 0.0 && number.is_sign_negative() {
                assert_eq!(digits, None);
            } else {
                assert_eq!(digits, Some(number.to_string().as_str()), "{number:e}");
            }

            assert_eq!(plain_number(number), number.to_string(), "{number:e}");
        }
    }

    #[test]
    fn the_longest_whole_digits_fill_their_room() {
        let longest = -(((1_u64 << 53) - 1) as f64);
        let mut room = [0; WHOLE_DIGITS];

        assert_eq!(whole_digits(longest, &mut room), Some("-9007199254740991"));
        assert_eq!("-9007199254740991".len(), WHOLE_DIGITS);
    }

    #[test]
    fn a_number_that_is_not_whole_or_not_exact_has_no_whole_digits() {
        let limit = (1_u64 << 53) as f64;

        assert_eq!(whole_digits(-0.0, &mut [0; WHOLE_DIGITS]), None);
        assert_eq!(plain_number(-0.0), "-0");

        for number in [
            0.5,
            0.1,
            1.5,
            1e-300,
            f64::MIN_POSITIVE,
            f64::from_bits(1),
            4_294_967_295.5,
            4_503_599_627_370_495.5,
            limit,
            limit + 2.0,
            1e16,
            9.223372036854776e18,
            1.8446744073709552e19,
            1e300,
            f64::MAX,
            f64::INFINITY,
            f64::NAN,
        ] {
            for number in [number, -number] {
                assert_eq!(
                    whole_digits(number, &mut [0; WHOLE_DIGITS]),
                    None,
                    "{number:e}"
                );
                assert_eq!(plain_number(number), number.to_string(), "{number:e}");
            }
        }
    }
}
