//! Bytecode executed by the register VM in [`crate::vm`].
//!
//! Every lambda body and every top-level statement is compiled (see [`crate::compiler`]) into a
//! [`Proto`]: a flat instruction array plus the constant, name and debug tables the VM needs to
//! run it. Instructions are 8-byte words (a few are followed by a second word holding a number
//! operand) and operate on a window of registers in the VM's value stack; parameters occupy
//! the first registers of the window, block-local variables and temporaries follow.

use crate::{
    ast::{BinaryOp, Span, SpannedExpr},
    intern::Symbol,
    record::Shape,
    values::{LambdaArg, Value},
};
use std::{
    cell::{Cell, OnceCell},
    fmt, ptr,
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Op {
    /// `R[a] = R[b]`
    Move,
    /// `R[a] = K[bx]`
    LoadK,
    /// `R[a] = ` the number stored in the following code word.
    LoadNum,
    /// `R[a] = ` a freshly allocated copy of string literal `S[bx]`
    LoadStr,
    /// `R[a] = <unbound>`: marks a not-yet-assigned block variable as invisible to name lookups.
    LoadUnbound,

    /// `R[a] = ` dynamic by-name lookup of `N[bx]` through the frame chain. `x == 1` selects the
    /// record-shorthand flavour of the "unknown identifier" error (no span, no hint).
    GetName,
    /// Chunk frames only: `if R[a] is unbound { R[a] = lookup of N[bx] in the boundary
    /// environment }`. The value is then read straight from `R[a]` for the rest of the chunk.
    /// Names that are bound before the chunk starts need no such check (see
    /// [`Proto::preloaded`]).
    EnsureGlobal,
    /// `R[a] = ` the value captured in slot `b`, which is that of slot reference `T[c]`. When
    /// the slot was unbound at closure creation this falls back to the frame's self-binding
    /// and then to a by-name lookup; `x == 1` skips the self-binding (a parameter or block
    /// variable may shadow it).
    GetCapture,
    /// `GetCapture` (with the same meaning of `x`) for slot reference `T[bx]`, for references
    /// that do not fit its operands.
    GetCaptureRef,
    /// `R[a] = ` the parameter slot of reference `T[bx]` in the nearest call frame of the
    /// boundary environment, with a by-name fallback (slot references evaluated outside their
    /// own lambda).
    GetEnvParam,
    /// `R[a] = ` the capture slot of reference `T[bx]` in the nearest call frame of the
    /// boundary environment, with a by-name fallback.
    GetEnvCapture,
    /// `R[a] = inputs.S[bx]` (the `#field` shorthand).
    GetInput,

    // Binary operators: `R[a] = R[b] <op> R[c]`. Numbers take an inline fast path; everything
    // else goes through the shared broadcasting implementation. On the arithmetic operators
    // (in all three forms), `x` says which operand registers hold a temporary that nothing
    // reads after this instruction (see [`LAST_USE_B`] and [`LAST_USE_C`]).
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    DotEq,
    DotNe,
    DotLt,
    DotLe,
    DotGt,
    DotGe,
    And,
    NaturalAnd,
    Or,
    NaturalOr,
    Via,
    Into,
    Where,
    Coalesce,

    // `R[a] = R[b] <op> k`, where `k` is the number stored in the following code word.
    AddK,
    SubK,
    MulK,
    DivK,
    ModK,
    PowK,
    EqK,
    NeK,
    LtK,
    LeK,
    GtK,
    GeK,

    // `R[a] = k <op> R[b]`, where `k` is the number stored in the following code word.
    KAdd,
    KSub,
    KMul,
    KDiv,
    KMod,
    KPow,

    // Fused compare-and-branch, always followed by a `Jmp`: when `R[b] <op> R[c]` holds the
    // jump is skipped, otherwise it is taken.
    JmpNotEq,
    JmpNotNe,
    JmpNotLt,
    JmpNotLe,
    JmpNotGt,
    JmpNotGe,

    // The same against the number stored in the following code word: when `R[a] <op> k`
    // holds, execution continues after that word, otherwise `pc += sbx` (relative to it).
    JmpNotEqK,
    JmpNotNeK,
    JmpNotLtK,
    JmpNotLeK,
    JmpNotGtK,
    JmpNotGeK,

    // Fused remainder tests, for `R[a] % k == c` and `R[a] % k != c` where `k` is a power of
    // two and `c` is a whole number from zero up to it. Two code words follow, which say what
    // the test comes to for an `R[a]` that is a whole number: its bits under the mask in the
    // first word are those in the second (see [`remainder_test`]). When the comparison holds,
    // execution continues after those words, otherwise `pc += sbx` (relative to the first).
    JmpNotRemEq,
    JmpNotRemNe,

    /// `R[a] = (R[b] == null)`; `x == 1` when `null` is the left-hand operand.
    EqNull,
    /// `R[a] = (R[b] != null)`; `x == 1` when `null` is the left-hand operand.
    NeNull,
    /// `if !(R[a] == null) { pc += sbx }`; `x == 1` when `null` is the left-hand operand.
    JmpNotEqNull,
    /// `if !(R[a] != null) { pc += sbx }`; `x == 1` when `null` is the left-hand operand.
    JmpNotNeNull,

    /// `R[a] = -R[b]`
    Neg,
    /// `R[a] = !R[b]`
    Not,
    /// `R[a] = R[b]!`
    Factorial,

    /// `pc += sbx`
    Jmp,
    /// `if R[a] == false { pc += sbx }`; errors unless `R[a]` is a boolean.
    JmpIfFalse,

    /// `R[c] = R[a](R[a+1], ..., R[a+b])`. The arguments are passed as they are: a call with a
    /// spread among its arguments gathers them in a list instead (see `CallList`).
    Call,
    /// `R[c] = R[a](...)`: call with the elements of the list being built (see `ListBegin`) as
    /// arguments, consuming it.
    CallList,
    /// `R[c] = <the running function>(R[a+1], ..., R[a+n])`. Only emitted in the variant of a
    /// function's code that is specialised for calls through its own name (see [`SelfRef`]),
    /// where `n` (operand `x`) is the function's own parameter count: the frame calls itself
    /// without resolving anything. `R[a]` is scratch space for the callee, and `b` is the
    /// distance from the start of the code to the word after this instruction.
    CallSelf,
    /// The tail-call form of `CallSelf`, always followed by `Return c`: the frame restarts
    /// itself with the new arguments (one call level deeper) instead of calling. Only emitted
    /// where no variable of the frame is visible, so the frame being replaced cannot be
    /// observed. The arguments are in the parameter registers already, except for a run of
    /// `x` of them: these are in `R[a]` and the registers after it, which is where a call
    /// whose scratch space is `R[c]` takes the same arguments from, and still have to be
    /// moved. `b` is as for `CallSelf`.
    TailSelf,
    /// `TailSelf` with no argument left to move (`x == 0`).
    TailSelf0,
    /// `TailSelf` with one argument left to move (`x == 1`).
    TailSelf1,
    /// `return R[a]`
    Return,

    /// `R[a] = ` closure of lambda template `L[bx]`
    Closure,

    /// `R[a] = [R[b], ..., R[b+c-1]]`
    NewList,
    /// `R[a] = [R[b], ..., R[b+c-1]]`, flattening spread values.
    NewListSpread,
    /// Start building a list. `x` selects a capacity hint: 1 = the length of the list
    /// `R[a+1]`, 2 = the element count `R[a+1]` of an inlined range.
    ListBegin,
    /// Append `R[a], ..., R[a+b-1]` to the list being built, flattening spread values.
    ListAppend,
    /// `R[a] = ` the finished list.
    ListEnd,

    /// `R[a] = ` record of layout `H[c]` with the values `R[b], R[b+1], ...`
    NewRecord,
    /// Start building a record.
    RecordBegin,
    /// Insert `S[bx] => R[a]` into the record being built.
    RecordSet,
    /// Stage `R[a]` (which must be a string) as the key of the next `RecordValue`.
    RecordKey,
    /// Insert `<staged key> => R[a]` into the record being built.
    RecordValue,
    /// Merge the spread value `R[a]` into the record being built.
    RecordSpread,
    /// `R[a] = ` the finished record.
    RecordEnd,

    /// `R[a] = R[b][R[c]]`
    Index,
    /// `R[a] = R[b].<field>`, where the field is that of access site `c | x << 16`.
    GetField,
    /// `R[a] = ...R[b]`
    Spread,

    /// Raise the static error message `S[bx]` (with this instruction's span).
    Raise,
    /// Raise "already defined" unless `N[bx]` is unbound in the whole scope chain.
    CheckUnbound,
    /// If `R[a]` is a lambda, record `N[bx]` as its name. `x == 1` promises that the lambda was
    /// created by the preceding instruction.
    NameLambda,
    /// Bind `N[bx] = R[a]` in the boundary environment.
    SetGlobal,

    /// Adjust the frame's call depth by `sbx` (entering/leaving an inlined callback region).
    AddDepth,

    /// Validate the arguments of an inlined `range(R[b])` (`c == 1`) or `range(R[b], R[b+1])`
    /// (`c == 2`) call and set up the loop state: `R[a]` = index of the next element,
    /// `R[a+1]` = number of elements, `R[a+2]` = first element.
    RangePrep,
    /// Range iteration entry: `if R[a] >= R[a+1] { pc += sbx } else { R[a+3] = R[a+2] + R[a];
    /// R[a+4] = R[a] (only when x == 1); R[a] += 1 }`.
    ForRange,
    /// Range iteration back-edge: like `ForRange`, but jumps (by `sbx`) when there IS a next
    /// value and falls through when the range is exhausted.
    ForRangeNext,
    /// Start an inlined iteration over the list `R[a+1]`: checks the call depth and that the
    /// value is a list, then points the cursor `R[a]` at its first element and `R[a+4]` past
    /// its last one (both are raw addresses, never visible to the program).
    IterPrep,
    /// List iteration entry: `if R[a] == R[a+4] { pc += sbx } else { R[a+2] = *R[a]; R[a+3] =
    /// 0 (only when x == 1); R[a] += 1 }`.
    ForList,
    /// List iteration back-edge: `if R[a] != R[a+4] { R[a+2] = *R[a]; R[a+3] += 1 (only when
    /// x == 1); R[a] += 1; pc += sbx }`.
    ForListNext,
    /// Raise the "maximum call depth" error an out-of-line callback call would have produced
    /// when the frame is too deep. Executed once, before the first iteration.
    CallbackDepth,
    /// Append `R[a]` to the list being built.
    ListPush,
}

/// Operand `a` of an instruction (see [`Op::registers`]).
const OPERAND_A: u8 = 1;

/// Operand `b` of an instruction.
const OPERAND_B: u8 = 2;

/// Operand `c` of an instruction.
const OPERAND_C: u8 = 4;

impl Op {
    /// Which of the operands `a`, `b` and `c` of an instruction with this opcode name a
    /// register.
    const fn registers(self) -> u8 {
        const A: u8 = OPERAND_A;
        const B: u8 = OPERAND_B;
        const C: u8 = OPERAND_C;

        match self {
            Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Mod
            | Op::Pow
            | Op::Eq
            | Op::Ne
            | Op::Lt
            | Op::Le
            | Op::Gt
            | Op::Ge
            | Op::DotEq
            | Op::DotNe
            | Op::DotLt
            | Op::DotLe
            | Op::DotGt
            | Op::DotGe
            | Op::And
            | Op::NaturalAnd
            | Op::Or
            | Op::NaturalOr
            | Op::Via
            | Op::Into
            | Op::Where
            | Op::Coalesce
            | Op::Index => A | B | C,

            Op::Move
            | Op::AddK
            | Op::SubK
            | Op::MulK
            | Op::DivK
            | Op::ModK
            | Op::PowK
            | Op::EqK
            | Op::NeK
            | Op::LtK
            | Op::LeK
            | Op::GtK
            | Op::GeK
            | Op::KAdd
            | Op::KSub
            | Op::KMul
            | Op::KDiv
            | Op::KMod
            | Op::KPow
            | Op::EqNull
            | Op::NeNull
            | Op::Neg
            | Op::Not
            | Op::Factorial
            | Op::NewList
            | Op::NewListSpread
            | Op::NewRecord
            | Op::GetField
            | Op::Spread
            | Op::RangePrep => A | B,

            Op::Call
            | Op::CallList
            | Op::CallSelf
            | Op::TailSelf
            | Op::TailSelf0
            | Op::TailSelf1 => A | C,

            Op::JmpNotEq
            | Op::JmpNotNe
            | Op::JmpNotLt
            | Op::JmpNotLe
            | Op::JmpNotGt
            | Op::JmpNotGe => B | C,

            Op::LoadK
            | Op::LoadNum
            | Op::LoadStr
            | Op::LoadUnbound
            | Op::GetName
            | Op::EnsureGlobal
            | Op::GetCapture
            | Op::GetCaptureRef
            | Op::GetEnvParam
            | Op::GetEnvCapture
            | Op::GetInput
            | Op::JmpNotEqK
            | Op::JmpNotNeK
            | Op::JmpNotLtK
            | Op::JmpNotLeK
            | Op::JmpNotGtK
            | Op::JmpNotGeK
            | Op::JmpNotRemEq
            | Op::JmpNotRemNe
            | Op::JmpNotEqNull
            | Op::JmpNotNeNull
            | Op::JmpIfFalse
            | Op::Return
            | Op::Closure
            | Op::ListBegin
            | Op::ListAppend
            | Op::ListEnd
            | Op::RecordSet
            | Op::RecordKey
            | Op::RecordValue
            | Op::RecordSpread
            | Op::RecordEnd
            | Op::NameLambda
            | Op::SetGlobal
            | Op::ForRange
            | Op::ForRangeNext
            | Op::IterPrep
            | Op::ForList
            | Op::ForListNext
            | Op::ListPush => A,

            Op::Jmp
            | Op::RecordBegin
            | Op::Raise
            | Op::CheckUnbound
            | Op::AddDepth
            | Op::CallbackDepth => 0,
        }
    }
}

/// A register operand counts the halves of a value between the frame's first register and
/// the register it names.
pub(crate) const REGISTER_SCALE: usize = 2;

/// The number of registers a frame can address.
pub(crate) const MAX_REGISTERS: usize = (u16::MAX as usize + 1) / REGISTER_SCALE;

/// The operand that names register `index`.
const fn register_operand(index: u16) -> u16 {
    debug_assert!((index as usize) < MAX_REGISTERS);

    index * REGISTER_SCALE as u16
}

/// The index of the register that `operand` names.
#[inline(always)]
pub(crate) const fn register_index(operand: u16) -> usize {
    operand as usize / REGISTER_SCALE
}

/// A single instruction word. `x` is a small flag/extra operand, `a`, `b` and `c` are register,
/// constant or table indices; `b` and `c` combine into the 32-bit operand `bx`. Some
/// instructions are followed by a second code word holding a number operand.
///
/// An operand that names a register does not hold the register's index but twice that: the
/// distance of the register from the first one of the frame, in halves of a value. A value is
/// two machine words wide, and an address can be scaled by one word but not by two, so this
/// is the form in which an operand locates its register without first being shifted. The
/// constructors take register indices and do the conversion.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub(crate) struct Instr {
    pub op: Op,
    pub x: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}

const _: () = assert!(size_of::<Instr>() == size_of::<u64>());
const _: () = assert!(size_of::<Value>() % REGISTER_SCALE == 0);

/// The VM keeps this many registers free above the highest frame base it accepts without
/// checking, so a call into a frame no larger than this needs a single stack check.
pub(crate) const FAST_FRAME_SLOTS: usize = 256;

/// Set in `x` of an arithmetic instruction whose operand register `b` holds a temporary that
/// this instruction is the last to read.
pub(crate) const LAST_USE_B: u8 = 1;

/// The same for operand register `c`.
pub(crate) const LAST_USE_C: u8 = 2;

/// Set in `x` of a `MulK` that stands for a division: the program divides by the reciprocal
/// of the number operand, which is a power of two. Dividing by a power of two and
/// multiplying by its reciprocal round the same exact result, so they agree on every number,
/// and the multiplication takes a fraction of the time. Anything other than a number is
/// still divided.
pub(crate) const RECIPROCAL: u8 = 4;

/// The number that dividing by `divisor` is the same as multiplying by, when there is one:
/// `divisor` is a power of two whose reciprocal is one as well.
pub(crate) fn exact_reciprocal(divisor: f64) -> Option<f64> {
    const FRACTION: u64 = (1 << 52) - 1;

    let reciprocal = 1.0 / divisor;
    let exact = divisor.is_normal() && reciprocal.is_normal() && divisor.to_bits() & FRACTION == 0;

    exact.then_some(reciprocal)
}

/// How a fused remainder test decides `x % divisor == remainder` for a whole number `x`
/// without dividing: by the bits of `x` (as a two's complement `i64`) under `mask`, which
/// have to be `bits`.
///
/// For a remainder of zero, the mask covers the bits below the divisor: they are all clear
/// in exactly the multiples of a power of two, of either sign. A remainder above zero is
/// only left by a number that is not negative, as a remainder takes the sign of the number
/// that was divided, so the mask covers the sign bit as well.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct RemainderTest {
    pub mask: u64,
    pub bits: u64,
}

impl RemainderTest {
    const SIGN: u64 = 1 << 63;

    /// The largest divisor a test is made for. Every whole number up to it is a number.
    const MAX_DIVISOR: f64 = (1u64 << 52) as f64;

    /// The test for `x % divisor == remainder`, when the two make for one: the divisor is a
    /// power of two, and the remainder is a whole number that it can leave.
    pub fn new(divisor: f64, remainder: f64) -> Option<Self> {
        let whole = |number: f64| number.fract() == 0.0 && number.is_sign_positive();
        if !whole(divisor)
            || !whole(remainder)
            || !(1.0..=Self::MAX_DIVISOR).contains(&divisor)
            || remainder >= divisor
        {
            return None;
        }

        let (divisor, remainder) = (divisor as u64, remainder as u64);
        if !divisor.is_power_of_two() {
            return None;
        }

        Some(match remainder {
            0 => Self {
                mask: divisor - 1,
                bits: 0,
            },
            _ => Self {
                mask: (divisor - 1) | Self::SIGN,
                bits: remainder,
            },
        })
    }

    /// The test in the two code words at `words`.
    ///
    /// # Safety
    /// `words` must point at the code word after a fused remainder test.
    #[inline(always)]
    pub unsafe fn read(words: *const u64) -> Self {
        // SAFETY: the instruction is followed by the two code words of its test.
        unsafe {
            Self {
                mask: *words,
                bits: *words.add(1),
            }
        }
    }

    /// The divisor and the remainder the test was made for.
    pub fn operands(self) -> (f64, f64) {
        let divisor = (self.mask & !Self::SIGN) + 1;

        (divisor as f64, self.bits as f64)
    }

    /// Whether `whole % divisor == remainder`.
    #[inline(always)]
    pub fn holds_for(self, whole: i64) -> bool {
        whole as u64 & self.mask == self.bits
    }
}

impl Instr {
    /// An instruction with the operands `a`, `b` and `c`, where those that name a register
    /// are given as its index.
    pub fn abc(op: Op, a: u16, b: u16, c: u16) -> Self {
        let registers = op.registers();
        let operand = |value: u16, which: u8| match registers & which {
            0 => value,
            _ => register_operand(value),
        };

        Self {
            op,
            x: 0,
            a: operand(a, OPERAND_A),
            b: operand(b, OPERAND_B),
            c: operand(c, OPERAND_C),
        }
    }

    /// An instruction with the operands `a` (a register index, if it names a register) and
    /// `bx`.
    pub fn abx(op: Op, a: u16, bx: u32) -> Self {
        let registers = op.registers();
        debug_assert!(registers & (OPERAND_B | OPERAND_C) == 0);

        Self {
            op,
            x: 0,
            a: match registers & OPERAND_A {
                0 => a,
                _ => register_operand(a),
            },
            b: bx as u16,
            c: (bx >> 16) as u16,
        }
    }

    #[inline(always)]
    pub fn bx(self) -> usize {
        (self.b as usize) | ((self.c as usize) << 16)
    }

    /// The signed jump offset stored in `bx`, relative to the following code word.
    #[inline(always)]
    pub fn sbx(self) -> isize {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32 as isize
    }

    /// The code word holding this instruction.
    pub fn encode(self) -> u64 {
        // SAFETY: `Instr` is eight bytes of plain data without padding.
        unsafe { std::mem::transmute::<Instr, u64>(self) }
    }

    /// The instruction held by a code word.
    ///
    /// # Safety
    /// `word` must have been produced by [`Instr::encode`].
    pub unsafe fn decode(word: u64) -> Self {
        // SAFETY: the caller guarantees the word holds a valid instruction.
        unsafe { std::mem::transmute::<u64, Instr>(word) }
    }

    /// Whether the instruction is followed by a code word holding a number operand.
    pub fn has_number_operand(self) -> bool {
        matches!(
            self.op,
            Op::LoadNum
                | Op::AddK
                | Op::SubK
                | Op::MulK
                | Op::DivK
                | Op::ModK
                | Op::PowK
                | Op::EqK
                | Op::NeK
                | Op::LtK
                | Op::LeK
                | Op::GtK
                | Op::GeK
                | Op::KAdd
                | Op::KSub
                | Op::KMul
                | Op::KDiv
                | Op::KMod
                | Op::KPow
                | Op::JmpNotEqK
                | Op::JmpNotNeK
                | Op::JmpNotLtK
                | Op::JmpNotLeK
                | Op::JmpNotGtK
                | Op::JmpNotGeK
        )
    }

    /// The number of code words after the instruction that hold operands of it.
    pub fn operand_words(self) -> usize {
        match self.op {
            Op::JmpNotRemEq | Op::JmpNotRemNe => 2,
            _ => self.has_number_operand() as usize,
        }
    }
}

/// How an error escaping an inlined region is decorated, mirroring what the equivalent
/// out-of-line call would have added on the way out.
#[derive(Clone, Debug)]
pub(crate) enum Wrap {
    /// Prefix the message with `in <name>: `.
    FunctionContext(Rc<str>),
    /// Attach the call-site span unless the error already carries a more specific one.
    CallSite(Span),
}

/// An instruction range whose errors get extra context (an inlined built-in or callback).
#[derive(Clone, Debug)]
pub(crate) struct Handler {
    pub start: u32,
    pub end: u32,
    pub wraps: Vec<Wrap>,
}

/// A variable that lives in a register and is visible to by-name lookups (from this frame or
/// from callee frames, since scoping is dynamic) while the frame's pc is in `start..end`.
#[derive(Clone, Debug)]
pub(crate) struct LocalVar {
    pub name: Symbol,
    pub reg: u16,
    pub start: u32,
    pub end: u32,
}

/// Where a closure finds the value of one of its captures at creation time.
#[derive(Clone, Copy, Debug)]
pub(crate) enum CaptureSource {
    /// The name is statically known to live in this register of the creating frame.
    Register(u16),
    /// Resolve the name dynamically through the frame chain.
    Dynamic,
}

/// Everything needed to turn a lambda expression into a function value.
#[derive(Debug)]
pub(crate) struct LambdaTemplate {
    pub args: Rc<Vec<LambdaArg>>,
    pub body: Rc<SpannedExpr>,
    pub captures: Rc<Vec<Symbol>>,
    pub capture_sources: Vec<CaptureSource>,

    /// The compiled body, filled in the first time a closure is created from this template.
    pub proto: OnceCell<Rc<Proto>>,
}

/// The layout of a record literal whose keys are all static.
#[derive(Debug)]
pub(crate) struct RecordLayout {
    /// Every distinct key in first-occurrence order. This is the canonical shape of those
    /// keys, so every record built from literals with the same keys shares it.
    pub shape: Rc<Shape>,

    /// For each entry of the literal, the position of its key in `shape` (a repeated key
    /// maps to the position of its first occurrence, which later entries overwrite).
    pub slots: Vec<u16>,

    /// Whether no key is repeated, so that entry `i` of the literal is the value at
    /// position `i`.
    pub distinct: bool,
}

/// What a `GetField` access site remembers about the last record it found its field in.
#[derive(Debug)]
pub(crate) struct FieldCache {
    /// The shape of that record if it is a canonical one, null otherwise. Canonical shapes
    /// are never freed, so the address is enough to recognise the shape again.
    pub shape: Cell<*const Shape>,

    /// The position the field was found at.
    pub position: Cell<u32>,
}

impl FieldCache {
    pub fn new() -> Self {
        Self {
            shape: Cell::new(ptr::null()),
            position: Cell::new(0),
        }
    }
}

/// A capture a function calls with exactly its own (all required) parameters. When the
/// function is named `name` and its capture slot `slot` is unbound, that capture is the
/// function itself, and such calls can run `variant`: the same body compiled with them as
/// direct self calls.
#[derive(Debug)]
pub(crate) struct SelfRef {
    pub name: Symbol,
    pub slot: u16,

    /// Compiled the first time it is needed; `None` when the variant could not be compiled.
    pub variant: OnceCell<Option<Rc<Proto>>>,
}

pub(crate) struct Proto {
    /// Instruction words, interleaved with the number operands of the instructions that take
    /// one.
    pub code: Vec<u64>,
    pub consts: Vec<Value>,
    pub strings: Vec<String>,
    pub names: Vec<Symbol>,

    /// Parameter and capture slot references: the slot index and the name to fall back to.
    pub slot_refs: Vec<(u16, Symbol)>,
    pub lambdas: Vec<LambdaTemplate>,
    pub layouts: Vec<RecordLayout>,

    /// The field name of each `GetField` access site.
    pub fields: Vec<String>,

    /// Per-code-word index into `spans` (`u32::MAX` when the instruction has no span).
    pub span_index: Vec<u32>,
    pub spans: Vec<Span>,

    pub locals: Vec<LocalVar>,
    pub handlers: Vec<Handler>,

    /// One inline cache per `GetField` access site.
    pub field_caches: Vec<FieldCache>,

    /// Parameter names in declaration order; parameter `i` lives in register `i`.
    pub params: Vec<Symbol>,

    /// True when every parameter is required, so a call with exactly `params.len()` arguments
    /// needs no argument adjustment.
    pub simple_params: bool,

    /// `params.len()` when `simple_params` holds, `u32::MAX` otherwise.
    pub simple_arity: u32,

    /// `simple_arity` when a frame of the code is no larger than [`FAST_FRAME_SLOTS`], so
    /// that a call with that many arguments has nothing to check about the code or to adjust,
    /// `u32::MAX` otherwise.
    pub fast_arity: u32,

    /// The captures this function may be calling itself through (always empty in a variant
    /// that is already specialised for one).
    pub self_refs: Vec<SelfRef>,

    /// Chunks only: the first `cache_regs` registers cache late-bound names and start unbound.
    pub cache_regs: usize,

    /// Chunks only: the cache registers (and names) the code reads without checking them,
    /// because the names were bound when the chunk was compiled. They are loaded before the
    /// chunk runs.
    pub preloaded: Vec<(u16, Symbol)>,

    /// Number of registers a frame running this proto needs.
    pub max_stack: usize,

    /// The lambda body and parameter list this proto was compiled from (absent for chunks),
    /// used to check that a cached proto still belongs to the definition being called.
    pub body: Option<Rc<SpannedExpr>>,
    pub args: Option<Rc<Vec<LambdaArg>>>,
}

impl Proto {
    #[inline]
    pub fn span_at(&self, pc: usize) -> Option<Span> {
        let index = *self.span_index.get(pc)?;
        self.spans.get(index as usize).copied()
    }

    /// Whether this proto was compiled from exactly this body and parameter list.
    #[inline(always)]
    pub fn belongs_to(&self, body: &Rc<SpannedExpr>, args: &Rc<Vec<LambdaArg>>) -> bool {
        self.body.as_ref().is_some_and(|own| Rc::ptr_eq(own, body))
            && self.args.as_ref().is_some_and(|own| Rc::ptr_eq(own, args))
    }
}

impl fmt::Debug for Proto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "proto(params: {:?}, max_stack: {})",
            self.params, self.max_stack
        )?;

        let mut pc = 0;
        while pc < self.code.len() {
            // SAFETY: `pc` only ever lands on instruction words; number operands are skipped.
            let instr = unsafe { Instr::decode(self.code[pc]) };

            // Register operands are shown as the register indices they stand for.
            let registers = instr.op.registers();
            let shown = |operand: u16, which: u8| match registers & which {
                0 => operand as usize,
                _ => register_index(operand),
            };
            write!(
                f,
                "  {:4}  {:<14} x={} a={} b={} c={}",
                pc,
                format!("{:?}", instr.op),
                instr.x,
                shown(instr.a, OPERAND_A),
                shown(instr.b, OPERAND_B),
                shown(instr.c, OPERAND_C)
            )?;
            pc += 1;

            if instr.has_number_operand() {
                write!(f, " k={}", f64::from_bits(self.code[pc]))?;
            } else if instr.operand_words() == 2 {
                write!(f, " mask={:#x} bits={}", self.code[pc], self.code[pc + 1])?;
            }
            pc += instr.operand_words();

            writeln!(f)?;
        }

        for (index, lambda) in self.lambdas.iter().enumerate() {
            match lambda.proto.get() {
                Some(proto) => writeln!(f, "lambda {}: {:?}", index, proto)?,
                None => writeln!(f, "lambda {}: <not compiled>", index)?,
            }
        }

        Ok(())
    }
}

/// The opcode implementing `op` on two register operands.
pub(crate) fn binary_opcode(op: BinaryOp) -> Op {
    match op {
        BinaryOp::Add => Op::Add,
        BinaryOp::Subtract => Op::Sub,
        BinaryOp::Multiply => Op::Mul,
        BinaryOp::Divide => Op::Div,
        BinaryOp::Modulo => Op::Mod,
        BinaryOp::Power => Op::Pow,
        BinaryOp::Equal => Op::Eq,
        BinaryOp::NotEqual => Op::Ne,
        BinaryOp::Less => Op::Lt,
        BinaryOp::LessEq => Op::Le,
        BinaryOp::Greater => Op::Gt,
        BinaryOp::GreaterEq => Op::Ge,
        BinaryOp::DotEqual => Op::DotEq,
        BinaryOp::DotNotEqual => Op::DotNe,
        BinaryOp::DotLess => Op::DotLt,
        BinaryOp::DotLessEq => Op::DotLe,
        BinaryOp::DotGreater => Op::DotGt,
        BinaryOp::DotGreaterEq => Op::DotGe,
        BinaryOp::And => Op::And,
        BinaryOp::NaturalAnd => Op::NaturalAnd,
        BinaryOp::Or => Op::Or,
        BinaryOp::NaturalOr => Op::NaturalOr,
        BinaryOp::Via => Op::Via,
        BinaryOp::Into => Op::Into,
        BinaryOp::Where => Op::Where,
        BinaryOp::Coalesce => Op::Coalesce,
    }
}
