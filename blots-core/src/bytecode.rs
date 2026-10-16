//! Bytecode executed by the register VM in [`crate::vm`].
//!
//! Every lambda body and every top-level statement is compiled (see [`crate::compiler`]) into a
//! [`Proto`]: a flat instruction array plus the constant, name and debug tables the VM needs to
//! run it. Instructions are fixed-width (8 bytes) and operate on a window of registers in the
//! VM's value stack; parameters occupy the first registers of the window, block-local
//! variables and temporaries follow.

use crate::{
    ast::{BinaryOp, Span, SpannedExpr},
    intern::Symbol,
    values::{LambdaArg, Value},
};
use indexmap::IndexMap;
use std::{
    cell::{Cell, OnceCell},
    fmt,
    rc::Rc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Op {
    /// `R[a] = R[b]`
    Move,
    /// `R[a] = K[bx]`
    LoadK,
    /// `R[a] = ` a freshly allocated copy of string literal `S[bx]`
    LoadStr,
    /// `R[a] = <unbound>`: marks a not-yet-assigned block variable as invisible to name lookups.
    LoadUnbound,

    /// `R[a] = ` dynamic by-name lookup of `N[bx]` through the frame chain. `x == 1` selects the
    /// record-shorthand flavour of the "unknown identifier" error (no span, no hint).
    GetName,
    /// Chunk frames only: `if R[a] is unbound { R[a] = lookup of N[bx] in the boundary
    /// environment }`. The value is then read straight from `R[a]` for the rest of the chunk.
    EnsureGlobal,
    /// `R[a] = ` the captured value of slot reference `T[bx]`, falling back to the frame's
    /// self-binding and then to a by-name lookup when the slot was unbound at closure creation.
    GetCapture,
    /// Like `GetCapture`, but the fallback goes straight to the generic by-name lookup (used
    /// when a parameter or block variable may shadow the self-binding).
    GetCaptureDyn,
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
    // else goes through the shared broadcasting implementation.
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

    // `R[a] = R[b] <op> F[c]`
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

    // `R[a] = F[b] <op> R[c]`
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

    // The same against a numeric constant: `R[b] <op> F[c]`.
    JmpNotEqK,
    JmpNotNeK,
    JmpNotLtK,
    JmpNotLeK,
    JmpNotGtK,
    JmpNotGeK,

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

    /// `R[c] = R[a](R[a+1], ..., R[a+b])`. `x == 1` promises that no argument can be a spread
    /// value, so the arguments need no flattening check.
    Call,
    /// `R[c] = R[a](...)`: call with the elements of the list being built (see `ListBegin`) as
    /// arguments, consuming it.
    CallList,
    /// `return R[a]`
    Return,

    /// `R[a] = ` closure of lambda template `L[bx]`
    Closure,

    /// `R[a] = [R[b], ..., R[b+c-1]]`
    NewList,
    /// Start building a list. `x` selects a capacity hint: 1 = the length of the list
    /// `R[a+1]`, 2 = the element count `R[a+1]` of an inlined range.
    ListBegin,
    /// Append `R[a], ..., R[a+b-1]` to the list being built, flattening spread values.
    ListAppend,
    /// `R[a] = ` the finished list.
    ListEnd,

    /// `R[a] = ` record of shape `H[c]` with the values `R[b], R[b+1], ...`
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
    /// If `R[a]` is a lambda, record `N[bx]` as its name.
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
    /// value is a list, then resets the element counter `R[a]`.
    IterPrep,
    /// List iteration entry: `if R[a] >= len(R[a+1]) { pc += sbx } else { R[a+2] =
    /// R[a+1][R[a]]; R[a+3] = R[a] (only when x == 1); R[a] += 1 }`.
    ForList,
    /// List iteration back-edge: like `ForList`, but jumps (by `sbx`) when there IS a next
    /// element and falls through when the list is exhausted.
    ForListNext,
    /// Raise the "maximum call depth" error an out-of-line callback call would have produced
    /// when the frame is too deep. Executed once, before the first iteration.
    CallbackDepth,
    /// Append `R[a]` to the list being built.
    ListPush,
}

/// A single fixed-width instruction. `x` is a small flag/extra operand, `a`, `b` and `c` are
/// register, constant or table indices; `b` and `c` combine into the 32-bit operand `bx`.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub(crate) struct Instr {
    pub op: Op,
    pub x: u8,
    pub a: u16,
    pub b: u16,
    pub c: u16,
}

impl Instr {
    pub fn abc(op: Op, a: u16, b: u16, c: u16) -> Self {
        Self { op, x: 0, a, b, c }
    }

    pub fn abx(op: Op, a: u16, bx: u32) -> Self {
        Self {
            op,
            x: 0,
            a,
            b: bx as u16,
            c: (bx >> 16) as u16,
        }
    }

    #[inline(always)]
    pub fn bx(self) -> usize {
        (self.b as usize) | ((self.c as usize) << 16)
    }

    /// The signed jump offset stored in `bx`, relative to the following instruction.
    #[inline(always)]
    pub fn sbx(self) -> isize {
        ((self.b as u32) | ((self.c as u32) << 16)) as i32 as isize
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
pub(crate) struct Shape {
    /// A record holding every distinct key in first-occurrence order.
    pub template: IndexMap<String, Value>,

    /// For each entry of the literal, the position of its key in `template` (a repeated key
    /// maps to the position of its first occurrence, which later entries overwrite).
    pub slots: Vec<u16>,
}

pub(crate) struct Proto {
    pub code: Vec<Instr>,
    pub consts: Vec<Value>,

    /// Numeric constants used as immediate operands.
    pub nums: Vec<f64>,
    pub strings: Vec<String>,
    pub names: Vec<Symbol>,

    /// Parameter and capture slot references: the slot index and the name to fall back to.
    pub slot_refs: Vec<(u16, Symbol)>,
    pub lambdas: Vec<LambdaTemplate>,
    pub shapes: Vec<Shape>,

    /// The field name of each `GetField` access site.
    pub fields: Vec<String>,

    /// Per-instruction index into `spans` (`u32::MAX` when the instruction has no span).
    pub span_index: Vec<u32>,
    pub spans: Vec<Span>,

    pub locals: Vec<LocalVar>,
    pub handlers: Vec<Handler>,

    /// One inline-cache slot per `GetField` access site: the entry index at which the field
    /// was last found.
    pub field_caches: Vec<Cell<u32>>,

    /// Parameter names in declaration order; parameter `i` lives in register `i`.
    pub params: Vec<Symbol>,

    /// True when every parameter is required, so a call with exactly `params.len()` arguments
    /// needs no argument adjustment.
    pub simple_params: bool,

    /// Chunks only: the first `cache_regs` registers cache late-bound names and start unbound.
    pub cache_regs: usize,

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
}

impl fmt::Debug for Proto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "proto(params: {:?}, max_stack: {})",
            self.params, self.max_stack
        )?;

        for (pc, instr) in self.code.iter().enumerate() {
            writeln!(
                f,
                "  {:4}  {:<14} x={} a={} b={} c={}",
                pc,
                format!("{:?}", instr.op),
                instr.x,
                instr.a,
                instr.b,
                instr.c
            )?;
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
