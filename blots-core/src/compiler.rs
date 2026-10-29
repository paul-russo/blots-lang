//! Compiles AST expressions into register bytecode (see [`crate::bytecode`]).
//!
//! Name resolution mirrors the scoping rules of the language exactly:
//!
//! * Lambda parameters and variables assigned by do-block statements live in registers and are
//!   read directly wherever the binding is statically known.
//! * Everything else is late-bound. Scoping is dynamic (a call frame's parent scope is its
//!   call site), so unresolved names are looked up by name through the frame chain at runtime.
//!   The per-proto [`LocalVar`] table tells that lookup which registers hold which names at
//!   any given instruction.
//! * Assignments used as expressions (rather than as do-block statements) may or may not have
//!   executed by the time a name is read, so the variables they create are "maybe" variables:
//!   their register starts out unbound and every read of such a name goes through the dynamic
//!   lookup.
//!
//! Calls to `map`/`filter`/`reduce`/`every`/`some` with a literal lambda callback are compiled
//! into inline loops: the callback's parameters become registers of the enclosing frame and no
//! call frame is pushed per element. The loop reproduces everything the out-of-line call would
//! have done that a program can observe (call-depth accounting, argument checks, error
//! context), and `range(...)`, `keys(...)`, `values(...)` and `entries(...)` sources are
//! iterated without materialising the list.

use crate::{
    ast::{
        BinaryOp, Commented, Expr, PostfixOp, RecordEntry, RecordKey, Span, SpannedExpr, UnaryOp,
    },
    bytecode::{
        CaptureSource, EntryPart, FAST_FRAME_SLOTS, FieldCache, Handler, Instr, LAST_USE_B,
        LAST_USE_C, LambdaTemplate, Literal, LocalVar, MAX_REGISTERS, Op, Proto, RECIPROCAL,
        RecordLayout, RemainderTest, SelfRef, Wrap, binary_opcode, exact_reciprocal, small_divisor,
    },
    environment::Environment,
    error::RuntimeError,
    functions::{BuiltInFunction, is_built_in_function},
    heap::RecordPointer,
    intern::Symbol,
    record::Keys,
    values::{LambdaArg, Value},
};
use indexmap::IndexSet;
use std::{borrow::Borrow, cell::OnceCell, collections::HashMap, hash::Hash, rc::Rc};

type CResult<T> = Result<T, RuntimeError>;

/// List literals up to this many elements (and without spreads) are built by one instruction
/// from consecutive registers; larger ones are appended in batches of this size.
const LIST_BATCH: usize = 64;

/// Record literals with only static keys and at most this many entries are built by one
/// instruction from consecutive registers.
const MAX_SHAPE_KEYS: usize = 64;

/// Upper bound on the number of late-bound names a chunk caches in registers.
const MAX_CACHED_GLOBALS: usize = 256;

/// Upper bound on the number of late-bound names that an inlined loop of a function caches
/// in registers, each of which is cleared whenever the loop starts.
const MAX_LOOP_NAMES: usize = 16;

/// How much deeper than its caller the callback of a higher-order built-in is called: one
/// level for the built-in itself, one for the callback call. The body of a callback runs one
/// level further in than it is called from.
const CALLBACK_CALL_DEPTH: u16 = 2;

/// The most arguments a direct self call can pass: its argument count is an 8-bit operand.
const MAX_SELF_CALL_ARGS: usize = u8::MAX as usize;

/// A tail call of a function to itself tracks which parameters each of its arguments reads,
/// for this many of the function's first parameters. A later parameter is taken to be read by
/// every argument.
const TRACKED_PARAMS: usize = u64::BITS as usize;

/// A call with at most this many arguments evaluates them into consecutive registers; one
/// with more gathers them in a list, so that the size of a call is not bounded by the number
/// of registers a function has.
const MAX_DIRECT_ARGS: usize = 255;

/// The number of record layouts an instruction can tell apart.
const MAX_LAYOUTS: usize = u16::MAX as usize + 1;

/// The most expressions of an inlined callback that are computed ahead of its loop, each of
/// which keeps a register for as long as the loop runs.
const MAX_HOISTED: usize = 8;

/// An expression of more nodes than this is not checked for being the same on every pass
/// through a loop; the expressions it is made of still are.
const MAX_INVARIANT_NODES: usize = 32;

/// The most expressions of one condition whose values are kept for its branches, each of
/// which keeps a register until the branches end.
const MAX_SHARED: usize = 4;

/// The most values that are kept for the branches of all the conditions around one point
/// of a function.
const MAX_SHARED_AT_ONCE: usize = 16;

/// The number of nodes a function's branches are searched through, all in all, for
/// expressions that their conditions evaluate as well.
const SHARED_SEARCH_NODES: usize = 4096;

/// Values are kept for branches where fewer registers than this are taken, so that keeping
/// them leaves a function its place among those with small frames.
const MAX_SHARING_FRAME: usize = FAST_FRAME_SLOTS / 2;

/// A table of up to this many entries is searched from the start for an entry.
const MAX_SCANNED_ENTRIES: usize = 16;

/// The distinct strings or names an instruction of a function refers to by index, in the
/// order they were first used.
///
/// Most functions refer to a handful, and comparing against each is the fastest way to find
/// one. A table pasted into a program can hold tens of thousands of different strings,
/// though, so a table that outgrows a scan is indexed.
struct Table<T> {
    entries: Vec<T>,

    /// The position of every entry, once there are more than [`MAX_SCANNED_ENTRIES`].
    positions: HashMap<T, u32>,
}

impl<T: Clone + Eq + Hash> Table<T> {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            positions: HashMap::new(),
        }
    }

    /// The index of `entry`, which is added if the table does not hold it yet.
    fn index_of<Q>(&mut self, entry: &Q) -> u32
    where
        T: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = T> + ?Sized,
    {
        let next = self.entries.len() as u32;

        if self.entries.len() <= MAX_SCANNED_ENTRIES {
            let found = self.entries.iter().position(|own| own.borrow() == entry);
            if let Some(position) = found {
                return position as u32;
            }
        } else {
            if self.positions.is_empty() {
                self.positions = self.entries.iter().cloned().zip(0..).collect();
            }

            match self.positions.get(entry) {
                Some(position) => return *position,
                None => self.positions.insert(entry.to_owned(), next),
            };
        }

        self.entries.push(entry.to_owned());

        next
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ScopeKind {
    /// The function body or chunk itself.
    Root,
    /// A do-block.
    Block,
    /// The body of an inlined callback; its variables are the callback's parameters.
    Callback,
}

struct ScopeVar {
    name: Symbol,
    reg: u16,

    /// True when the variable is created by an assignment expression that may not have run
    /// yet; such names always resolve dynamically.
    maybe: bool,
}

struct Scope {
    kind: ScopeKind,
    vars: Vec<ScopeVar>,

    /// Registers of an inlined callback's parameters, by parameter position.
    param_regs: Vec<u16>,

    /// Indices into the proto's local table whose visibility ends with this scope.
    locals: Vec<usize>,

    /// First register owned by this scope; everything from here up is released on exit.
    base: u16,
}

enum Resolved {
    /// The name is bound to this register at this point of the program.
    Register(u16),
    /// The name is not bound by any enclosing scope of the chunk, so it resolves through the
    /// boundary environment and is cached in this register. In a function, the name is not
    /// bound by any enclosing scope of an inlined loop, and what the frame finds for it by
    /// name is cached in this register for as long as the loop runs.
    Global(Symbol, u16),
    /// The name has to be looked up at runtime.
    Dynamic,
}

#[derive(Clone, Copy)]
enum Owner {
    Chunk,
    Function,
    Callback(usize),
}

#[derive(Clone, Copy)]
enum Target {
    Register(u16),
    Return,
}

/// A call to a higher-order built-in that can be compiled as an inline loop, or an operator
/// that goes through a list the way one of them does.
pub(crate) struct InlineCall<'e> {
    /// The built-in function that is called, or the one that does to a list what the
    /// operator does to it: `map` for `via` and `filter` for `where`.
    builtin: BuiltInFunction,
    list: &'e SpannedExpr,
    params: &'e [LambdaArg],
    body: &'e SpannedExpr,
    init: Option<&'e SpannedExpr>,

    /// The operator and its right-hand side, which is the lambda, when this is not a call.
    operator: Option<(BinaryOp, &'e SpannedExpr)>,
}

/// The parameters and the body of `callback` when it is a literal lambda the compiler can
/// inline. It must take only required parameters, in a number that it would be called with
/// (`min_params`, or one more for the index of the element), and must not assign variables
/// directly in its call frame.
fn inline_callback(
    callback: &SpannedExpr,
    min_params: usize,
) -> Option<(&[LambdaArg], &SpannedExpr)> {
    let Expr::Lambda {
        args: params, body, ..
    } = &callback.node
    else {
        return None;
    };

    if params.len() < min_params
        || params.len() > min_params + 1
        || !params.iter().all(LambdaArg::is_required)
    {
        return None;
    }

    let mut assigned = Vec::new();
    collect_assignment_exprs(body, &mut assigned);
    if !assigned.is_empty() {
        return None;
    }

    Some((params, body))
}

/// Recognise `map(list, lambda)`-style calls whose callback is a literal lambda the compiler
/// can inline (see [`inline_callback`]).
fn inline_call<'e>(func: &SpannedExpr, args: &'e [SpannedExpr]) -> Option<InlineCall<'e>> {
    let Expr::BuiltIn(builtin) = &func.node else {
        return None;
    };
    let (arg_count, min_params) = match builtin {
        BuiltInFunction::Map
        | BuiltInFunction::Filter
        | BuiltInFunction::Every
        | BuiltInFunction::Some => (2, 1),
        BuiltInFunction::Reduce => (3, 2),
        _ => return None,
    };

    if args.len() != arg_count || args.iter().any(|arg| matches!(arg.node, Expr::Spread(_))) {
        return None;
    }

    let (params, body) = inline_callback(&args[1], min_params)?;

    Some(InlineCall {
        builtin: *builtin,
        list: &args[0],
        params,
        body,
        init: args.get(2),
        operator: None,
    })
}

/// Recognise `list via lambda` and `list where lambda` with a literal lambda the compiler can
/// inline (see [`inline_callback`]). What the operator does to a list is what `map` and
/// `filter` do to it, apart from how deep the lambda is called and what an error is told
/// about where it comes from. What is on the left may turn out to be something else than a
/// list, which the operator is left to deal with as it does when nothing is inlined.
fn inline_operator<'e>(
    op: BinaryOp,
    left: &'e SpannedExpr,
    right: &'e SpannedExpr,
) -> Option<InlineCall<'e>> {
    let builtin = match op {
        BinaryOp::Via => BuiltInFunction::Map,
        BinaryOp::Where => BuiltInFunction::Filter,
        _ => return None,
    };
    let (params, body) = inline_callback(right, 1)?;

    Some(InlineCall {
        builtin,
        list: left,
        params,
        body,
        init: None,
        operator: Some((op, right)),
    })
}

/// The arguments of `list` when it is a plain `range(a)` / `range(a, b)` call.
fn range_call(list: &SpannedExpr) -> Option<&[SpannedExpr]> {
    let Expr::Call { func, args } = &list.node else {
        return None;
    };

    if !matches!(func.node, Expr::BuiltIn(BuiltInFunction::Range))
        || !(1..=2).contains(&args.len())
        || args.iter().any(|arg| matches!(arg.node, Expr::Spread(_)))
    {
        return None;
    }

    Some(args)
}

/// A call that lists the entries of a record, whose list an inlined loop goes through.
struct EntriesCall<'e> {
    built_in: BuiltInFunction,

    /// What the list is made of.
    part: EntryPart,
    record: &'e SpannedExpr,
}

/// What `list` lists of the entries of which record, when it is a plain `keys(record)`,
/// `values(record)` or `entries(record)` call.
fn entries_call(list: &SpannedExpr) -> Option<EntriesCall<'_>> {
    let Expr::Call { func, args } = &list.node else {
        return None;
    };
    let Expr::BuiltIn(built_in) = &func.node else {
        return None;
    };

    let part = match built_in {
        BuiltInFunction::Keys => EntryPart::Key,
        BuiltInFunction::Values => EntryPart::Value,
        BuiltInFunction::Entries => EntryPart::Pair,
        _ => return None,
    };

    match &args[..] {
        [record] if !matches!(record.node, Expr::Spread(_)) => Some(EntriesCall {
            built_in: *built_in,
            part,
            record,
        }),
        _ => None,
    }
}

/// The part of an entry that the element at `index` of the list `entries` makes of it is,
/// when `index` is a literal position of one: the key comes first and the value second.
fn pair_part(index: &SpannedExpr) -> Option<EntryPart> {
    match index.node {
        Expr::Number(0.0) => Some(EntryPart::Key),
        Expr::Number(1.0) => Some(EntryPart::Value),
        _ => None,
    }
}

/// Whether the callback of `call`, which goes through `entries` of a record, only ever
/// reads the key or the value of the list that it is given for an entry, as `name[0]` or
/// `name[1]`: nothing else can tell that the list is not made, so the loop gives the
/// callback the value alone (see `Op::LoadEntry`).
///
/// A list that is not read in one of those two ways is made, and so is one that something
/// may find by name: a function that is called looks up the names it does not bind
/// through the frame of its caller, and a lambda takes what the name is bound to with it.
/// The body may call built-in functions that call no function, and loops that are inlined
/// themselves.
fn takes_entries_apart(call: &InlineCall, name: Symbol) -> bool {
    let distinct = call.params.iter().enumerate().all(|(index, param)| {
        call.params[..index]
            .iter()
            .all(|other| other.get_name() != param.get_name())
    });

    distinct && !is_constant_name(name) && only_takes_apart(call.body, name)
}

/// Whether `expr` reads `name` only as `name[0]` or `name[1]`, and nothing that it does can
/// find `name` otherwise (see [`takes_entries_apart`]).
fn only_takes_apart(expr: &SpannedExpr, name: Symbol) -> bool {
    let apart = |expr: &SpannedExpr| only_takes_apart(expr, name);

    match &expr.node {
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::InputReference(_)
        | Expr::BuiltIn(_) => true,
        Expr::Identifier(_) | Expr::ParamSlot { .. } | Expr::CaptureSlot { .. } => {
            variable_name(expr) != Some(name)
        }
        Expr::Access {
            expr: object,
            index,
        } => match variable_name(object) == Some(name) && pair_part(index).is_some() {
            true => true,
            false => apart(object) && apart(index),
        },
        Expr::List(items) => items.iter().all(|item| apart(&item.node)),
        Expr::Record(entries) => entries.iter().all(|entry| {
            let entry = &entry.node;

            match &entry.key {
                RecordKey::Static(_) => apart(&entry.value),
                RecordKey::Dynamic(key) => apart(key) && apart(&entry.value),
                RecordKey::Spread(key) => apart(key),
                RecordKey::Shorthand(key) => key.as_str() != name.as_str(),
            }
        }),
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => apart(condition) && apart(then_expr) && apart(else_expr),
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            statements
                .iter()
                .all(|statement| match &statement.node.node {
                    Expr::Assignment { ident, value } => *ident != name && apart(value),
                    _ => apart(&statement.node),
                })
                && apart(&return_expr.node)
        }
        Expr::Call { func, args } => {
            if let Some(inline) = inline_call(func, args) {
                return inline.params.iter().all(|param| param.get_name() != name)
                    && apart(inline.list)
                    && inline.init.is_none_or(apart)
                    && apart(inline.body);
            }

            match &func.node {
                Expr::BuiltIn(built_in) if !built_in.takes_callback() => args.iter().all(apart),
                _ => false,
            }
        }
        Expr::BinaryOp { op, left, right } => {
            !matches!(op, BinaryOp::Via | BinaryOp::Into | BinaryOp::Where)
                && apart(left)
                && apart(right)
        }
        Expr::DotAccess { expr: operand, .. }
        | Expr::UnaryOp { expr: operand, .. }
        | Expr::PostfixOp { expr: operand, .. }
        | Expr::Output { expr: operand }
        | Expr::Spread(operand) => apart(operand),
        Expr::Lambda { .. } | Expr::Assignment { .. } => false,
    }
}

/// Collect the names assigned by assignment expressions that execute directly in the scope
/// `expr` is evaluated in (do-blocks and lambdas open their own scopes and are skipped).
fn collect_assignment_exprs(expr: &SpannedExpr, out: &mut Vec<Symbol>) {
    match &expr.node {
        Expr::Assignment { ident, value } => {
            if !out.contains(ident) {
                out.push(*ident);
            }
            collect_assignment_exprs(value, out);
        }
        Expr::DoBlock { .. } | Expr::Lambda { .. } => {}
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::Identifier(_)
        | Expr::InputReference(_)
        | Expr::BuiltIn(_)
        | Expr::ParamSlot { .. }
        | Expr::CaptureSlot { .. } => {}
        Expr::List(items) => {
            for item in items {
                collect_assignment_exprs(&item.node, out);
            }
        }
        Expr::Record(entries) => {
            for entry in entries {
                let entry = &entry.node;

                match &entry.key {
                    RecordKey::Dynamic(key) | RecordKey::Spread(key) => {
                        collect_assignment_exprs(key, out);
                    }
                    RecordKey::Static(_) | RecordKey::Shorthand(_) => {}
                }

                if matches!(entry.key, RecordKey::Static(_) | RecordKey::Dynamic(_)) {
                    collect_assignment_exprs(&entry.value, out);
                }
            }
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_assignment_exprs(condition, out);
            collect_assignment_exprs(then_expr, out);
            collect_assignment_exprs(else_expr, out);
        }
        Expr::Output { expr } => collect_assignment_exprs(expr, out),
        Expr::Call { func, args } => {
            collect_assignment_exprs(func, out);
            for arg in args {
                collect_assignment_exprs(arg, out);
            }
        }
        Expr::Access { expr, index } => {
            collect_assignment_exprs(expr, out);
            collect_assignment_exprs(index, out);
        }
        Expr::DotAccess { expr, .. } => collect_assignment_exprs(expr, out),
        Expr::BinaryOp { left, right, .. } => {
            collect_assignment_exprs(left, out);
            collect_assignment_exprs(right, out);
        }
        Expr::UnaryOp { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Spread(expr) => {
            collect_assignment_exprs(expr, out);
        }
    }
}

/// The names assigned by assignment expressions directly in a do-block's scope: everything in
/// its statements except the statement-level assignments themselves.
fn collect_block_assignment_exprs(
    statements: &[Commented<SpannedExpr>],
    return_expr: &SpannedExpr,
    out: &mut Vec<Symbol>,
) {
    for statement in statements.iter().map(|s| &s.node).chain([return_expr]) {
        match &statement.node {
            Expr::Assignment { value, .. } => collect_assignment_exprs(value, out),
            _ => collect_assignment_exprs(statement, out),
        }
    }
}

/// Collect the names that `expr` may read without binding them, so each can be given a cache
/// register up front: those a chunk reads through its boundary environment, and those the
/// body of an inlined callback (`in_callback`) reads through the frame it is inlined into.
/// This over-approximates (a name that turns out to be a variable is told apart where it is
/// read), except that a name is left out where a statement of a block has bound it; lambdas
/// that are not inlined resolve their own names.
fn collect_late_names(expr: &SpannedExpr, in_callback: bool, out: &mut Vec<Symbol>) {
    let mut add = |name: Symbol| {
        if out.len() < MAX_CACHED_GLOBALS && !out.contains(&name) {
            out.push(name);
        }
    };

    match &expr.node {
        Expr::Identifier(name) => {
            if !is_constant_name(*name) {
                add(*name);
            }
        }
        Expr::CaptureSlot { name, .. } => {
            if in_callback {
                add(*name);
            }
        }
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::InputReference(_)
        | Expr::BuiltIn(_)
        | Expr::ParamSlot { .. }
        | Expr::Lambda { .. } => {}
        Expr::List(items) => {
            for item in items {
                collect_late_names(&item.node, in_callback, out);
            }
        }
        Expr::Record(entries) => {
            for entry in entries {
                let entry = &entry.node;

                match &entry.key {
                    RecordKey::Dynamic(key) | RecordKey::Spread(key) => {
                        collect_late_names(key, in_callback, out);
                    }
                    RecordKey::Static(_) | RecordKey::Shorthand(_) => {}
                }

                if matches!(entry.key, RecordKey::Static(_) | RecordKey::Dynamic(_)) {
                    collect_late_names(&entry.value, in_callback, out);
                }
            }
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_late_names(condition, in_callback, out);
            collect_late_names(then_expr, in_callback, out);
            collect_late_names(else_expr, in_callback, out);
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            // What a statement binds is a variable of the block for the ones that follow.
            let mut bound: Vec<Symbol> = Vec::new();
            let mut read = Vec::new();

            for statement in statements
                .iter()
                .map(|s| &s.node)
                .chain([&return_expr.node])
            {
                collect_late_names(statement, in_callback, &mut read);
                for name in read.drain(..) {
                    if !bound.contains(&name) {
                        add(name);
                    }
                }

                if let Expr::Assignment { ident, .. } = &statement.node {
                    bound.push(*ident);
                }
            }
        }
        Expr::Assignment { value, .. } => collect_late_names(value, in_callback, out),
        Expr::Output { expr } => collect_late_names(expr, in_callback, out),
        Expr::Call { func, args } => {
            if let Some(inline) = inline_call(func, args) {
                collect_late_names(inline.list, in_callback, out);
                collect_late_names(inline.body, true, out);
                if let Some(init) = inline.init {
                    collect_late_names(init, in_callback, out);
                }
                return;
            }

            collect_late_names(func, in_callback, out);
            for arg in args {
                collect_late_names(arg, in_callback, out);
            }
        }
        Expr::Access { expr, index } => {
            collect_late_names(expr, in_callback, out);
            collect_late_names(index, in_callback, out);
        }
        Expr::DotAccess { expr, .. } => collect_late_names(expr, in_callback, out),
        Expr::BinaryOp { op, left, right } => {
            if let Some(inline) = inline_operator(*op, left, right) {
                collect_late_names(inline.list, in_callback, out);
                collect_late_names(inline.body, true, out);
                return;
            }

            collect_late_names(left, in_callback, out);
            collect_late_names(right, in_callback, out);
        }
        Expr::UnaryOp { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Spread(expr) => {
            collect_late_names(expr, in_callback, out);
        }
    }
}

/// Identifiers that evaluate to a constant before any scope is consulted.
fn is_constant_name(name: Symbol) -> bool {
    matches!(name.as_str(), "infinity" | "inf" | "constants")
}

/// The name that a reference to a variable reads.
fn variable_name(expr: &SpannedExpr) -> Option<Symbol> {
    match &expr.node {
        Expr::Identifier(name) | Expr::ParamSlot { name, .. } | Expr::CaptureSlot { name, .. } => {
            Some(*name)
        }
        _ => None,
    }
}

/// Whether two expressions are spelled alike, as far as they are made of constants, names
/// and the operators that call no function. Where the names mean the same variables in
/// both, the two come to the same value.
fn same_shape(a: &SpannedExpr, b: &SpannedExpr) -> bool {
    match (&a.node, &b.node) {
        (Expr::Number(x), Expr::Number(y)) => x.to_bits() == y.to_bits(),
        (Expr::Bool(x), Expr::Bool(y)) => x == y,
        (Expr::Null, Expr::Null) => true,
        (Expr::String(x), Expr::String(y)) => x == y,
        (
            Expr::BinaryOp { op, left, right },
            Expr::BinaryOp {
                op: other_op,
                left: other_left,
                right: other_right,
            },
        ) => op == other_op && same_shape(left, other_left) && same_shape(right, other_right),
        (
            Expr::Access { expr, index },
            Expr::Access {
                expr: other,
                index: other_index,
            },
        ) => same_shape(expr, other) && same_shape(index, other_index),
        (
            Expr::DotAccess { expr, field },
            Expr::DotAccess {
                expr: other,
                field: other_field,
            },
        ) => field == other_field && same_shape(expr, other),
        (
            Expr::UnaryOp { op, expr },
            Expr::UnaryOp {
                op: other_op,
                expr: other,
            },
        ) => op == other_op && same_shape(expr, other),
        (
            Expr::PostfixOp { op, expr },
            Expr::PostfixOp {
                op: other_op,
                expr: other,
            },
        ) => op == other_op && same_shape(expr, other),
        _ => match (variable_name(a), variable_name(b)) {
            (Some(name), Some(other)) => name == other,
            _ => false,
        },
    }
}

/// Whether `expr` holds an expression that is spelled like `needle` (see [`same_shape`]),
/// looking at no more than `budget` of its nodes, and not into the expressions that are
/// spelled like one of `kept`: the value of such an expression is taken as a whole.
fn occurs(needle: &SpannedExpr, expr: &SpannedExpr, kept: &[Shared], budget: &mut usize) -> bool {
    if *budget == 0 {
        return false;
    }
    *budget -= 1;

    if kept.iter().any(|kept| same_shape(kept.expr(), expr)) {
        return false;
    }

    if same_shape(needle, expr) {
        return true;
    }

    match &expr.node {
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::Identifier(_)
        | Expr::InputReference(_)
        | Expr::BuiltIn(_)
        | Expr::ParamSlot { .. }
        | Expr::CaptureSlot { .. } => false,
        Expr::List(items) => items
            .iter()
            .any(|item| occurs(needle, &item.node, kept, budget)),
        Expr::Record(entries) => entries.iter().any(|entry| {
            let entry = &entry.node;

            match &entry.key {
                RecordKey::Dynamic(key) => {
                    occurs(needle, key, kept, budget) || occurs(needle, &entry.value, kept, budget)
                }
                RecordKey::Spread(key) => occurs(needle, key, kept, budget),
                RecordKey::Static(_) => occurs(needle, &entry.value, kept, budget),
                RecordKey::Shorthand(_) => false,
            }
        }),
        Expr::Lambda { body, .. } => occurs(needle, body, kept, budget),
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            occurs(needle, condition, kept, budget)
                || occurs(needle, then_expr, kept, budget)
                || occurs(needle, else_expr, kept, budget)
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            statements
                .iter()
                .any(|statement| occurs(needle, &statement.node, kept, budget))
                || occurs(needle, &return_expr.node, kept, budget)
        }
        Expr::Call { func, args } => {
            occurs(needle, func, kept, budget)
                || args.iter().any(|arg| occurs(needle, arg, kept, budget))
        }
        Expr::Access { expr, index } => {
            occurs(needle, expr, kept, budget) || occurs(needle, index, kept, budget)
        }
        Expr::BinaryOp { left, right, .. } => {
            occurs(needle, left, kept, budget) || occurs(needle, right, kept, budget)
        }
        Expr::Assignment { value: operand, .. }
        | Expr::Output { expr: operand }
        | Expr::DotAccess { expr: operand, .. }
        | Expr::UnaryOp { expr: operand, .. }
        | Expr::PostfixOp { expr: operand, .. }
        | Expr::Spread(operand) => occurs(needle, operand, kept, budget),
    }
}

/// What `expr` evaluates before it applies an operator that calls no function, when that is
/// what it does: the operands of the operator, in the order they are evaluated.
fn plain_operands(expr: &SpannedExpr) -> Option<[Option<&SpannedExpr>; 2]> {
    match &expr.node {
        Expr::BinaryOp { op, left, right }
            if !matches!(op, BinaryOp::Via | BinaryOp::Into | BinaryOp::Where) =>
        {
            Some([Some(left), Some(right)])
        }
        Expr::Access {
            expr: object,
            index,
        } => Some([Some(object), Some(index)]),
        Expr::DotAccess { expr: operand, .. }
        | Expr::UnaryOp { expr: operand, .. }
        | Expr::PostfixOp { expr: operand, .. } => Some([Some(operand), None]),
        _ => None,
    }
}

/// Whether evaluating `expr` can produce a function value (which an assignment then names).
fn may_be_lambda(expr: &SpannedExpr) -> bool {
    match &expr.node {
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::BuiltIn(_)
        | Expr::List(_)
        | Expr::Record(_)
        | Expr::UnaryOp { .. }
        | Expr::PostfixOp { .. }
        | Expr::Spread(_) => false,
        Expr::BinaryOp { op, .. } => matches!(
            op,
            BinaryOp::Via | BinaryOp::Into | BinaryOp::Where | BinaryOp::Coalesce
        ),
        Expr::Output { expr } => may_be_lambda(expr),
        _ => true,
    }
}

fn too_complex(what: &str) -> RuntimeError {
    RuntimeError::new(format!("expression is too complex to evaluate: {}", what))
}

/// Compile an expression evaluated directly against `env` (a top-level statement). The code
/// is only good for one run, against `env` as it is now.
///
/// A name that `env` binds keeps its value for as long as the chunk runs: nothing removes a
/// binding, and an assignment to a name that is visible anywhere fails. So the chunk reads
/// such names from registers that are loaded before it starts (see [`Proto::preloaded`]),
/// and only the names that are still unbound are looked up, where the program reads them.
/// Those can be bound more than once while the chunk runs, by an assignment whose value
/// binds the name of the assignment: each time, the value goes into the register as well.
pub(crate) fn compile_chunk(
    expr: &SpannedExpr,
    inline: bool,
    env: &Environment,
) -> CResult<Rc<Proto>> {
    let mut compiler = FnCompiler::new(true, Vec::new(), false, inline);

    let mut names = Vec::new();
    collect_late_names(expr, false, &mut names);
    for name in names {
        let reg = compiler.alloc()?;
        compiler.globals.push((name, reg));

        if env.get(name).is_some() {
            compiler.preloaded.push((reg, name));
            compiler.ensured.push(name);
        }
    }

    compiler.scopes.push(Scope {
        kind: ScopeKind::Root,
        vars: Vec::new(),
        param_regs: Vec::new(),
        locals: Vec::new(),
        base: compiler.free_reg,
    });
    compiler.expr_return(expr)?;

    Ok(Rc::new(compiler.finish(None, None)))
}

/// Compile a lambda body into a proto whose first registers are the lambda's parameters.
pub(crate) fn compile_lambda(
    args: &Rc<Vec<LambdaArg>>,
    body: &Rc<SpannedExpr>,
    inline: bool,
) -> CResult<Rc<Proto>> {
    compile_function(args, body, inline, None)
}

/// Compile the variant of a lambda body that is specialised for a function named `name` whose
/// capture slot `slot` is unbound: calls through that capture are calls of the function itself
/// and are compiled as direct self calls (see [`SelfRef`]).
pub(crate) fn compile_self_variant(
    args: &Rc<Vec<LambdaArg>>,
    body: &Rc<SpannedExpr>,
    inline: bool,
    name: Symbol,
    slot: u16,
) -> CResult<Rc<Proto>> {
    compile_function(args, body, inline, Some((name, slot)))
}

fn compile_function(
    args: &Rc<Vec<LambdaArg>>,
    body: &Rc<SpannedExpr>,
    inline: bool,
    self_target: Option<(Symbol, u16)>,
) -> CResult<Rc<Proto>> {
    let params: Vec<Symbol> = args.iter().map(LambdaArg::get_name).collect();
    let simple_params = args.iter().all(LambdaArg::is_required);
    let mut compiler = FnCompiler::new(false, params, simple_params, inline);
    compiler.self_target = self_target;

    for _ in 0..args.len() {
        compiler.alloc()?;
    }

    let mut scope = Scope {
        kind: ScopeKind::Root,
        vars: Vec::new(),
        param_regs: Vec::new(),
        locals: Vec::new(),
        base: compiler.free_reg,
    };

    // Assignment expressions evaluated directly in the call frame create frame-level variables
    // that may or may not exist at any given point.
    let mut assigned = Vec::new();
    collect_assignment_exprs(body, &mut assigned);
    compiler.declare_maybe_vars(&mut scope, &assigned)?;

    compiler.scopes.push(scope);
    compiler.expr_return(body)?;

    // Self calls address the start of the code with a 16-bit offset.
    if self_target.is_some() && compiler.code.len() > u16::MAX as usize {
        return Err(too_complex("the function is too long"));
    }

    Ok(Rc::new(
        compiler.finish(Some(Rc::clone(body)), Some(Rc::clone(args))),
    ))
}

struct FnCompiler {
    code: Vec<u64>,
    span_index: Vec<u32>,
    spans: Vec<Span>,
    consts: Vec<Value>,
    strings: Table<String>,
    names: Table<Symbol>,
    slot_refs: Vec<(u16, Symbol)>,
    self_refs: Vec<SelfRef>,
    lambdas: Vec<LambdaTemplate>,
    layouts: Vec<RecordLayout>,
    fields: Vec<String>,
    locals: Vec<LocalVar>,
    handlers: Vec<Handler>,

    scopes: Vec<Scope>,
    free_reg: u16,
    max_reg: u16,

    is_chunk: bool,
    params: Vec<Symbol>,

    /// Functions only: whether every parameter is required.
    simple_params: bool,

    /// The cache register of each late-bound name: of those that a chunk reads, for as long
    /// as it runs, and of those that the inlined loops around the current point of a
    /// function read, for as long as the loops run (see [`Self::inline_loop`]).
    globals: Vec<(Symbol, u16)>,

    /// Chunk only: the cache registers that are loaded before the chunk starts.
    preloaded: Vec<(u16, Symbol)>,

    /// The names whose cache register is known to be loaded at the current point. Those
    /// that are preloaded come first and stay for good.
    ensured: Vec<Symbol>,

    /// Whether a call that names a built-in function may be compiled into something other
    /// than a call of the function's value: an inline loop for the higher-order ones, an
    /// instruction that calls the function for the others. An operator that calls a function
    /// for the elements of a list is compiled into such a loop under the same condition.
    inline: bool,

    /// Variants only: the capture (name and slot) that is known to hold the function itself.
    self_target: Option<(Symbol, u16)>,

    /// The expressions of the inlined callbacks being compiled whose values were computed
    /// ahead of their loops, and the registers holding them (see [`Self::hoist`]).
    hoisted: Vec<(*const SpannedExpr, u16)>,

    /// The expressions of the conditions around the current point whose values are kept for
    /// their branches (see [`Self::share`]).
    shared: Vec<Shared>,

    /// How many more nodes are searched for expressions to share.
    shared_search: usize,

    /// The first state register of each inlined loop around the current point that goes
    /// through `entries` of a record without making the list of a key and a value that its
    /// callback is given for an entry (see [`takes_entries_apart`]). The element register of
    /// such a loop holds the value alone.
    apart: Vec<u16>,
}

/// An expression that a condition evaluates and that one of its branches evaluates again:
/// the first value is kept in a register for as long as the branches last.
struct Shared {
    expr: *const SpannedExpr,
    reg: u16,

    /// The registers of the variables that the expression reads, in the order it names them.
    /// A name can mean another variable further into a branch, so these are what tells
    /// whether an expression there that is spelled the same comes to the same value.
    variables: Vec<u16>,

    /// Whether the code up to the current point has computed the value.
    computed: bool,
}

impl Shared {
    fn expr(&self) -> &SpannedExpr {
        // SAFETY: the expression is part of the body that the compiler was given to compile,
        // which outlives the compiler.
        unsafe { &*self.expr }
    }
}

impl FnCompiler {
    fn new(is_chunk: bool, params: Vec<Symbol>, simple_params: bool, inline: bool) -> Self {
        Self {
            code: Vec::new(),
            span_index: Vec::new(),
            spans: Vec::new(),
            consts: Vec::new(),
            strings: Table::new(),
            names: Table::new(),
            slot_refs: Vec::new(),
            self_refs: Vec::new(),
            lambdas: Vec::new(),
            layouts: Vec::new(),
            fields: Vec::new(),
            locals: Vec::new(),
            handlers: Vec::new(),
            scopes: Vec::new(),
            free_reg: 0,
            max_reg: 0,
            is_chunk,
            params,
            simple_params,
            globals: Vec::new(),
            preloaded: Vec::new(),
            ensured: Vec::new(),
            inline,
            self_target: None,
            hoisted: Vec::new(),
            shared: Vec::new(),
            shared_search: SHARED_SEARCH_NODES,
            apart: Vec::new(),
        }
    }

    fn finish(mut self, body: Option<Rc<SpannedExpr>>, args: Option<Rc<Vec<LambdaArg>>>) -> Proto {
        let end = self.code.len() as u32;
        for scope in &self.scopes {
            for &local in &scope.locals {
                self.locals[local].end = end;
            }
        }

        let simple_params = args
            .as_ref()
            .is_none_or(|args| args.iter().all(LambdaArg::is_required));
        let simple_arity = match &args {
            Some(args) if simple_params => u32::try_from(args.len()).unwrap_or(u32::MAX),
            _ => u32::MAX,
        };
        let fast_arity = if self.max_reg as usize <= FAST_FRAME_SLOTS {
            simple_arity
        } else {
            u32::MAX
        };

        Proto {
            code: self.code,
            consts: self.consts,
            literals: self
                .strings
                .entries
                .iter()
                .map(|_| Literal::new())
                .collect(),
            strings: self.strings.entries,
            names: self.names.entries,
            slot_refs: self.slot_refs,
            self_refs: self.self_refs,
            lambdas: self.lambdas,
            layouts: self.layouts,
            field_caches: self.fields.iter().map(|_| FieldCache::new()).collect(),
            fields: self.fields,
            span_index: self.span_index,
            spans: self.spans,
            locals: self.locals,
            handlers: self.handlers,
            params: self.params,
            simple_params,
            simple_arity,
            fast_arity,
            cache_regs: match self.is_chunk {
                true => self.globals.len(),
                false => 0,
            },
            preloaded: self.preloaded,
            max_stack: self.max_reg as usize,
            body,
            args,
        }
    }

    // ---------------------------------------------------------------------------------------
    // Emission helpers
    // ---------------------------------------------------------------------------------------

    fn emit(&mut self, instr: Instr) -> usize {
        self.code.push(instr.encode());
        self.span_index.push(u32::MAX);
        self.code.len() - 1
    }

    /// Emit the number operand of the instruction that was just emitted.
    fn emit_number(&mut self, number: f64) {
        self.emit_word(number.to_bits());
    }

    /// Emit an operand word of the instruction that was just emitted.
    fn emit_word(&mut self, word: u64) {
        self.code.push(word);
        self.span_index.push(u32::MAX);
    }

    fn emit_spanned(&mut self, instr: Instr, span: Span) -> usize {
        let index = self.emit(instr);
        self.set_span(index, span);

        index
    }

    /// Emit an operand word of the instruction that was just emitted. An error that the
    /// instruction raises once it has got as far as this word is located at `span`.
    fn emit_word_spanned(&mut self, word: u64, span: Span) {
        self.emit_word(word);
        self.set_span(self.code.len() - 1, span);
    }

    /// Locate the errors raised at code word `index` at `span`.
    fn set_span(&mut self, index: usize, span: Span) {
        // Consecutive instructions often share a span (e.g. the checks of one assignment).
        let span_id = match self.spans.last() {
            Some(last) if *last == span => self.spans.len() - 1,
            _ => {
                self.spans.push(span);
                self.spans.len() - 1
            }
        };
        self.span_index[index] = span_id as u32;
    }

    fn here(&self) -> usize {
        self.code.len()
    }

    /// Emit a jump whose target is patched later with [`Self::patch_jump`].
    fn emit_jump(&mut self, op: Op, a: u16) -> usize {
        self.emit(Instr::abx(op, a, 0))
    }

    /// Point the jump at code word `jump` to `target`. Offsets are relative to the word that
    /// follows the jump instruction.
    fn patch_jump(&mut self, jump: usize, target: usize) {
        let offset = target as i64 - (jump as i64 + 1);
        let bits = offset as i32 as u32;

        // SAFETY: `jump` is the index of an instruction word returned by an emit method.
        let mut instr = unsafe { Instr::decode(self.code[jump]) };
        instr.b = bits as u16;
        instr.c = (bits >> 16) as u16;
        self.code[jump] = instr.encode();
    }

    fn alloc(&mut self) -> CResult<u16> {
        let reg = self.free_reg;
        if reg as usize >= MAX_REGISTERS {
            return Err(too_complex("too many values are live at once"));
        }

        self.free_reg += 1;
        self.max_reg = self.max_reg.max(self.free_reg);

        Ok(reg)
    }

    fn constant(&mut self, value: Value) -> u32 {
        let same = |a: &Value, b: &Value| match (a, b) {
            (Value::Number(x), Value::Number(y)) => x.to_bits() == y.to_bits(),
            _ => a == b,
        };

        if let Some(index) = self
            .consts
            .iter()
            .position(|existing| same(existing, &value))
        {
            return index as u32;
        }

        self.consts.push(value);
        (self.consts.len() - 1) as u32
    }

    fn string(&mut self, text: &str) -> u32 {
        self.strings.index_of(text)
    }

    fn name(&mut self, name: Symbol) -> u32 {
        self.names.index_of(&name)
    }

    /// The table index of a parameter or capture slot reference.
    fn slot_ref(&mut self, index: u16, name: Symbol) -> u32 {
        let slot = (index, name);
        if let Some(position) = self.slot_refs.iter().position(|existing| *existing == slot) {
            return position as u32;
        }

        self.slot_refs.push(slot);
        (self.slot_refs.len() - 1) as u32
    }

    fn load_const(&mut self, dst: u16, value: Value) {
        if let Value::Number(number) = value {
            self.emit(Instr::abc(Op::LoadNum, dst, 0, 0));
            self.emit_number(number);

            return;
        }

        let index = self.constant(value);
        self.emit(Instr::abx(Op::LoadK, dst, index));
    }

    fn raise(&mut self, message: String, span: Span) {
        let index = self.string(&message);
        self.emit_spanned(Instr::abx(Op::Raise, 0, index), span);
    }

    // ---------------------------------------------------------------------------------------
    // Scopes and name resolution
    // ---------------------------------------------------------------------------------------

    fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope {
            kind,
            vars: Vec::new(),
            param_regs: Vec::new(),
            locals: Vec::new(),
            base: self.free_reg,
        });
    }

    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().expect("scope stack underflow");
        let end = self.here() as u32;

        for local in scope.locals {
            self.locals[local].end = end;
        }

        self.free_reg = scope.base;
    }

    /// Declare a variable of the innermost scope that is visible from the next instruction on.
    fn declare(&mut self, name: Symbol, reg: u16, maybe: bool) {
        let start = self.here() as u32;
        self.locals.push(LocalVar {
            name,
            reg,
            start,
            end: u32::MAX,
        });

        let local = self.locals.len() - 1;
        let scope = self.scopes.last_mut().expect("no scope to declare into");
        scope.vars.push(ScopeVar { name, reg, maybe });
        scope.locals.push(local);
    }

    /// Give each name in `names` an (initially unbound) register in `scope`, which is about to
    /// be pushed.
    fn declare_maybe_vars(&mut self, scope: &mut Scope, names: &[Symbol]) -> CResult<()> {
        let mut regs = Vec::with_capacity(names.len());
        for _ in names {
            let reg = self.alloc()?;
            self.emit(Instr::abc(Op::LoadUnbound, reg, 0, 0));
            regs.push(reg);
        }

        let start = self.here() as u32;
        for (name, reg) in names.iter().zip(regs) {
            self.locals.push(LocalVar {
                name: *name,
                reg,
                start,
                end: u32::MAX,
            });
            scope.vars.push(ScopeVar {
                name: *name,
                reg,
                maybe: true,
            });
            scope.locals.push(self.locals.len() - 1);
        }

        Ok(())
    }

    fn resolve(&self, name: Symbol) -> Resolved {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.vars.iter().rev().find(|var| var.name == name) {
                if var.maybe {
                    return Resolved::Dynamic;
                }

                return Resolved::Register(var.reg);
            }

            if scope.kind != ScopeKind::Root {
                continue;
            }

            if !self.is_chunk
                && let Some(index) = self.params.iter().position(|param| *param == name)
            {
                return Resolved::Register(index as u16);
            }

            return match self.globals.iter().find(|(global, _)| *global == name) {
                Some((_, reg)) => Resolved::Global(name, *reg),
                None => Resolved::Dynamic,
            };
        }

        Resolved::Dynamic
    }

    /// Whether a "maybe" variable with this name is declared in any enclosing scope.
    fn has_maybe_var(&self, name: Symbol) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.vars.iter().any(|var| var.maybe && var.name == name))
    }

    /// Whether something that precedes the function's self-binding in lookup order can be
    /// bound to `name`. The self-binding shortcut of capture references is only sound when
    /// nothing can.
    fn shadows_self_binding(&self, name: Symbol) -> bool {
        self.params.contains(&name) || self.has_maybe_var(name)
    }

    /// Whether any variable of the function is visible to by-name lookups at this point.
    fn has_visible_vars(&self) -> bool {
        self.scopes.iter().any(|scope| !scope.vars.is_empty())
    }

    /// The number literal `expr` is, if it is one.
    fn number_operand(expr: &SpannedExpr) -> Option<f64> {
        match &expr.node {
            Expr::Number(number) => Some(*number),
            _ => None,
        }
    }

    /// When calling `func` with `args` would be a self call if `func` turned out to be the
    /// enclosing function, the capture (name and slot) the call goes through: `func` must be a
    /// capture reference that nothing can shadow, and the arguments must be exactly what the
    /// enclosing function itself takes.
    fn self_call_candidate(
        &self,
        func: &SpannedExpr,
        args: &[SpannedExpr],
    ) -> Option<(Symbol, u16)> {
        let Expr::CaptureSlot { name, index } = &func.node else {
            return None;
        };

        if !matches!(self.owner(), Owner::Function)
            || !self.simple_params
            || args.len() != self.params.len()
            || args.len() > MAX_SELF_CALL_ARGS
            || args.iter().any(|arg| matches!(arg.node, Expr::Spread(_)))
            || self.shadows_self_binding(*name)
        {
            return None;
        }

        Some((*name, *index))
    }

    /// Record that the function may be calling itself through capture `slot`, named `name`.
    fn note_self_ref(&mut self, name: Symbol, slot: u16) {
        if self.self_refs.iter().any(|existing| existing.slot == slot) {
            return;
        }

        self.self_refs.push(SelfRef {
            name,
            slot,
            variant: OnceCell::new(),
        });
    }

    /// The lambda whose parameter and capture slots a slot reference at this point refers to.
    fn owner(&self) -> Owner {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            match scope.kind {
                ScopeKind::Callback => return Owner::Callback(index),
                ScopeKind::Root => break,
                ScopeKind::Block => {}
            }
        }

        if self.is_chunk {
            Owner::Chunk
        } else {
            Owner::Function
        }
    }

    /// Make sure the cache register of a late-bound name is loaded, raising the usual
    /// "unknown identifier" error at this point of the program otherwise.
    fn ensure_global(&mut self, name: Symbol, reg: u16, span: Span) {
        if self.ensured.contains(&name) {
            return;
        }

        let index = self.name(name);
        let op = match self.is_chunk {
            true => Op::EnsureGlobal,
            false => Op::EnsureName,
        };
        self.emit_spanned(Instr::abx(op, reg, index), span);
        self.ensured.push(name);
    }

    /// Where a variable reference finds its value, when that is known at this point of the
    /// program. `Dynamic` stands for a reference that has to be looked up when the code runs,
    /// and for anything that is not a reference to a variable.
    fn place(&self, expr: &SpannedExpr) -> Resolved {
        let name = match &expr.node {
            Expr::Identifier(name) if !is_constant_name(*name) => *name,
            Expr::ParamSlot { name, index } => match self.owner() {
                Owner::Callback(scope) => {
                    match self.scopes[scope].param_regs.get(*index as usize) {
                        Some(reg) => return Resolved::Register(*reg),
                        None => *name,
                    }
                }
                Owner::Function if (*index as usize) < self.params.len() => {
                    return Resolved::Register(*index);
                }
                Owner::Function => *name,
                Owner::Chunk => return Resolved::Dynamic,
            },
            // An inlined callback never materialises its captured scope: a capture bound at
            // closure creation holds exactly the value a by-name lookup from the callback
            // body finds, and an unbound one falls back to that same lookup.
            Expr::CaptureSlot { name, .. } if matches!(self.owner(), Owner::Callback(_)) => *name,
            _ => return Resolved::Dynamic,
        };

        self.resolve(name)
    }

    /// If `expr` is a variable reference whose value already sits in a register, return that
    /// register (emitting at most a cache check).
    fn direct_reg(&mut self, expr: &SpannedExpr) -> Option<u16> {
        match self.place(expr) {
            Resolved::Register(reg) => Some(reg),
            Resolved::Global(name, reg) => {
                self.ensure_global(name, reg, expr.span);
                Some(reg)
            }
            Resolved::Dynamic => None,
        }
    }

    /// Compile a variable reference into `dst`.
    fn name_into(&mut self, expr: &SpannedExpr, dst: u16) -> CResult<()> {
        if let Some(reg) = self.direct_reg(expr) {
            if reg != dst {
                self.emit(Instr::abc(Op::Move, dst, reg, 0));
            }

            return Ok(());
        }

        match &expr.node {
            Expr::Identifier(name) => match name.as_str() {
                "infinity" | "inf" => self.load_const(dst, Value::Number(f64::INFINITY)),
                "constants" => self.load_const(dst, Value::Record(RecordPointer::new(0))),
                _ => self.get_name(*name, dst, expr.span),
            },
            Expr::ParamSlot { name, index } => match self.owner() {
                Owner::Chunk => {
                    let slot = self.slot_ref(*index, *name);
                    self.emit_spanned(Instr::abx(Op::GetEnvParam, dst, slot), expr.span);
                }
                Owner::Function | Owner::Callback(_) => self.get_name(*name, dst, expr.span),
            },
            Expr::CaptureSlot { name, index } => match self.owner() {
                Owner::Chunk => {
                    let slot = self.slot_ref(*index, *name);
                    self.emit_spanned(Instr::abx(Op::GetEnvCapture, dst, slot), expr.span);
                }
                Owner::Function => {
                    let shadowable = self.shadows_self_binding(*name);
                    let reference = self.slot_ref(*index, *name);
                    let mut instr = match u16::try_from(reference) {
                        Ok(reference) => Instr::abc(Op::GetCapture, dst, *index, reference),
                        Err(_) => Instr::abx(Op::GetCaptureRef, dst, reference),
                    };
                    instr.x = shadowable as u8;
                    self.emit_spanned(instr, expr.span);
                }
                Owner::Callback(_) => self.get_name(*name, dst, expr.span),
            },
            _ => unreachable!("name_into called on a non-name expression"),
        }

        Ok(())
    }

    fn get_name(&mut self, name: Symbol, dst: u16, span: Span) {
        let index = self.name(name);
        self.emit_spanned(Instr::abx(Op::GetName, dst, index), span);
    }

    // ---------------------------------------------------------------------------------------
    // Expressions
    // ---------------------------------------------------------------------------------------

    /// Compile `expr` and return a register holding its value: the variable's own register for
    /// plain variable references, a fresh temporary otherwise. The caller releases temporaries
    /// by restoring `free_reg`.
    fn expr_any(&mut self, expr: &SpannedExpr) -> CResult<u16> {
        if let Some(reg) = self.direct_reg(expr) {
            return Ok(reg);
        }

        if let Some(reg) = self.hoisted_reg(expr) {
            return Ok(reg);
        }

        if !self.shared.is_empty() {
            // The value is computed into the register that keeps it for the branches, or
            // was already.
            if let Some(index) = self.awaited_shared(expr) {
                let reg = self.shared[index].reg;
                self.expr_into(expr, reg)?;
                self.shared[index].computed = true;

                return Ok(reg);
            }

            if let Some(reg) = self.shared_reg(expr) {
                return Ok(reg);
            }
        }

        if let Some((state, EntryPart::Value)) = self.entry_access(expr) {
            return Ok(state + 2);
        }

        let reg = self.alloc()?;
        self.expr_into(expr, reg)?;

        Ok(reg)
    }

    /// Compile `expr` in tail position of the function or chunk.
    fn expr_return(&mut self, expr: &SpannedExpr) -> CResult<()> {
        match &expr.node {
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => self.conditional(condition, then_expr, else_expr, Target::Return),
            Expr::DoBlock {
                statements,
                return_expr,
            } => self.do_block(statements, &return_expr.node, Target::Return),
            Expr::Output { expr } => self.expr_return(expr),
            Expr::Call { func, args } => {
                let mark = self.free_reg;
                let reg = self.alloc()?;
                self.call(expr.span, func, args, reg, true)?;
                self.emit(Instr::abc(Op::Return, reg, 0, 0));
                self.free_reg = mark;

                Ok(())
            }
            _ => {
                let mark = self.free_reg;
                let reg = self.expr_any(expr)?;
                self.emit(Instr::abc(Op::Return, reg, 0, 0));
                self.free_reg = mark;

                Ok(())
            }
        }
    }

    /// Compile `expr` so that its value ends up in `dst`. `dst` is only ever written by the
    /// final instruction of the expression, so it may alias a variable the expression reads.
    fn expr_into(&mut self, expr: &SpannedExpr, dst: u16) -> CResult<()> {
        if let Some(reg) = self.hoisted_reg(expr) {
            self.emit(Instr::abc(Op::Move, dst, reg, 0));

            return Ok(());
        }

        if !self.shared.is_empty()
            && let Some(reg) = self.shared_reg(expr)
        {
            self.emit(Instr::abc(Op::Move, dst, reg, 0));

            return Ok(());
        }

        if let Some((state, part)) = self.entry_access(expr) {
            if part == EntryPart::Value {
                self.emit(Instr::abc(Op::Move, dst, state + 2, 0));
            } else {
                let mut load = Instr::abc(Op::LoadEntry, dst, state, 0);
                load.x = part as u8;
                self.emit(load);
            }

            return Ok(());
        }

        match &expr.node {
            Expr::Number(n) => self.load_const(dst, Value::Number(*n)),
            Expr::Bool(b) => self.load_const(dst, Value::Bool(*b)),
            Expr::Null => self.load_const(dst, Value::Null),
            Expr::BuiltIn(built_in) => self.load_const(dst, Value::BuiltIn(*built_in)),
            Expr::String(text) => {
                let index = self.string(text);
                self.emit(Instr::abx(Op::LoadStr, dst, index));
            }
            Expr::Identifier(_) | Expr::ParamSlot { .. } | Expr::CaptureSlot { .. } => {
                self.name_into(expr, dst)?;
            }
            Expr::InputReference(field) => {
                let index = self.string(field);
                self.emit_spanned(Instr::abx(Op::GetInput, dst, index), expr.span);
            }
            Expr::List(items) => self.list(items, dst)?,
            Expr::Record(entries) => self.record(entries, dst)?,
            Expr::Lambda {
                args,
                body,
                captures,
            } => self.closure(args, body, captures, dst)?,
            Expr::Assignment { ident, value } => self.assignment(expr.span, *ident, value, dst)?,
            Expr::Output { expr } => self.expr_into(expr, dst)?,
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => self.conditional(condition, then_expr, else_expr, Target::Register(dst))?,
            Expr::DoBlock {
                statements,
                return_expr,
            } => self.do_block(statements, &return_expr.node, Target::Register(dst))?,
            Expr::Call { func, args } => self.call(expr.span, func, args, dst, false)?,
            Expr::Access {
                expr: object,
                index,
            } => {
                let mark = self.free_reg;
                let object_reg = self.expr_any(object)?;
                let index_reg = self.expr_any(index)?;
                self.emit_spanned(
                    Instr::abc(Op::Index, dst, object_reg, index_reg),
                    object.span,
                );
                self.free_reg = mark;
            }
            Expr::DotAccess {
                expr: object,
                field,
            } => {
                let mark = self.free_reg;
                let object_reg = self.expr_any(object)?;
                let site = self.fields.len();
                if site >= 1 << 24 {
                    return Err(too_complex("too many field accesses"));
                }
                self.fields.push(field.clone());

                let mut instr = Instr::abc(Op::GetField, dst, object_reg, site as u16);
                instr.x = (site >> 16) as u8;
                self.emit_spanned(instr, object.span);
                self.free_reg = mark;
            }
            Expr::BinaryOp { op, left, right } => self.binary(*op, left, right, dst)?,
            Expr::UnaryOp { op, expr: operand } => {
                let mark = self.free_reg;
                let operand_reg = self.expr_any(operand)?;
                let op = match op {
                    UnaryOp::Negate => Op::Neg,
                    UnaryOp::Not | UnaryOp::Invert => Op::Not,
                };
                self.emit(Instr::abc(op, dst, operand_reg, 0));
                self.free_reg = mark;
            }
            Expr::PostfixOp {
                op: PostfixOp::Factorial,
                expr: operand,
            } => {
                let mark = self.free_reg;
                let operand_reg = self.expr_any(operand)?;
                self.emit_spanned(Instr::abc(Op::Factorial, dst, operand_reg, 0), operand.span);
                self.free_reg = mark;
            }
            Expr::Spread(inner) => {
                let mark = self.free_reg;
                let inner_reg = self.expr_any(inner)?;
                self.emit_spanned(Instr::abc(Op::Spread, dst, inner_reg, 0), inner.span);
                self.free_reg = mark;
            }
        }

        Ok(())
    }

    fn binary(
        &mut self,
        op: BinaryOp,
        left: &SpannedExpr,
        right: &SpannedExpr,
        dst: u16,
    ) -> CResult<()> {
        let span = binary_span(left, right);
        let mark = self.free_reg;

        if self.inline
            && let Some(inline) = inline_operator(op, left, right)
        {
            return self.inline_loop(span, &inline, dst);
        }

        if let Some(null_op) = null_compare_opcode(op)
            && let Some((operand, null_first)) = null_operand(left, right)
        {
            let operand_reg = self.expr_any(operand)?;
            let mut instr = Instr::abc(null_op, dst, operand_reg, 0);
            instr.x = null_first as u8;
            self.emit_spanned(instr, span);
        } else if let Some(const_op) = const_rhs_opcode(op)
            && let Some(constant) = Self::number_operand(right)
        {
            let left_reg = self.expr_any(left)?;
            let flags = last_use_flags(op, left_reg >= mark, false);

            // Halving, and dividing by any other power of two, is a multiplication.
            let reciprocal = match op {
                BinaryOp::Divide => exact_reciprocal(constant),
                _ => None,
            };
            let (const_op, constant, flags) = match reciprocal {
                Some(reciprocal) => (Op::MulK, reciprocal, flags | RECIPROCAL),
                None => (const_op, constant, flags),
            };

            let divisor = match const_op {
                Op::ModK => small_divisor(constant),
                _ => 0,
            };

            let mut instr = Instr::abc(const_op, dst, left_reg, divisor);
            instr.x = flags;
            self.emit_spanned(instr, span);
            self.emit_number(constant);
        } else if let Some(const_op) = const_lhs_opcode(op)
            && let Some(constant) = Self::number_operand(left)
        {
            let right_reg = self.expr_any(right)?;
            let mut instr = Instr::abc(const_op, dst, right_reg, 0);
            instr.x = last_use_flags(op, right_reg >= mark, false);
            self.emit_spanned(instr, span);
            self.emit_number(constant);
        } else {
            let left_reg = self.expr_any(left)?;
            let right_reg = match self.repeats(left, right) {
                true => left_reg,
                false => self.expr_any(right)?,
            };
            let mut instr = Instr::abc(binary_opcode(op), dst, left_reg, right_reg);
            instr.x = last_use_flags(op, left_reg >= mark, right_reg >= mark);
            self.emit_spanned(instr, span);
        }

        self.free_reg = mark;

        Ok(())
    }

    /// Whether `right`, which is evaluated right after `left` and with the same variables in
    /// sight, is bound to come to the value that `left` just came to: the two are the same
    /// expression, and one that does nothing but come to a value (see [`Self::is_plain`]).
    fn repeats(&self, left: &SpannedExpr, right: &SpannedExpr) -> bool {
        if plain_operands(left).is_none()
            || !same_shape(left, right)
            || !self.is_plain(left, u16::MAX, &mut MAX_INVARIANT_NODES.clone())
        {
            return false;
        }

        // The two name the same variables wherever a name means in one what it means in the
        // other.
        let mut variables = Vec::new();
        self.variable_regs(left, &mut variables);

        self.same_value(left, &mut variables.iter(), right)
    }

    /// Compile a condition and emit a jump (returned for patching) that is taken when the
    /// condition is false.
    fn branch_if_false(&mut self, condition: &SpannedExpr) -> CResult<usize> {
        let mark = self.free_reg;

        if let Expr::BinaryOp { op, left, right } = &condition.node
            && let Some((jump_op, jump_const_op)) = compare_jump_opcodes(*op)
        {
            let span = binary_span(left, right);

            if let Some(null_op) = null_jump_opcode(*op)
                && let Some((operand, null_first)) = null_operand(left, right)
            {
                let operand_reg = self.expr_any(operand)?;
                let mut instr = Instr::abx(null_op, operand_reg, 0);
                instr.x = null_first as u8;
                self.free_reg = mark;

                return Ok(self.emit_spanned(instr, span));
            }

            if let Some(constant) = Self::number_operand(right) {
                // The remainder by a power of two is tested without being computed. The
                // instruction is located at the division, and its first operand word at the
                // comparison, which is where each of the two can fail.
                if let Some(remainder_op) = remainder_jump_opcode(*op)
                    && self.hoisted_reg(left).is_none()
                    && let Expr::BinaryOp {
                        op: BinaryOp::Modulo,
                        left: dividend,
                        right: divisor,
                    } = &left.node
                    && let Some(test) = Self::number_operand(divisor)
                        .and_then(|divisor| RemainderTest::new(divisor, constant))
                {
                    let dividend_reg = self.expr_any(dividend)?;
                    let jump = self.emit_spanned(
                        Instr::abx(remainder_op, dividend_reg, 0),
                        binary_span(dividend, divisor),
                    );
                    self.emit_word_spanned(test.mask, span);
                    self.emit_word(test.bits);
                    self.free_reg = mark;

                    return Ok(jump);
                }

                let left_reg = self.expr_any(left)?;
                let jump = self.emit_spanned(Instr::abx(jump_const_op, left_reg, 0), span);
                self.emit_number(constant);
                self.free_reg = mark;

                return Ok(jump);
            }

            let left_reg = self.expr_any(left)?;
            let right_reg = self.expr_any(right)?;
            self.emit_spanned(Instr::abc(jump_op, 0, left_reg, right_reg), span);
            self.free_reg = mark;

            // The comparison skips this jump when it holds and takes it otherwise.
            return Ok(self.emit_jump(Op::Jmp, 0));
        }

        let reg = self.expr_any(condition)?;
        self.free_reg = mark;

        Ok(self.emit_jump(Op::JmpIfFalse, reg))
    }

    fn conditional(
        &mut self,
        condition: &SpannedExpr,
        then_expr: &SpannedExpr,
        else_expr: &SpannedExpr,
        target: Target,
    ) -> CResult<()> {
        let mark = self.free_reg;
        let shared = self.shared.len();
        self.share(condition, [then_expr, else_expr])?;

        let to_else = self.branch_if_false(condition)?;

        // Cache checks emitted inside a branch do not dominate the code after it.
        let ensured = self.ensured.len();

        match target {
            Target::Register(dst) => {
                self.expr_into(then_expr, dst)?;
                let to_end = self.emit_jump(Op::Jmp, 0);
                self.ensured.truncate(ensured);

                self.patch_jump(to_else, self.here());
                self.expr_into(else_expr, dst)?;
                self.ensured.truncate(ensured);

                self.patch_jump(to_end, self.here());
            }
            Target::Return => {
                self.expr_return(then_expr)?;
                self.ensured.truncate(ensured);

                self.patch_jump(to_else, self.here());
                self.expr_return(else_expr)?;
                self.ensured.truncate(ensured);
            }
        }

        self.shared.truncate(shared);
        self.free_reg = mark;

        Ok(())
    }

    /// Keep what `condition` computes on the way to its value for its `branches`, where one
    /// of them computes the same again.
    ///
    /// The expressions in question read variables and apply operators that call no
    /// function, so all they do is come to a value or fail, and they come to the same
    /// value for as long as the variables hold theirs. Those of a condition have not
    /// failed by the time a branch runs, and no variable is assigned in between: a branch
    /// can only bind variables of its own. So a branch that takes the value the condition
    /// came to cannot be told from one that computes it.
    ///
    /// Each such expression is given a register here, which the condition computes it into
    /// (see [`Self::expr_any`]) and which stays put until the branches end.
    fn share(&mut self, condition: &SpannedExpr, branches: [&SpannedExpr; 2]) -> CResult<()> {
        if self.free_reg as usize >= MAX_SHARING_FRAME {
            return Ok(());
        }

        let first = self.shared.len();

        match &condition.node {
            // A comparison that decides a branch is compiled together with the branch: it
            // has no value to keep, but its operands do.
            Expr::BinaryOp { op, left, right } if compare_jump_opcodes(*op).is_some() => {
                self.share_from(left, branches, first)?;
                self.share_from(right, branches, first)
            }
            _ => self.share_from(condition, branches, first),
        }
    }

    /// [`Self::share`] for `expr` and everything it evaluates on the way to its value, as
    /// part of a condition whose kept values start at `first`.
    fn share_from(
        &mut self,
        expr: &SpannedExpr,
        branches: [&SpannedExpr; 2],
        first: usize,
    ) -> CResult<()> {
        let Some(operands) = plain_operands(expr) else {
            return Ok(());
        };

        if self.shared.len() - first == MAX_SHARED || self.shared.len() == MAX_SHARED_AT_ONCE {
            return Ok(());
        }

        if self.is_plain(expr, u16::MAX, &mut MAX_INVARIANT_NODES.clone()) {
            // The value may be on hand already, or about to be.
            let awaited = self.shared[first..]
                .iter()
                .any(|shared| !shared.computed && same_shape(shared.expr(), expr));
            if awaited || self.shared_reg(expr).is_some() {
                return Ok(());
            }

            // A branch that only repeats the expression as part of one that is kept already
            // has no use for it.
            let mut search = self.shared_search;
            let kept = &self.shared[first..];
            let repeated = branches
                .iter()
                .any(|branch| occurs(expr, branch, kept, &mut search));
            self.shared_search = search;

            if repeated {
                let mut variables = Vec::new();
                self.variable_regs(expr, &mut variables);

                // A value that was computed ahead of a loop is where it will stay.
                let (reg, computed) = match self.hoisted_reg(expr) {
                    Some(reg) => (reg, true),
                    None => (self.alloc()?, false),
                };
                self.shared.push(Shared {
                    expr,
                    reg,
                    variables,
                    computed,
                });
            }
        }

        for operand in operands.into_iter().flatten() {
            self.share_from(operand, branches, first)?;
        }

        Ok(())
    }

    /// The registers of the variables that `expr` reads, in the order it names them. `expr`
    /// is an expression that [`Self::is_plain`] holds for.
    fn variable_regs(&self, expr: &SpannedExpr, out: &mut Vec<u16>) {
        if let Some(operands) = plain_operands(expr) {
            for operand in operands.into_iter().flatten() {
                self.variable_regs(operand, out);
            }
        } else if let Resolved::Register(reg) | Resolved::Global(_, reg) = self.place(expr) {
            out.push(reg);
        }
    }

    /// The kept value that `expr` itself is to be computed into, as a position in `shared`.
    fn awaited_shared(&self, expr: &SpannedExpr) -> Option<usize> {
        self.shared
            .iter()
            .position(|shared| !shared.computed && std::ptr::eq(shared.expr, expr))
    }

    /// The register that holds the value of `expr` already, when a condition around this
    /// point has computed the same expression over the same variables (see [`Self::share`]).
    fn shared_reg(&self, expr: &SpannedExpr) -> Option<u16> {
        plain_operands(expr)?;

        self.shared
            .iter()
            .find(|shared| {
                shared.computed
                    && self.same_value(shared.expr(), &mut shared.variables.iter(), expr)
            })
            .map(|shared| shared.reg)
    }

    /// Whether `expr` comes to the value that the kept expression `kept` came to: the two
    /// are spelled alike, and the variables `expr` reads at this point are those that
    /// `kept` read where it was evaluated, whose registers `variables` yields in the order
    /// `kept` names them.
    fn same_value(
        &self,
        kept: &SpannedExpr,
        variables: &mut std::slice::Iter<u16>,
        expr: &SpannedExpr,
    ) -> bool {
        match (&kept.node, &expr.node) {
            (Expr::Number(x), Expr::Number(y)) => x.to_bits() == y.to_bits(),
            (Expr::Bool(x), Expr::Bool(y)) => x == y,
            (Expr::Null, Expr::Null) => true,
            (Expr::String(x), Expr::String(y)) => x == y,
            (
                Expr::BinaryOp { op, left, right },
                Expr::BinaryOp {
                    op: other_op,
                    left: other_left,
                    right: other_right,
                },
            ) => {
                op == other_op
                    && self.same_value(left, variables, other_left)
                    && self.same_value(right, variables, other_right)
            }
            (
                Expr::Access {
                    expr: object,
                    index,
                },
                Expr::Access {
                    expr: other,
                    index: other_index,
                },
            ) => {
                self.same_value(object, variables, other)
                    && self.same_value(index, variables, other_index)
            }
            (
                Expr::DotAccess {
                    expr: operand,
                    field,
                },
                Expr::DotAccess {
                    expr: other,
                    field: other_field,
                },
            ) => field == other_field && self.same_value(operand, variables, other),
            (
                Expr::UnaryOp { op, expr: operand },
                Expr::UnaryOp {
                    op: other_op,
                    expr: other,
                },
            ) => op == other_op && self.same_value(operand, variables, other),
            (
                Expr::PostfixOp { op, expr: operand },
                Expr::PostfixOp {
                    op: other_op,
                    expr: other,
                },
            ) => op == other_op && self.same_value(operand, variables, other),
            (Expr::Identifier(name), _) if is_constant_name(*name) => {
                matches!(&expr.node, Expr::Identifier(other) if other == name)
            }
            _ if variable_name(kept).is_some() => match (variables.next(), self.place(expr)) {
                (Some(kept_reg), Resolved::Register(reg) | Resolved::Global(_, reg)) => {
                    *kept_reg == reg
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn do_block(
        &mut self,
        statements: &[Commented<SpannedExpr>],
        return_expr: &SpannedExpr,
        target: Target,
    ) -> CResult<()> {
        let mut scope = Scope {
            kind: ScopeKind::Block,
            vars: Vec::new(),
            param_regs: Vec::new(),
            locals: Vec::new(),
            base: self.free_reg,
        };

        let mut assigned = Vec::new();
        collect_block_assignment_exprs(statements, return_expr, &mut assigned);
        self.declare_maybe_vars(&mut scope, &assigned)?;
        self.scopes.push(scope);

        for statement in statements {
            self.block_statement(&statement.node)?;
        }

        self.block_return(return_expr, target)?;
        self.pop_scope();

        Ok(())
    }

    /// The error raised by assigning to a reserved name as a do-block statement, if any.
    fn block_keyword_error(ident: Symbol) -> Option<String> {
        matches!(
            ident.as_str(),
            "return" | "if" | "then" | "else" | "do" | "true" | "false" | "null" | "output"
        )
        .then(|| format!("{} is a keyword, and cannot be reassigned", ident))
    }

    /// Compile a do-block statement. Statement-level assignments may shadow outer variables and
    /// rebind earlier variables of the same block.
    fn block_statement(&mut self, statement: &SpannedExpr) -> CResult<()> {
        let mark = self.free_reg;

        let Expr::Assignment { ident, value } = &statement.node else {
            self.expr_any(statement)?;
            self.free_reg = mark;

            return Ok(());
        };

        if let Some(message) = Self::block_keyword_error(*ident) {
            self.raise(message, statement.span);
            return Ok(());
        }

        let scope = self.scopes.last().expect("do-block scope");
        let existing = scope
            .vars
            .iter()
            .rev()
            .find(|var| var.name == *ident)
            .map(|var| var.reg);

        if let Some(reg) = existing {
            // The value may read the variable it rebinds, so it is computed elsewhere first.
            let temp = self.alloc()?;
            self.expr_into(value, temp)?;
            self.name_lambda(value, temp, *ident);
            self.emit(Instr::abc(Op::Move, reg, temp, 0));
            self.free_reg = mark;

            return Ok(());
        }

        // The register stays allocated until the block ends; the variable only becomes visible
        // once its value has been computed.
        let reg = self.alloc()?;
        self.expr_into(value, reg)?;
        self.name_lambda(value, reg, *ident);
        self.declare(*ident, reg, false);

        Ok(())
    }

    fn block_return(&mut self, return_expr: &SpannedExpr, target: Target) -> CResult<()> {
        let Expr::Assignment { ident, value } = &return_expr.node else {
            return match target {
                Target::Register(dst) => self.expr_into(return_expr, dst),
                Target::Return => self.expr_return(return_expr),
            };
        };

        if let Some(message) = Self::block_keyword_error(*ident) {
            self.raise(message, return_expr.span);
            return Ok(());
        }

        // The block ends right after this assignment, so nothing can observe the binding
        // itself; only the value (and the naming of a function value) matters.
        match target {
            Target::Register(dst) => {
                self.expr_into(value, dst)?;
                self.name_lambda(value, dst, *ident);
            }
            Target::Return => {
                let mark = self.free_reg;
                let reg = self.alloc()?;
                self.expr_into(value, reg)?;
                self.name_lambda(value, reg, *ident);
                self.emit(Instr::abc(Op::Return, reg, 0, 0));
                self.free_reg = mark;
            }
        }

        Ok(())
    }

    fn name_lambda(&mut self, value: &SpannedExpr, reg: u16, ident: Symbol) {
        if !may_be_lambda(value) {
            return;
        }

        let index = self.name(ident);
        let mut instr = Instr::abx(Op::NameLambda, reg, index);
        instr.x = matches!(value.node, Expr::Lambda { .. }) as u8;
        self.emit(instr);
    }

    /// Compile an assignment used as an expression (or as a top-level statement): the name
    /// must not be visible anywhere in the scope chain, and the binding goes into the
    /// innermost scope.
    fn assignment(
        &mut self,
        span: Span,
        ident: Symbol,
        value: &SpannedExpr,
        dst: u16,
    ) -> CResult<()> {
        if is_built_in_function(ident.as_str()) {
            self.raise(
                format!(
                    "{} is the name of a built-in function, and cannot be reassigned",
                    ident
                ),
                span,
            );
            return Ok(());
        }

        if matches!(
            ident.as_str(),
            "constants"
                | "if"
                | "then"
                | "else"
                | "true"
                | "false"
                | "null"
                | "inputs"
                | "and"
                | "or"
        ) {
            self.raise(
                format!("{} is a keyword, and cannot be reassigned", ident),
                span,
            );
            return Ok(());
        }

        let name = self.name(ident);
        self.emit_spanned(Instr::abx(Op::CheckUnbound, 0, name), span);

        self.expr_into(value, dst)?;
        self.name_lambda(value, dst, ident);

        let scope = self.scopes.last().expect("assignment outside of any scope");
        match scope.kind {
            ScopeKind::Root if self.is_chunk => {
                self.emit(Instr::abx(Op::SetGlobal, dst, name));

                // The value may have bound the name itself, to something that a reference
                // in it then read into the cache register of the name; this binds the name
                // again. The register follows it.
                if let Some(&(_, reg)) = self.globals.iter().find(|(global, _)| *global == ident) {
                    self.emit(Instr::abc(Op::Move, reg, dst, 0));

                    if !self.ensured.contains(&ident) {
                        self.ensured.push(ident);
                    }
                }
            }
            ScopeKind::Root | ScopeKind::Block => {
                let reg = scope
                    .vars
                    .iter()
                    .find(|var| var.maybe && var.name == ident)
                    .map(|var| var.reg)
                    .expect("assignment expression target was not pre-declared");

                if reg != dst {
                    self.emit(Instr::abc(Op::Move, reg, dst, 0));
                }
            }
            ScopeKind::Callback => {
                unreachable!("callbacks with frame-level assignments are never inlined")
            }
        }

        Ok(())
    }

    fn list(&mut self, items: &[Commented<SpannedExpr>], dst: u16) -> CResult<()> {
        let mark = self.free_reg;
        let has_spread = items
            .iter()
            .any(|item| matches!(item.node.node, Expr::Spread(_)));

        if items.len() <= LIST_BATCH {
            let first = self.free_reg;
            for item in items {
                let reg = self.alloc()?;
                self.expr_into(&item.node, reg)?;
            }

            let op = match has_spread {
                true => Op::NewListSpread,
                false => Op::NewList,
            };
            self.emit(Instr::abc(op, dst, first, items.len() as u16));
            self.free_reg = mark;

            return Ok(());
        }

        self.emit(Instr::abc(Op::ListBegin, 0, 0, 0));

        for batch in items.chunks(LIST_BATCH) {
            let first = self.free_reg;
            for item in batch {
                let reg = self.alloc()?;
                self.expr_into(&item.node, reg)?;
            }

            self.emit(Instr::abc(Op::ListAppend, first, batch.len() as u16, 0));
            self.free_reg = mark;
        }

        self.emit(Instr::abc(Op::ListEnd, dst, 0, 0));

        Ok(())
    }

    fn record(&mut self, entries: &[Commented<RecordEntry>], dst: u16) -> CResult<()> {
        let mark = self.free_reg;
        let all_static = entries
            .iter()
            .all(|entry| matches!(entry.node.key, RecordKey::Static(_)));

        // A function that has used up its layouts builds the rest of its records key by key.
        if all_static && entries.len() <= MAX_SHAPE_KEYS && self.layouts.len() < MAX_LAYOUTS {
            let mut keys = IndexSet::new();
            let mut slots = Vec::with_capacity(entries.len());

            for entry in entries {
                if let RecordKey::Static(key) = &entry.node.key {
                    let (slot, _) = keys.insert_full(key.clone());
                    slots.push(slot as u16);
                }
            }

            // The layout takes its place before the values are compiled: they can hold record
            // literals of their own, which may use up the layouts that are left.
            let layout = self.layouts.len() as u16;
            self.layouts.push(RecordLayout {
                distinct: keys.len() == slots.len(),
                keys: Keys::interned(keys.iter().map(String::as_str)),
                slots,
            });

            let first = self.free_reg;
            for entry in entries {
                let reg = self.alloc()?;
                self.expr_into(&entry.node.value, reg)?;
            }

            self.emit(Instr::abc(Op::NewRecord, dst, first, layout));
            self.free_reg = mark;

            return Ok(());
        }

        // How many entries the literal spells out tells the record how much room to make
        // for them when it starts with the entries of a spread.
        let spelled_out = entries
            .iter()
            .filter(|entry| !matches!(entry.node.key, RecordKey::Spread(_)))
            .count();
        self.emit(Instr::abx(Op::RecordBegin, 0, spelled_out as u32));

        for entry in entries {
            let entry = &entry.node;

            match &entry.key {
                RecordKey::Static(key) => {
                    let value = self.expr_any(&entry.value)?;
                    let key = self.string(key);
                    self.emit(Instr::abx(Op::RecordSet, value, key));
                }
                RecordKey::Dynamic(key_expr) => {
                    let key = self.expr_any(key_expr)?;
                    self.emit(Instr::abc(Op::RecordKey, key, 0, 0));
                    let value = self.expr_any(&entry.value)?;
                    self.emit(Instr::abc(Op::RecordValue, value, 0, 0));
                }
                RecordKey::Shorthand(ident) => {
                    // Shorthand keys read the variable by name, without the special handling
                    // (or the error span) identifier expressions get.
                    let name = Symbol::intern(ident);
                    let value = match self.resolve(name) {
                        Resolved::Register(reg) => reg,
                        Resolved::Global(_, reg) if self.ensured.contains(&name) => reg,
                        Resolved::Global(..) | Resolved::Dynamic => {
                            let reg = self.alloc()?;
                            let index = self.name(name);
                            let mut instr = Instr::abx(Op::GetName, reg, index);
                            instr.x = 1;
                            self.emit(instr);
                            reg
                        }
                    };
                    let key = self.string(ident);
                    self.emit(Instr::abx(Op::RecordSet, value, key));
                }
                RecordKey::Spread(spread_expr) => {
                    let value = self.expr_any(spread_expr)?;
                    self.emit(Instr::abc(Op::RecordSpread, value, 0, 0));
                }
            }

            self.free_reg = mark;
        }

        self.emit(Instr::abc(Op::RecordEnd, dst, 0, 0));

        Ok(())
    }

    fn closure(
        &mut self,
        args: &[LambdaArg],
        body: &Rc<SpannedExpr>,
        captures: &[Symbol],
        dst: u16,
    ) -> CResult<()> {
        let capture_sources = captures
            .iter()
            .map(|name| match self.resolve(*name) {
                Resolved::Register(reg) => CaptureSource::Register(reg),
                Resolved::Global(_, reg) if self.ensured.contains(name) => {
                    CaptureSource::Register(reg)
                }
                Resolved::Global(..) | Resolved::Dynamic => CaptureSource::Dynamic,
            })
            .collect();

        let index = self.lambdas.len() as u32;
        self.lambdas.push(LambdaTemplate {
            args: Rc::new(args.to_vec()),
            body: Rc::clone(body),
            captures: Rc::new(captures.to_vec()),
            capture_sources,
            proto: OnceCell::new(),
        });
        self.emit(Instr::abx(Op::Closure, dst, index));

        Ok(())
    }

    /// Compile a call whose result goes to `dst`. `tail` is set when the call is the last
    /// thing the function does: `dst` is then a fresh register that is returned right away.
    fn call(
        &mut self,
        span: Span,
        func: &SpannedExpr,
        args: &[SpannedExpr],
        dst: u16,
        tail: bool,
    ) -> CResult<()> {
        if self.inline
            && let Some(inline) = inline_call(func, args)
        {
            return self.inline_loop(span, &inline, dst);
        }

        if self.inline
            && let Expr::BuiltIn(built_in) = &func.node
            && self.built_in_call(span, *built_in, args, dst)?
        {
            return Ok(());
        }

        let mark = self.free_reg;
        let candidate = self.self_call_candidate(func, args);

        if candidate.is_some() && candidate == self.self_target {
            // The frame can only be replaced when nothing could tell: a callee resolves the
            // names it does not bind through its caller's variables.
            if tail && !self.has_visible_vars() {
                return self.tail_self(span, args, dst);
            }

            let func_reg = self.alloc()?;
            for arg in args {
                let reg = self.alloc()?;
                self.expr_into(arg, reg)?;
            }

            let mut instr = Instr::abc(Op::CallSelf, func_reg, self.restart_distance(), dst);
            instr.x = args.len() as u8;
            self.emit_spanned(instr, span);
            self.free_reg = mark;

            return Ok(());
        }

        if let Some((name, slot)) = candidate
            && self.self_target.is_none()
        {
            self.note_self_ref(name, slot);
        }

        let func_reg = self.alloc()?;
        self.expr_into(func, func_reg)?;

        let has_spread = args.iter().any(|arg| matches!(arg.node, Expr::Spread(_)));
        if has_spread || args.len() > MAX_DIRECT_ARGS {
            // The argument count is only known at runtime, or there are more arguments than
            // should be live in registers at once: gather the (flattened) arguments in a list
            // builder and call with its contents.
            self.emit(Instr::abc(Op::ListBegin, 0, 0, 0));

            for batch in args.chunks(LIST_BATCH) {
                let first = self.free_reg;
                for arg in batch {
                    let reg = self.alloc()?;
                    self.expr_into(arg, reg)?;
                }

                self.emit(Instr::abc(Op::ListAppend, first, batch.len() as u16, 0));
                self.free_reg = first;
            }

            self.emit_spanned(Instr::abc(Op::CallList, func_reg, 0, dst), span);
            self.free_reg = mark;

            return Ok(());
        }

        // The callee and its arguments occupy consecutive registers; the callee's frame is
        // laid directly over the arguments.
        for arg in args {
            let reg = self.alloc()?;
            self.expr_into(arg, reg)?;
        }

        self.emit_spanned(Instr::abc(Op::Call, func_reg, args.len() as u16, dst), span);
        self.free_reg = mark;

        Ok(())
    }

    /// Compile a call that names a built-in function as an instruction that calls that
    /// function, which leaves nothing to find out about what is called. Returns whether the
    /// call has such an instruction: one that passes a number of arguments the function does
    /// not take is left to fail the way any call does, and so is one whose number of
    /// arguments only shows when it is made.
    ///
    /// Either way the arguments are evaluated from left to right before anything is checked.
    fn built_in_call(
        &mut self,
        span: Span,
        built_in: BuiltInFunction,
        args: &[SpannedExpr],
        dst: u16,
    ) -> CResult<bool> {
        let spelled_out = args.len() <= MAX_DIRECT_ARGS
            && !args.iter().any(|arg| matches!(arg.node, Expr::Spread(_)));
        if !spelled_out || !built_in.arity().can_accept(args.len()) {
            return Ok(false);
        }

        let mark = self.free_reg;

        if let Some(op) = Op::of_call(built_in, args.len()) {
            // The instruction reads its arguments wherever they are.
            let first = self.expr_any(&args[0])?;
            let second = match args.get(1) {
                Some(arg) => self.expr_any(arg)?,
                None => 0,
            };
            self.emit_spanned(Instr::abc(op, dst, first, second), span);
        } else {
            let function = self.constant(Value::BuiltIn(built_in));
            let Ok(function) = u16::try_from(function) else {
                return Ok(false);
            };

            // The arguments occupy consecutive registers.
            let first = match args.is_empty() {
                true => 0,
                false => self.free_reg,
            };
            for arg in args {
                let reg = self.alloc()?;
                self.expr_into(arg, reg)?;
            }

            let mut instr = Instr::abc(Op::CallBuiltIn, first, function, dst);
            instr.x = args.len() as u8;
            self.emit_spanned(instr, span);
        }

        self.free_reg = mark;

        Ok(true)
    }

    /// What a self call takes for the way back to the start of the code: the distance from
    /// there to the word after the call, which is the next one to be emitted. A variant too
    /// long for the operand is rejected as a whole (see `compile_function`), so the truncation
    /// never reaches the VM.
    fn restart_distance(&self) -> u16 {
        (self.here() + 1) as u16
    }

    /// Compile a call of the function to itself that replaces the frame: the arguments become
    /// the parameters and the code starts over. `dst` is the register an ordinary call would
    /// leave its result in, and the registers after it are where such a call takes its
    /// arguments from.
    ///
    /// An argument is computed right into its parameter when nothing that is evaluated after
    /// it can see that parameter, and one that passes its own parameter on takes no code at
    /// all. The others wait where an ordinary call wants them, as does whatever lies between
    /// the first and the last of them, and the instruction moves that run into place.
    fn tail_self(&mut self, span: Span, args: &[SpannedExpr], dst: u16) -> CResult<()> {
        let mark = self.free_reg;
        debug_assert_eq!(mark, dst + 1);

        for _ in args {
            self.alloc()?;
        }

        // Going backwards, `observed` holds the parameters that the arguments after the
        // current one can see.
        let mut observed = 0;
        let mut waiting = 0..0;

        for (index, arg) in args.iter().enumerate().rev() {
            let passed_on = self.frame_reg(arg) == Some(index as u16);
            let seen = index >= TRACKED_PARAMS || observed & (1 << index) != 0;

            if seen && !passed_on {
                if waiting.is_empty() {
                    waiting.end = index + 1;
                }
                waiting.start = index;
            }

            observed |= self.observed_params(arg);
        }

        for (index, arg) in args.iter().enumerate() {
            let reg = if waiting.contains(&index) {
                mark + index as u16
            } else {
                index as u16
            };
            self.expr_into(arg, reg)?;
        }

        let first = mark + waiting.start as u16;
        let op = match waiting.len() {
            0 => Op::TailSelf0,
            1 => Op::TailSelf1,
            _ => Op::TailSelf,
        };
        let mut instr = Instr::abc(op, first, self.restart_distance(), dst);
        instr.x = waiting.len() as u8;
        self.emit_spanned(instr, span);
        self.free_reg = mark;

        Ok(())
    }

    /// The register that a variable reference in the body of a function reads its value from:
    /// `None` for anything that is not a reference to a variable that is known to be bound.
    fn frame_reg(&self, expr: &SpannedExpr) -> Option<u16> {
        match self.place(expr) {
            Resolved::Register(reg) => Some(reg),
            Resolved::Global(..) | Resolved::Dynamic => None,
        }
    }

    /// The parameters of the function that evaluating `expr` can see, as a bit set over the
    /// registers of the first [`TRACKED_PARAMS`] of them. This is for a point where no other
    /// variable of the function is visible, so a name that is looked up when the code runs is
    /// not that of a parameter. An expression that may run code from outside of the function
    /// sees every parameter, as that code can look each of them up by name.
    fn observed_params(&self, expr: &SpannedExpr) -> u64 {
        match &expr.node {
            Expr::Number(_) | Expr::Bool(_) | Expr::Null | Expr::String(_) | Expr::BuiltIn(_) => 0,
            Expr::Identifier(_) | Expr::ParamSlot { .. } => match self.frame_reg(expr) {
                Some(reg) if (reg as usize) < TRACKED_PARAMS => 1 << reg,
                _ => 0,
            },
            Expr::CaptureSlot { name, .. } if !self.params.contains(name) => 0,
            Expr::UnaryOp { expr: operand, .. }
            | Expr::PostfixOp { expr: operand, .. }
            | Expr::DotAccess { expr: operand, .. }
            | Expr::Spread(operand) => self.observed_params(operand),
            Expr::BinaryOp { op, left, right }
                if !matches!(op, BinaryOp::Via | BinaryOp::Into | BinaryOp::Where) =>
            {
                self.observed_params(left) | self.observed_params(right)
            }
            Expr::Access {
                expr: object,
                index,
            } => self.observed_params(object) | self.observed_params(index),
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                self.observed_params(condition)
                    | self.observed_params(then_expr)
                    | self.observed_params(else_expr)
            }
            Expr::List(items) => items
                .iter()
                .fold(0, |seen, item| seen | self.observed_params(&item.node)),
            Expr::Call { func, args } => match &func.node {
                Expr::BuiltIn(built_in) if !built_in.takes_callback() => args
                    .iter()
                    .fold(0, |seen, arg| seen | self.observed_params(arg)),
                _ => u64::MAX,
            },
            _ => u64::MAX,
        }
    }

    /// The first state register of the loop, and the part of its entry, that `expr` reads
    /// when it is `name[0]` or `name[1]` for the parameter `name` of an inlined loop that
    /// takes the entries of a record apart (see [`takes_entries_apart`]).
    fn entry_access(&self, expr: &SpannedExpr) -> Option<(u16, EntryPart)> {
        if self.apart.is_empty() {
            return None;
        }

        let Expr::Access {
            expr: object,
            index,
        } = &expr.node
        else {
            return None;
        };
        let part = pair_part(index)?;
        let Resolved::Register(reg) = self.place(object) else {
            return None;
        };

        self.apart
            .iter()
            .find(|state| **state + 2 == reg)
            .map(|state| (*state, part))
    }

    /// The register holding the value of `expr`, when it was computed ahead of the loop that
    /// is being compiled.
    fn hoisted_reg(&self, expr: &SpannedExpr) -> Option<u16> {
        self.hoisted
            .iter()
            .find(|(hoisted, _)| std::ptr::eq(*hoisted, expr))
            .map(|(_, reg)| *reg)
    }

    /// Whether `expr` gets to its value without running anything but its own operators, and
    /// from nothing but constants and the variables in the registers below `limit`: it reads
    /// elements and fields and applies operators that call no function. All such an
    /// expression does is come to a value or fail, and it does the same whenever those
    /// variables hold the same values. `budget` is the number of nodes that are still looked
    /// at.
    ///
    /// With the first state register of an inlined loop for `limit`, the variables are those
    /// that were bound before the loop, so the expression comes to the same value on every
    /// pass through it.
    fn is_plain(&self, expr: &SpannedExpr, limit: u16, budget: &mut usize) -> bool {
        if *budget == 0 {
            return false;
        }
        *budget -= 1;

        match &expr.node {
            Expr::Number(_) | Expr::Bool(_) | Expr::Null | Expr::String(_) => true,
            Expr::Identifier(name) if is_constant_name(*name) => true,
            Expr::Identifier(_) | Expr::ParamSlot { .. } | Expr::CaptureSlot { .. } => {
                match self.place(expr) {
                    Resolved::Register(reg) | Resolved::Global(_, reg) => reg < limit,
                    Resolved::Dynamic => false,
                }
            }
            _ => match plain_operands(expr) {
                Some(operands) => operands
                    .into_iter()
                    .flatten()
                    .all(|operand| self.is_plain(operand, limit, budget)),
                None => false,
            },
        }
    }

    /// Compute what the body of an inlined callback starts with ahead of its loop, where that
    /// is the same on every pass (see [`Self::is_plain`]). `expr` is the next expression
    /// the body evaluates, and `state` the first state register of the loop.
    ///
    /// This is called where the first pass is about to begin, so the code it emits runs as
    /// the start of that pass, and the passes after it start behind that code. Nothing can
    /// tell that from computing the same values again: an expression is only moved when the
    /// pass has done nothing before it that could fail or be observed, so an error it raises
    /// is the first thing the pass does either way.
    ///
    /// Returns whether that still holds after `expr`: whether everything it evaluates was
    /// moved or cannot fail.
    fn hoist(&mut self, expr: &SpannedExpr, state: u16, room: &mut usize) -> CResult<bool> {
        match &expr.node {
            Expr::Number(_) | Expr::Bool(_) | Expr::Null | Expr::String(_) | Expr::BuiltIn(_) => {
                return Ok(true);
            }
            Expr::Identifier(name) if is_constant_name(*name) => return Ok(true),
            Expr::Identifier(_) | Expr::ParamSlot { .. } | Expr::CaptureSlot { .. } => {
                // A name of the chunk that is first read here has its cache checked here.
                return Ok(self.direct_reg(expr).is_some());
            }
            _ => {}
        }

        if *room == 0 {
            return Ok(false);
        }

        if self.is_plain(expr, state, &mut MAX_INVARIANT_NODES.clone()) {
            let reg = self.alloc()?;
            self.expr_into(expr, reg)?;
            self.hoisted.push((expr, reg));
            *room -= 1;

            return Ok(true);
        }

        // What is left evaluates its operands in this order and then does something that may
        // fail, or that differs from one pass to the next.
        match &expr.node {
            Expr::BinaryOp { left, right, .. } => {
                let _ = self.hoist(left, state, room)? && self.hoist(right, state, room)?;
            }
            Expr::Access {
                expr: object,
                index,
            } => {
                let _ = self.hoist(object, state, room)? && self.hoist(index, state, room)?;
            }
            Expr::DotAccess { expr: operand, .. }
            | Expr::UnaryOp { expr: operand, .. }
            | Expr::PostfixOp { expr: operand, .. } => {
                self.hoist(operand, state, room)?;
            }
            Expr::Conditional { condition, .. } => match &condition.node {
                // A comparison that decides a branch is compiled together with the branch.
                Expr::BinaryOp { op, left, right } if compare_jump_opcodes(*op).is_some() => {
                    let _ = self.hoist(left, state, room)? && self.hoist(right, state, room)?;
                }
                _ => {
                    self.hoist(condition, state, room)?;
                }
            },
            Expr::Call { func, args } => self.hoist_call(func, args, state, room)?,
            _ => {}
        }

        Ok(false)
    }

    /// [`Self::hoist`] for what a call evaluates before it calls.
    fn hoist_call(
        &mut self,
        func: &SpannedExpr,
        args: &[SpannedExpr],
        state: u16,
        room: &mut usize,
    ) -> CResult<()> {
        if self.inline
            && let Some(inline) = inline_call(func, args)
        {
            // A loop of its own: its range has to be checked once its bounds are known, and
            // a list is followed by the initial value before anything is checked.
            if let Some(range_args) = range_call(inline.list) {
                for arg in range_args {
                    if !self.hoist(arg, state, room)? {
                        break;
                    }
                }
            } else if self.hoist(inline.list, state, room)?
                && let Some(init) = inline.init
            {
                self.hoist(init, state, room)?;
            }

            return Ok(());
        }

        // The arguments of the other calls follow the function, unless they are gathered in
        // a list. (A call of the enclosing function to itself evaluates no function, but a
        // callback never makes one: see `self_call_candidate`.)
        let gathered = args.len() > MAX_DIRECT_ARGS
            || args.iter().any(|arg| matches!(arg.node, Expr::Spread(_)));
        if gathered || !self.hoist(func, state, room)? {
            return Ok(());
        }

        for arg in args {
            if !self.hoist(arg, state, room)? {
                break;
            }
        }

        Ok(())
    }

    /// Give each name that the callback of an inlined loop of a function looks up through the
    /// frame chain a cache register for as long as the loop runs: a name that no scope around
    /// the loop binds, and that no assignment expression around it can come to bind. Returns
    /// the number of cache registers that the loops around this one have.
    ///
    /// What the frame finds for such a name is the same wherever in the loop it looks, and
    /// whenever: the frame has no variable of that name there, the function it runs keeps
    /// its name and what it captured, the frames below are waiting for this one, and nothing
    /// is assigned at the top level of the program while a function runs. So the name is
    /// looked up where the loop reads it first, and the register counts among those that
    /// hold the same value on every pass (see [`Self::is_plain`]).
    ///
    /// The registers start out unbound whenever the loop starts, and lie below its state.
    fn cache_loop_names(&mut self, call: &InlineCall) -> CResult<usize> {
        let before = self.globals.len();
        if self.is_chunk {
            return Ok(before);
        }

        let mut names = Vec::new();
        collect_late_names(call.body, true, &mut names);
        names.retain(|name| {
            matches!(self.resolve(*name), Resolved::Dynamic) && !self.has_maybe_var(*name)
        });
        names.truncate(MAX_LOOP_NAMES);

        for name in names {
            let reg = self.alloc()?;
            self.emit(Instr::abc(Op::LoadUnbound, reg, 0, 0));
            self.globals.push((name, reg));
        }

        Ok(before)
    }

    /// Give up the cache registers of an inlined loop that has ended, which leaves `before`
    /// of them (see [`Self::cache_loop_names`]).
    fn release_loop_names(&mut self, before: usize) {
        let released = self.globals.split_off(before);

        self.ensured
            .retain(|name| !released.iter().any(|(released, _)| released == name));
    }

    /// Compile `map`/`filter`/`reduce`/`every`/`some` over a literal lambda as an inline loop,
    /// and `via`/`where` with a literal lambda on the right as one that runs when what is on
    /// the left is a list. `span` is where the call is, or the operation. A loop over a
    /// `range`, `keys`, `values` or `entries` call goes through what the call would list
    /// without making the list.
    fn inline_loop(&mut self, span: Span, call: &InlineCall, dst: u16) -> CResult<()> {
        let mark = self.free_reg;
        let cached = self.cache_loop_names(call)?;
        let is_reduce = call.builtin == BuiltInFunction::Reduce;

        // How much deeper than the frame the callback would have been called: a built-in
        // function calls it from one level further in than its own call, and an operator
        // from where the operator is. Its body runs one level further in than that.
        let call_depth = match call.operator {
            Some(_) => 0,
            None => CALLBACK_CALL_DEPTH,
        };
        let body_depth = i32::from(call_depth) + 1;
        let element_params = if is_reduce {
            &call.params[1..]
        } else {
            call.params
        };
        let wants_index = element_params.len() == 2;
        let range_args = range_call(call.list);
        let entries = match range_args {
            Some(_) => None,
            None => entries_call(call.list),
        };

        // What the loop is given of each entry of a record. A callback that only takes
        // apart the list of a key and a value that `entries` makes for an entry is given the
        // value, and no such list is made.
        let apart = entries
            .as_ref()
            .is_some_and(|entries| entries.part == EntryPart::Pair)
            && takes_entries_apart(call, element_params[0].get_name());
        let part = match &entries {
            Some(_) if apart => Some(EntryPart::Value),
            Some(entries) => Some(entries.part),
            None => None,
        };

        // Loop state registers, laid out the way the iteration instructions expect:
        //   list:   [cursor, list, element, index, end]
        //   range:  [next, end, start, element, index]
        //   record: [position, record, element, index]
        let state = self.free_reg;
        let state_regs = match &entries {
            Some(_) => 4,
            None => 5,
        };
        for _ in 0..state_regs {
            self.alloc()?;
        }
        let (element_reg, index_reg) = if range_args.is_some() {
            (state + 3, state + 4)
        } else {
            (state + 2, state + 3)
        };

        if let Some(entries) = &entries {
            // The call that lists the entries is made before anything else is evaluated,
            // and all that it comes to here is what it checks.
            self.expr_into(entries.record, state + 1)?;

            let prep = self.emit(Instr::abc(Op::IterRecord, state, 0, 0));
            let context = format!("built-in function \"{}\"", entries.built_in.name());
            self.handlers.push(Handler {
                start: prep as u32,
                end: self.here() as u32,
                wraps: vec![
                    Wrap::FunctionContext(Rc::from(context)),
                    Wrap::CallSite(call.list.span),
                ],
            });
        } else if let Some(range_args) = range_args {
            let args_mark = self.free_reg;
            let first = self.free_reg;
            for arg in range_args {
                let reg = self.alloc()?;
                self.expr_into(arg, reg)?;
            }

            let prep = self.emit(Instr::abc(
                Op::RangePrep,
                state,
                first,
                range_args.len() as u16,
            ));
            self.handlers.push(Handler {
                start: prep as u32,
                end: self.here() as u32,
                wraps: vec![
                    Wrap::FunctionContext(Rc::from("built-in function \"range\"")),
                    Wrap::CallSite(call.list.span),
                ],
            });
            self.free_reg = args_mark;
        } else {
            self.expr_into(call.list, state + 1)?;
        }

        let acc_reg = match call.init {
            Some(init) => {
                let reg = self.alloc()?;
                self.expr_into(init, reg)?;
                Some(reg)
            }
            None => None,
        };

        let region_start = self.here();

        // A built-in function fails when it is given anything but a list. An operator does
        // something else with that, which is compiled behind the loop. A range and the
        // entries of a record are lists whenever there is no error.
        let mut to_operator = None;
        if range_args.is_none() && entries.is_none() {
            match call.operator {
                Some(_) => to_operator = Some(self.emit_jump(Op::IterList, state)),
                None => {
                    self.emit(Instr::abc(Op::IterPrep, state, 0, 0));
                }
            }
        }

        // The iteration instructions, their operand `x`, and the capacity hint of a list
        // that gets an element per pass.
        let (entry_op, next_op, mode, hint) = match (range_args, part) {
            (Some(_), _) => (Op::ForRange, Op::ForRangeNext, wants_index as u8, 2),
            (None, Some(part)) => (
                Op::ForRecord,
                Op::ForRecordNext,
                part.operand(wants_index),
                3,
            ),
            (None, None) => (Op::ForList, Op::ForListNext, wants_index as u8, 1),
        };

        if matches!(call.builtin, BuiltInFunction::Map | BuiltInFunction::Filter) {
            // Mapping produces exactly one result per element, so the result list can be sized
            // up front; filtering starts empty like the built-in does.
            let mut begin = Instr::abc(Op::ListBegin, state, 0, 0);
            if call.builtin == BuiltInFunction::Map {
                begin.x = hint;
            }
            self.emit(begin);
        }

        let mut entry = Instr::abx(entry_op, state, 0);
        entry.x = mode;
        let to_exit = self.emit(entry);

        self.emit(Instr::abc(Op::CallbackDepth, 0, call_depth, 0));
        self.emit(Instr::abx(Op::AddDepth, 0, body_depth as u32));

        let ensured = self.ensured.len();

        self.push_scope(ScopeKind::Callback);
        let mut param_regs = Vec::with_capacity(call.params.len());
        if let Some(acc) = acc_reg {
            param_regs.push(acc);
        }
        param_regs.push(element_reg);
        if wants_index {
            param_regs.push(index_reg);
        }

        for (param, reg) in call.params.iter().zip(&param_regs) {
            // With duplicate parameter names, by-name lookups see the first one.
            let name = param.get_name();
            let duplicate = self
                .scopes
                .last()
                .is_some_and(|scope| scope.vars.iter().any(|var| var.name == name));
            if !duplicate {
                self.declare(name, *reg, false);
            }
        }
        self.scopes.last_mut().expect("callback scope").param_regs = param_regs;

        let taken_apart = self.apart.len();
        if apart {
            self.apart.push(state);
        }

        // The first pass through the body starts here. It begins by computing what every pass
        // would compute the same way, and the passes after it start behind that.
        let body_start = self.here();
        let hoisted = self.hoisted.len();
        self.hoist(call.body, state, &mut MAX_HOISTED.clone())?;

        let loop_start = self.here();
        let body_mark = self.free_reg;

        // After the body: the instructions that consume its result, then the exits that leave
        // the loop early (each has to undo the depth adjustment).
        let mut early_exit = None;
        let body_end;

        match call.builtin {
            BuiltInFunction::Map => {
                let result = self.expr_any(call.body)?;
                body_end = self.here();
                self.emit(Instr::abc(Op::ListPush, result, 0, 0));
            }
            BuiltInFunction::Filter => {
                let result = self.expr_any(call.body)?;
                body_end = self.here();
                let skip = self.emit_jump(Op::JmpIfFalse, result);

                // An entry that is kept is the list of its key and its value after all.
                let kept = match apart {
                    true => {
                        let pair = self.alloc()?;
                        let mut load = Instr::abc(Op::LoadEntry, pair, state, 0);
                        load.x = EntryPart::Pair as u8;
                        self.emit(load);
                        pair
                    }
                    false => element_reg,
                };
                self.emit(Instr::abc(Op::ListPush, kept, 0, 0));
                self.patch_jump(skip, self.here());
            }
            BuiltInFunction::Reduce => {
                self.expr_into(call.body, acc_reg.expect("reduce accumulator"))?;
                body_end = self.here();
            }
            BuiltInFunction::Every => {
                let result = self.expr_any(call.body)?;
                body_end = self.here();
                early_exit = Some(self.emit_jump(Op::JmpIfFalse, result));
            }
            BuiltInFunction::Some => {
                let result = self.expr_any(call.body)?;
                body_end = self.here();
                let keep_going = self.emit_jump(Op::JmpIfFalse, result);
                early_exit = Some(self.emit_jump(Op::Jmp, 0));
                self.patch_jump(keep_going, self.here());
            }
            _ => unreachable!("not an inlinable built-in"),
        }

        self.handlers.push(Handler {
            start: body_start as u32,
            end: body_end as u32,
            wraps: vec![Wrap::FunctionContext(Rc::from("anonymous function"))],
        });

        // The callback's parameters are only visible to the body.
        let scope = self.scopes.pop().expect("callback scope");
        for local in scope.locals {
            self.locals[local].end = body_end as u32;
        }
        self.free_reg = body_mark;
        self.ensured.truncate(ensured);
        self.hoisted.truncate(hoisted);
        self.apart.truncate(taken_apart);

        let mut next = Instr::abx(next_op, state, 0);
        next.x = mode;
        let back = self.emit(next);
        self.patch_jump(back, loop_start);

        self.emit(Instr::abx(Op::AddDepth, 0, (-body_depth) as u32));

        match call.builtin {
            BuiltInFunction::Map | BuiltInFunction::Filter => {
                self.patch_jump(to_exit, self.here());
                self.emit(Instr::abc(Op::ListEnd, dst, 0, 0));
            }
            BuiltInFunction::Reduce => {
                self.patch_jump(to_exit, self.here());
                let acc = acc_reg.expect("reduce accumulator");
                if acc != dst {
                    self.emit(Instr::abc(Op::Move, dst, acc, 0));
                }
            }
            BuiltInFunction::Every | BuiltInFunction::Some => {
                // `every` is true unless a callback said false; `some` is false unless a
                // callback said true.
                let exhausted = call.builtin == BuiltInFunction::Every;

                self.patch_jump(to_exit, self.here());
                self.load_const(dst, Value::Bool(exhausted));
                let to_end = self.emit_jump(Op::Jmp, 0);

                self.patch_jump(early_exit.expect("early exit"), self.here());
                self.emit(Instr::abx(Op::AddDepth, 0, (-body_depth) as u32));
                self.load_const(dst, Value::Bool(!exhausted));

                self.patch_jump(to_end, self.here());
            }
            _ => unreachable!("not an inlinable built-in"),
        }

        // An error that leaves the loop is told where it comes from the way the built-in
        // function would have told it, or the operator.
        let context = match call.operator {
            Some((BinaryOp::Where, _)) => String::from("\"where\" callback"),
            Some(_) => String::from("\"via\" callback"),
            None => format!("built-in function \"{}\"", call.builtin.name()),
        };
        self.handlers.push(Handler {
            start: region_start as u32,
            end: self.here() as u32,
            wraps: vec![
                Wrap::FunctionContext(Rc::from(context)),
                Wrap::CallSite(span),
            ],
        });

        self.release_loop_names(cached);

        if let (Some(to_operator), Some((op, lambda))) = (to_operator, call.operator) {
            // What is on the left is no list: the operator gets it together with the
            // function that the lambda makes, as it does when nothing is inlined. Nothing
            // was done since the left-hand side was evaluated that the lambda could tell.
            let to_end = self.emit_jump(Op::Jmp, 0);
            self.patch_jump(to_operator, self.here());

            let function = self.alloc()?;
            self.expr_into(lambda, function)?;
            self.emit_spanned(
                Instr::abc(binary_opcode(op), dst, state + 1, function),
                span,
            );

            self.patch_jump(to_end, self.here());
        }

        self.free_reg = mark;

        Ok(())
    }
}

/// The span reported for errors in a binary operation: from the start of the left operand to
/// the end of the right one.
fn binary_span(left: &SpannedExpr, right: &SpannedExpr) -> Span {
    Span::new(
        left.span.start_byte,
        right.span.end_byte,
        left.span.start_line,
        left.span.start_col,
    )
}

/// The `x` operand of the instruction for an arithmetic `op`: which of its operand registers
/// `b` and `c` are temporaries that are released right after it. A register allocated for an
/// operand is written by the code of that operand and read by this instruction alone.
fn last_use_flags(op: BinaryOp, b: bool, c: bool) -> u8 {
    let arithmetic = matches!(
        op,
        BinaryOp::Add
            | BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo
            | BinaryOp::Power
    );
    if !arithmetic {
        return 0;
    }

    (if b { LAST_USE_B } else { 0 }) | (if c { LAST_USE_C } else { 0 })
}

/// The operand that is compared against a `null` literal, and whether the literal comes first.
fn null_operand<'e>(
    left: &'e SpannedExpr,
    right: &'e SpannedExpr,
) -> Option<(&'e SpannedExpr, bool)> {
    if matches!(right.node, Expr::Null) {
        return Some((left, false));
    }

    matches!(left.node, Expr::Null).then_some((right, true))
}

/// The opcode comparing a register against `null`, for operators that have one.
fn null_compare_opcode(op: BinaryOp) -> Option<Op> {
    match op {
        BinaryOp::Equal => Some(Op::EqNull),
        BinaryOp::NotEqual => Some(Op::NeNull),
        _ => None,
    }
}

/// The fused compare-and-branch opcode comparing a register against `null`.
fn null_jump_opcode(op: BinaryOp) -> Option<Op> {
    match op {
        BinaryOp::Equal => Some(Op::JmpNotEqNull),
        BinaryOp::NotEqual => Some(Op::JmpNotNeNull),
        _ => None,
    }
}

/// The opcode for `register <op> constant`, for operators that have one.
fn const_rhs_opcode(op: BinaryOp) -> Option<Op> {
    Some(match op {
        BinaryOp::Add => Op::AddK,
        BinaryOp::Subtract => Op::SubK,
        BinaryOp::Multiply => Op::MulK,
        BinaryOp::Divide => Op::DivK,
        BinaryOp::Modulo => Op::ModK,
        BinaryOp::Power => Op::PowK,
        BinaryOp::Equal => Op::EqK,
        BinaryOp::NotEqual => Op::NeK,
        BinaryOp::Less => Op::LtK,
        BinaryOp::LessEq => Op::LeK,
        BinaryOp::Greater => Op::GtK,
        BinaryOp::GreaterEq => Op::GeK,
        _ => return None,
    })
}

/// The opcode for `constant <op> register`, for operators that have one.
fn const_lhs_opcode(op: BinaryOp) -> Option<Op> {
    Some(match op {
        BinaryOp::Add => Op::KAdd,
        BinaryOp::Subtract => Op::KSub,
        BinaryOp::Multiply => Op::KMul,
        BinaryOp::Divide => Op::KDiv,
        BinaryOp::Modulo => Op::KMod,
        BinaryOp::Power => Op::KPow,
        _ => return None,
    })
}

/// The fused opcode that compares the remainder by a power of two the way `op` does and
/// branches on it.
fn remainder_jump_opcode(op: BinaryOp) -> Option<Op> {
    match op {
        BinaryOp::Equal => Some(Op::JmpNotRemEq),
        BinaryOp::NotEqual => Some(Op::JmpNotRemNe),
        _ => None,
    }
}

/// The fused compare-and-branch opcodes (register and constant right-hand side) for `op`.
fn compare_jump_opcodes(op: BinaryOp) -> Option<(Op, Op)> {
    Some(match op {
        BinaryOp::Equal => (Op::JmpNotEq, Op::JmpNotEqK),
        BinaryOp::NotEqual => (Op::JmpNotNe, Op::JmpNotNeK),
        BinaryOp::Less => (Op::JmpNotLt, Op::JmpNotLtK),
        BinaryOp::LessEq => (Op::JmpNotLe, Op::JmpNotLeK),
        BinaryOp::Greater => (Op::JmpNotGt, Op::JmpNotGtK),
        BinaryOp::GreaterEq => (Op::JmpNotGe, Op::JmpNotGeK),
        _ => return None,
    })
}
