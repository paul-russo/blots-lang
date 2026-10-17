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
//! context), and `range(...)` sources are iterated without materialising the list.

use crate::{
    ast::{
        BinaryOp, Commented, Expr, PostfixOp, RecordEntry, RecordKey, Span, SpannedExpr, UnaryOp,
    },
    bytecode::{
        CaptureSource, FAST_FRAME_SLOTS, FieldCache, Handler, Instr, LAST_USE_B, LAST_USE_C,
        LambdaTemplate, LocalVar, Op, Proto, RecordLayout, SelfRef, Wrap, binary_opcode,
    },
    error::RuntimeError,
    functions::{BuiltInFunction, is_built_in_function},
    heap::RecordPointer,
    intern::Symbol,
    record::Shape,
    values::{LambdaArg, Value},
};
use indexmap::IndexSet;
use std::{cell::OnceCell, rc::Rc};

type CResult<T> = Result<T, RuntimeError>;

/// List literals up to this many elements (and without spreads) are built by one instruction
/// from consecutive registers; larger ones are appended in batches of this size.
const LIST_BATCH: usize = 64;

/// Record literals with only static keys and at most this many entries are built by one
/// instruction from consecutive registers.
const MAX_SHAPE_KEYS: usize = 64;

/// Upper bound on the number of late-bound names a chunk caches in registers.
const MAX_CACHED_GLOBALS: usize = 256;

/// How many call-depth levels separate the caller of a higher-order built-in from the body of
/// its callback: one for the built-in itself, one for the callback call, one for its body.
const CALLBACK_DEPTH: i32 = 3;

/// The most arguments a direct self call can pass: its argument count shares an 8-bit operand
/// with a flag.
const MAX_SELF_CALL_ARGS: usize = 127;

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
    /// boundary environment and is cached in this register.
    Global(u16),
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

/// A call to a higher-order built-in that can be compiled as an inline loop.
pub(crate) struct InlineCall<'e> {
    builtin: BuiltInFunction,
    list: &'e SpannedExpr,
    params: &'e [LambdaArg],
    body: &'e SpannedExpr,
    init: Option<&'e SpannedExpr>,
}

/// Recognise `map(list, lambda)`-style calls whose callback is a literal lambda the compiler
/// can inline. The callback must take only required parameters, in a number the built-in
/// would call it with, and must not assign variables directly in its call frame.
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

    let Expr::Lambda {
        args: params, body, ..
    } = &args[1].node
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

    Some(InlineCall {
        builtin: *builtin,
        list: &args[0],
        params,
        body,
        init: args.get(2),
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

/// Collect the names a chunk may read through its boundary environment, so each can be given a
/// cache register up front. This over-approximates (names that turn out to be block variables
/// simply leave their register unused); lambdas that are not inlined resolve their own names.
fn collect_chunk_names(expr: &SpannedExpr, in_callback: bool, out: &mut Vec<Symbol>) {
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
                collect_chunk_names(&item.node, in_callback, out);
            }
        }
        Expr::Record(entries) => {
            for entry in entries {
                let entry = &entry.node;

                match &entry.key {
                    RecordKey::Dynamic(key) | RecordKey::Spread(key) => {
                        collect_chunk_names(key, in_callback, out);
                    }
                    RecordKey::Static(_) | RecordKey::Shorthand(_) => {}
                }

                if matches!(entry.key, RecordKey::Static(_) | RecordKey::Dynamic(_)) {
                    collect_chunk_names(&entry.value, in_callback, out);
                }
            }
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_chunk_names(condition, in_callback, out);
            collect_chunk_names(then_expr, in_callback, out);
            collect_chunk_names(else_expr, in_callback, out);
        }
        Expr::DoBlock {
            statements,
            return_expr,
        } => {
            for statement in statements {
                collect_chunk_names(&statement.node, in_callback, out);
            }
            collect_chunk_names(&return_expr.node, in_callback, out);
        }
        Expr::Assignment { value, .. } => collect_chunk_names(value, in_callback, out),
        Expr::Output { expr } => collect_chunk_names(expr, in_callback, out),
        Expr::Call { func, args } => {
            if let Some(inline) = inline_call(func, args) {
                collect_chunk_names(inline.list, in_callback, out);
                collect_chunk_names(inline.body, true, out);
                if let Some(init) = inline.init {
                    collect_chunk_names(init, in_callback, out);
                }
                return;
            }

            collect_chunk_names(func, in_callback, out);
            for arg in args {
                collect_chunk_names(arg, in_callback, out);
            }
        }
        Expr::Access { expr, index } => {
            collect_chunk_names(expr, in_callback, out);
            collect_chunk_names(index, in_callback, out);
        }
        Expr::DotAccess { expr, .. } => collect_chunk_names(expr, in_callback, out),
        Expr::BinaryOp { left, right, .. } => {
            collect_chunk_names(left, in_callback, out);
            collect_chunk_names(right, in_callback, out);
        }
        Expr::UnaryOp { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Spread(expr) => {
            collect_chunk_names(expr, in_callback, out);
        }
    }
}

/// Identifiers that evaluate to a constant before any scope is consulted.
fn is_constant_name(name: Symbol) -> bool {
    matches!(name.as_str(), "infinity" | "inf" | "constants")
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

/// Whether `expr` can never evaluate to a spread value, whatever its variables hold.
fn never_spread(expr: &SpannedExpr) -> bool {
    match &expr.node {
        Expr::Number(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Null
        | Expr::BuiltIn(_)
        | Expr::List(_)
        | Expr::Record(_)
        | Expr::Lambda { .. }
        | Expr::UnaryOp { .. }
        | Expr::PostfixOp { .. } => true,
        Expr::BinaryOp { op, .. } => !matches!(
            op,
            BinaryOp::Via | BinaryOp::Into | BinaryOp::Where | BinaryOp::Coalesce
        ),
        _ => false,
    }
}

fn too_complex(what: &str) -> RuntimeError {
    RuntimeError::new(format!("expression is too complex to evaluate: {}", what))
}

/// Compile an expression evaluated directly against an environment (a top-level statement).
pub(crate) fn compile_chunk(expr: &SpannedExpr, inline: bool) -> CResult<Rc<Proto>> {
    let mut compiler = FnCompiler::new(true, Vec::new(), false, inline);

    let mut names = Vec::new();
    collect_chunk_names(expr, false, &mut names);
    for name in names {
        let reg = compiler.alloc()?;
        compiler.globals.push((name, reg));
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
    strings: Vec<String>,
    names: Vec<Symbol>,
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

    /// Chunk only: the cache register of each late-bound name.
    globals: Vec<(Symbol, u16)>,

    /// Chunk only: names whose cache register is known to be loaded at the current point.
    ensured: Vec<Symbol>,

    /// Whether higher-order built-in calls may be compiled as inline loops.
    inline: bool,

    /// Variants only: the capture (name and slot) that is known to hold the function itself.
    self_target: Option<(Symbol, u16)>,
}

impl FnCompiler {
    fn new(is_chunk: bool, params: Vec<Symbol>, simple_params: bool, inline: bool) -> Self {
        Self {
            code: Vec::new(),
            span_index: Vec::new(),
            spans: Vec::new(),
            consts: Vec::new(),
            strings: Vec::new(),
            names: Vec::new(),
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
            ensured: Vec::new(),
            inline,
            self_target: None,
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
        let fast_arity = if self.self_refs.is_empty() && self.max_reg as usize <= FAST_FRAME_SLOTS {
            simple_arity
        } else {
            u32::MAX
        };

        Proto {
            code: self.code,
            consts: self.consts,
            strings: self.strings,
            names: self.names,
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
            cache_regs: self.globals.len(),
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
        self.code.push(number.to_bits());
        self.span_index.push(u32::MAX);
    }

    fn emit_spanned(&mut self, instr: Instr, span: Span) -> usize {
        let index = self.emit(instr);

        // Consecutive instructions often share a span (e.g. the checks of one assignment).
        let span_id = match self.spans.last() {
            Some(last) if *last == span => self.spans.len() - 1,
            _ => {
                self.spans.push(span);
                self.spans.len() - 1
            }
        };
        self.span_index[index] = span_id as u32;

        index
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
        if reg == u16::MAX {
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
        if let Some(index) = self.strings.iter().position(|existing| existing == text) {
            return index as u32;
        }

        self.strings.push(text.to_string());
        (self.strings.len() - 1) as u32
    }

    fn name(&mut self, name: Symbol) -> u32 {
        if let Some(index) = self.names.iter().position(|existing| *existing == name) {
            return index as u32;
        }

        self.names.push(name);
        (self.names.len() - 1) as u32
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

            if !self.is_chunk {
                return match self.params.iter().position(|param| *param == name) {
                    Some(index) => Resolved::Register(index as u16),
                    None => Resolved::Dynamic,
                };
            }

            return match self.globals.iter().find(|(global, _)| *global == name) {
                Some((_, reg)) => Resolved::Global(*reg),
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

    /// Make sure the cache register of a chunk-level name is loaded, raising the usual
    /// "unknown identifier" error at this point of the program otherwise.
    fn ensure_global(&mut self, name: Symbol, reg: u16, span: Span) {
        if self.ensured.contains(&name) {
            return;
        }

        let index = self.name(name);
        self.emit_spanned(Instr::abx(Op::EnsureGlobal, reg, index), span);
        self.ensured.push(name);
    }

    /// Resolve a by-name reference to a register that already holds its value, if there is one.
    fn name_reg(&mut self, name: Symbol, span: Span) -> Option<u16> {
        match self.resolve(name) {
            Resolved::Register(reg) => Some(reg),
            Resolved::Global(reg) => {
                self.ensure_global(name, reg, span);
                Some(reg)
            }
            Resolved::Dynamic => None,
        }
    }

    /// If `expr` is a variable reference whose value already sits in a register, return that
    /// register (emitting at most a cache check).
    fn direct_reg(&mut self, expr: &SpannedExpr) -> Option<u16> {
        match &expr.node {
            Expr::Identifier(name) => {
                if is_constant_name(*name) {
                    return None;
                }

                self.name_reg(*name, expr.span)
            }
            Expr::ParamSlot { name, index } => match self.owner() {
                Owner::Callback(scope) => {
                    match self.scopes[scope].param_regs.get(*index as usize) {
                        Some(reg) => Some(*reg),
                        None => self.name_reg(*name, expr.span),
                    }
                }
                Owner::Function => {
                    if (*index as usize) < self.params.len() {
                        return Some(*index);
                    }

                    self.name_reg(*name, expr.span)
                }
                Owner::Chunk => None,
            },
            Expr::CaptureSlot { name, .. } => match self.owner() {
                // An inlined callback never materialises its captured scope: a capture bound at
                // closure creation holds exactly the value a by-name lookup from the callback
                // body finds, and an unbound one falls back to that same lookup.
                Owner::Callback(_) => self.name_reg(*name, expr.span),
                Owner::Function | Owner::Chunk => None,
            },
            _ => None,
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
            let mut instr = Instr::abc(const_op, dst, left_reg, 0);
            instr.x = last_use_flags(op, left_reg >= mark, false);
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
            let right_reg = self.expr_any(right)?;
            let mut instr = Instr::abc(binary_opcode(op), dst, left_reg, right_reg);
            instr.x = last_use_flags(op, left_reg >= mark, right_reg >= mark);
            self.emit_spanned(instr, span);
        }

        self.free_reg = mark;

        Ok(())
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

        Ok(())
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

        if !has_spread && items.len() <= LIST_BATCH {
            let first = self.free_reg;
            for item in items {
                let reg = self.alloc()?;
                self.expr_into(&item.node, reg)?;
            }

            self.emit(Instr::abc(Op::NewList, dst, first, items.len() as u16));
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

        if all_static && entries.len() <= MAX_SHAPE_KEYS {
            let first = self.free_reg;
            let mut keys = IndexSet::new();
            let mut slots = Vec::with_capacity(entries.len());

            for entry in entries {
                if let RecordKey::Static(key) = &entry.node.key {
                    let (slot, _) = keys.insert_full(key.clone());
                    slots.push(slot as u16);
                }

                let reg = self.alloc()?;
                self.expr_into(&entry.node.value, reg)?;
            }

            let layout =
                u16::try_from(self.layouts.len()).map_err(|_| too_complex("too many records"))?;
            self.layouts.push(RecordLayout {
                distinct: keys.len() == slots.len(),
                shape: Shape::interned(keys),
                slots,
            });
            self.emit(Instr::abc(Op::NewRecord, dst, first, layout));
            self.free_reg = mark;

            return Ok(());
        }

        self.emit(Instr::abc(Op::RecordBegin, 0, 0, 0));

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
                        Resolved::Global(reg) if self.ensured.contains(&name) => reg,
                        Resolved::Global(_) | Resolved::Dynamic => {
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
                Resolved::Global(reg) if self.ensured.contains(name) => {
                    CaptureSource::Register(reg)
                }
                Resolved::Global(_) | Resolved::Dynamic => CaptureSource::Dynamic,
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

        let mark = self.free_reg;
        let candidate = self.self_call_candidate(func, args);

        if candidate.is_some() && candidate == self.self_target {
            // The frame can only be replaced when nothing could tell: a callee resolves the
            // names it does not bind through its caller's variables.
            let replace = tail && !self.has_visible_vars();
            let func_reg = if replace { dst } else { self.alloc()? };

            for arg in args {
                let reg = self.alloc()?;
                self.expr_into(arg, reg)?;
            }

            // The instruction finds the start of the code by its own position. A variant too
            // long for that is rejected as a whole (see `compile_function`), so the truncation
            // never reaches the VM.
            let op = if replace { Op::TailSelf } else { Op::CallSelf };
            let no_spread = args.iter().all(never_spread);
            let mut instr = Instr::abc(op, func_reg, (self.here() + 1) as u16, dst);
            instr.x = args.len() as u8 | (no_spread as u8) << 7;
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

        if args.iter().any(|arg| matches!(arg.node, Expr::Spread(_))) {
            // The argument count is only known at runtime: gather the (flattened) arguments in
            // a list builder and call with its contents.
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
        let arg_count =
            u16::try_from(args.len()).map_err(|_| too_complex("too many call arguments"))?;
        for arg in args {
            let reg = self.alloc()?;
            self.expr_into(arg, reg)?;
        }

        let mut instr = Instr::abc(Op::Call, func_reg, arg_count, dst);
        instr.x = args.iter().all(never_spread) as u8;
        self.emit_spanned(instr, span);
        self.free_reg = mark;

        Ok(())
    }

    /// Compile `map`/`filter`/`reduce`/`every`/`some` over a literal lambda as an inline loop.
    fn inline_loop(&mut self, span: Span, call: &InlineCall, dst: u16) -> CResult<()> {
        let mark = self.free_reg;
        let is_reduce = call.builtin == BuiltInFunction::Reduce;
        let element_params = if is_reduce {
            &call.params[1..]
        } else {
            call.params
        };
        let wants_index = element_params.len() == 2;
        let range_args = range_call(call.list);

        // Loop state registers, laid out the way the iteration instructions expect:
        //   list:  [cursor, list, element, index, end]
        //   range: [next, end, start, element, index]
        let state = self.free_reg;
        for _ in 0..5 {
            self.alloc()?;
        }
        let (element_reg, index_reg) = if range_args.is_some() {
            (state + 3, state + 4)
        } else {
            (state + 2, state + 3)
        };

        if let Some(range_args) = range_args {
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

        if range_args.is_none() {
            self.emit(Instr::abc(Op::IterPrep, state, 0, 0));
        }

        let (entry_op, next_op) = if range_args.is_some() {
            (Op::ForRange, Op::ForRangeNext)
        } else {
            (Op::ForList, Op::ForListNext)
        };

        if matches!(call.builtin, BuiltInFunction::Map | BuiltInFunction::Filter) {
            // Mapping produces exactly one result per element, so the result list can be sized
            // up front; filtering starts empty like the built-in does.
            let mut begin = Instr::abc(Op::ListBegin, state, 0, 0);
            if call.builtin == BuiltInFunction::Map {
                begin.x = if range_args.is_some() { 2 } else { 1 };
            }
            self.emit(begin);
        }

        let mut entry = Instr::abx(entry_op, state, 0);
        entry.x = wants_index as u8;
        let to_exit = self.emit(entry);

        self.emit(Instr::abc(Op::CallbackDepth, 0, 0, 0));
        self.emit(Instr::abx(Op::AddDepth, 0, CALLBACK_DEPTH as u32));

        let loop_start = self.here();
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

        let body_start = self.here();
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
                self.emit(Instr::abc(Op::ListPush, element_reg, 0, 0));
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

        let mut next = Instr::abx(next_op, state, 0);
        next.x = wants_index as u8;
        let back = self.emit(next);
        self.patch_jump(back, loop_start);

        self.emit(Instr::abx(Op::AddDepth, 0, (-CALLBACK_DEPTH) as u32));

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
                self.emit(Instr::abx(Op::AddDepth, 0, (-CALLBACK_DEPTH) as u32));
                self.load_const(dst, Value::Bool(!exhausted));

                self.patch_jump(to_end, self.here());
            }
            _ => unreachable!("not an inlinable built-in"),
        }

        self.handlers.push(Handler {
            start: region_start as u32,
            end: self.here() as u32,
            wraps: vec![
                Wrap::FunctionContext(Rc::from(format!(
                    "built-in function \"{}\"",
                    call.builtin.name()
                ))),
                Wrap::CallSite(span),
            ],
        });

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
