use crate::intern::{Symbol, SymbolMap};
use crate::values::{CapturedScope, LambdaArg, Value};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
enum LocalBindings {
    /// A general-purpose scope that owns its bindings (the root document scope, do-block
    /// scopes, and embedder-provided binding sets).
    Owned(RefCell<SymbolMap<Value>>),

    /// A lambda call frame: argument values stored positionally (matching the lambda's
    /// parameter order), the optional self-binding for named recursion, and the closure's
    /// captured scope. Name lookups use linear scans -- functions have a handful of
    /// parameters, so this avoids hashing and per-call map allocation entirely.
    Frame {
        params: Rc<Vec<LambdaArg>>,
        self_binding: Option<(Symbol, Value)>,
        values: Vec<Value>,
        captures: CapturedScope,

        /// Bindings created by assignments evaluated directly against the frame (do-blocks get
        /// their own scope, so this is a cold path that stays empty for ordinary calls).
        spill: RefCell<SymbolMap<Value>>,
    },
}

/// A scope chain environment for variable bindings.
///
/// Instead of cloning the entire map on each function call,
/// we create a linked chain of scopes. Lookups walk the chain,
/// but creating a new scope is O(1).
///
/// Bindings are keyed by interned [`Symbol`]s, so lookups and inserts never hash or allocate
/// strings.
#[derive(Debug)]
pub struct Environment {
    /// Local bindings in this scope
    local: LocalBindings,
    /// Parent scope (if any)
    parent: Option<Rc<Environment>>,
}

impl Environment {
    /// Create a new root environment with no parent
    pub fn new() -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(SymbolMap::default())),
            parent: None,
        }
    }

    /// Create a new root environment with initial bindings
    pub fn with_bindings(bindings: SymbolMap<Value>) -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(bindings)),
            parent: None,
        }
    }

    /// Create a child environment that inherits from this one
    pub fn extend(parent: Rc<Environment>) -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(SymbolMap::default())),
            parent: Some(parent),
        }
    }

    /// Create a lambda call frame as a child of the call-site environment.
    ///
    /// `values` holds one entry per parameter, in declaration order, so resolved `ParamSlot`
    /// references can read arguments by index while name-based lookups scan `params`.
    pub fn extend_frame(
        parent: Rc<Environment>,
        params: Rc<Vec<LambdaArg>>,
        self_binding: Option<(Symbol, Value)>,
        values: Vec<Value>,
        captures: CapturedScope,
    ) -> Self {
        Environment {
            local: LocalBindings::Frame {
                params,
                self_binding,
                values,
                captures,
                spill: RefCell::new(SymbolMap::default()),
            },
            parent: Some(parent),
        }
    }

    /// Look up a variable, walking the scope chain
    pub fn get(&self, key: Symbol) -> Option<Value> {
        if let Some(value) = self.get_local(key) {
            return Some(value);
        }
        // Then check parent scope
        if let Some(parent) = &self.parent {
            return parent.get(key);
        }
        None
    }

    /// Look up a variable in this scope only (no parent traversal).
    fn get_local(&self, key: Symbol) -> Option<Value> {
        match &self.local {
            LocalBindings::Owned(map) => map.borrow().get(&key).copied(),
            LocalBindings::Frame {
                params,
                self_binding,
                values,
                captures,
                spill,
            } => {
                // Parameters take precedence over the self-binding and captures, mirroring the
                // insertion order of the per-call map this frame replaces.
                if let Some(index) = params.iter().position(|param| param.get_name() == key) {
                    return values.get(index).copied();
                }
                if let Some((name, value)) = self_binding
                    && *name == key
                {
                    return Some(*value);
                }
                if let Some(value) = captures.get(key) {
                    return Some(value);
                }
                spill.borrow().get(&key).copied()
            }
        }
    }

    /// Read a parameter of the nearest enclosing lambda call frame by its positional slot.
    ///
    /// Resolved `ParamSlot` references only evaluate inside their own lambda's call, where the
    /// nearest frame on the chain is always that lambda's own frame; do-block scopes sit below
    /// it and the call-site environment sits above it.
    pub fn get_param_slot(&self, index: usize) -> Option<Value> {
        let mut env = self;
        loop {
            if let LocalBindings::Frame { values, .. } = &env.local {
                return values.get(index).copied();
            }
            env = env.parent.as_deref()?;
        }
    }

    /// Read a captured value of the nearest enclosing lambda call frame by its slot index.
    ///
    /// Returns `None` when the slot was unbound at closure creation (late-bound names) or when
    /// no frame is on the chain (e.g. during tests that evaluate bodies directly); callers fall
    /// back to a by-name lookup in both cases.
    pub fn get_capture_slot(&self, index: usize) -> Option<Value> {
        let mut env = self;
        loop {
            if let LocalBindings::Frame { captures, .. } = &env.local {
                return captures.get_slot(index);
            }
            env = env.parent.as_deref()?;
        }
    }

    /// Insert or update a binding in the local scope
    pub fn insert(&self, key: Symbol, value: Value) {
        match &self.local {
            LocalBindings::Owned(map) => {
                map.borrow_mut().insert(key, value);
            }
            LocalBindings::Frame { spill, .. } => {
                // Assignments evaluated directly against a call frame (outside any do-block)
                // spill into the side map rather than the positional argument slots.
                spill.borrow_mut().insert(key, value);
            }
        }
    }

    /// Check if a key exists in any scope
    pub fn contains_key(&self, key: Symbol) -> bool {
        if self.contains_key_local(key) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.contains_key(key);
        }
        false
    }

    /// Check if a key exists in the local scope only
    pub fn contains_key_local(&self, key: Symbol) -> bool {
        match &self.local {
            LocalBindings::Owned(map) => map.borrow().contains_key(&key),
            LocalBindings::Frame {
                params,
                self_binding,
                captures,
                spill,
                ..
            } => {
                params.iter().any(|param| param.get_name() == key)
                    || self_binding.as_ref().is_some_and(|(name, _)| *name == key)
                    || captures.contains_key(key)
                    || spill.borrow().contains_key(&key)
            }
        }
    }

    /// Rewrite every binding value in this scope chain in place.
    ///
    /// Used by heap compaction to remap heap pointers held by the root environment. Every scope
    /// in the chain must own its bindings; call frames only exist inside in-progress lambda
    /// calls, which never span a compaction point.
    pub fn rewrite_binding_values(&self, rewrite: &mut impl FnMut(Value) -> Value) {
        match &self.local {
            LocalBindings::Owned(map) => {
                for value in map.borrow_mut().values_mut() {
                    *value = rewrite(*value);
                }
            }
            LocalBindings::Frame { .. } => {
                panic!("cannot rewrite bindings in a lambda call frame");
            }
        }

        if let Some(parent) = &self.parent {
            parent.rewrite_binding_values(rewrite);
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        // For closures that capture scope, we need to clone
        // This flattens the scope chain into a single map
        Environment {
            local: LocalBindings::Owned(RefCell::new(self.flatten())),
            parent: None,
        }
    }
}

impl Environment {
    /// Flatten the scope chain into a single map
    /// Used when capturing variables for closures or serialization
    pub fn flatten(&self) -> SymbolMap<Value> {
        let mut result = SymbolMap::default();
        self.flatten_into(&mut result);
        result
    }

    fn flatten_into(&self, result: &mut SymbolMap<Value>) {
        // First add parent bindings (so local can override)
        if let Some(parent) = &self.parent {
            parent.flatten_into(result);
        }

        // Then add local bindings, lowest precedence first so that higher-precedence names
        // (parameters over the self-binding over captures) overwrite as they are inserted.
        match &self.local {
            LocalBindings::Owned(map) => {
                for (key, value) in map.borrow().iter() {
                    result.insert(*key, *value);
                }
            }
            LocalBindings::Frame {
                params,
                self_binding,
                values,
                captures,
                spill,
            } => {
                for (key, value) in captures.iter() {
                    result.insert(*key, *value);
                }
                if let Some((name, value)) = self_binding {
                    result.insert(*name, *value);
                }
                for (param, value) in params.iter().zip(values.iter()) {
                    result.insert(param.get_name(), *value);
                }
                for (key, value) in spill.borrow().iter() {
                    result.insert(*key, *value);
                }
            }
        }
    }

    /// Iterate over all bindings (flattened)
    /// Useful for serialization
    pub fn iter(&self) -> impl Iterator<Item = (Symbol, Value)> {
        self.flatten().into_iter()
    }

    /// Get all keys (flattened)
    /// Useful for completion
    pub fn keys(&self) -> impl Iterator<Item = Symbol> {
        self.flatten().into_keys()
    }
}
