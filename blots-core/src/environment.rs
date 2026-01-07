use crate::values::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
enum LocalBindings {
    Owned(RefCell<HashMap<String, Value>>),
    Shared(Rc<HashMap<String, Value>>),
}

/// A scope chain environment for variable bindings.
///
/// Instead of cloning the entire HashMap on each function call,
/// we create a linked chain of scopes. Lookups walk the chain,
/// but creating a new scope is O(1).
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
            local: LocalBindings::Owned(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    /// Create a new root environment with initial bindings
    pub fn with_bindings(bindings: HashMap<String, Value>) -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(bindings)),
            parent: None,
        }
    }

    /// Create a child environment that inherits from this one
    pub fn extend(parent: Rc<Environment>) -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(HashMap::new())),
            parent: Some(parent),
        }
    }

    /// Create a child environment with initial local bindings
    pub fn extend_with(parent: Rc<Environment>, local: HashMap<String, Value>) -> Self {
        Environment {
            local: LocalBindings::Owned(RefCell::new(local)),
            parent: Some(parent),
        }
    }

    /// Create a child environment with shared local bindings
    pub fn extend_shared(parent: Rc<Environment>, local: Rc<HashMap<String, Value>>) -> Self {
        Environment {
            local: LocalBindings::Shared(local),
            parent: Some(parent),
        }
    }

    /// Look up a variable, walking the scope chain
    pub fn get(&self, key: &str) -> Option<Value> {
        // Check local scope first
        let local_value = match &self.local {
            LocalBindings::Owned(map) => map.borrow().get(key).copied(),
            LocalBindings::Shared(map) => map.get(key).copied(),
        };
        if let Some(value) = local_value {
            return Some(value);
        }
        // Then check parent scope
        if let Some(parent) = &self.parent {
            return parent.get(key);
        }
        None
    }

    /// Insert or update a binding in the local scope
    pub fn insert(&self, key: String, value: Value) {
        match &self.local {
            LocalBindings::Owned(map) => {
                map.borrow_mut().insert(key, value);
            }
            LocalBindings::Shared(_) => {
                panic!("cannot insert into shared environment");
            }
        }
    }

    /// Check if a key exists in any scope
    pub fn contains_key(&self, key: &str) -> bool {
        let contains_local = match &self.local {
            LocalBindings::Owned(map) => map.borrow().contains_key(key),
            LocalBindings::Shared(map) => map.contains_key(key),
        };
        if contains_local {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.contains_key(key);
        }
        false
    }

    /// Check if a key exists in the local scope only
    pub fn contains_key_local(&self, key: &str) -> bool {
        match &self.local {
            LocalBindings::Owned(map) => map.borrow().contains_key(key),
            LocalBindings::Shared(map) => map.contains_key(key),
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
        // This flattens the scope chain into a single HashMap
        Environment {
            local: LocalBindings::Owned(RefCell::new(self.flatten())),
            parent: None,
        }
    }
}

impl Environment {
    /// Flatten the scope chain into a single HashMap
    /// Used when capturing variables for closures or serialization
    pub fn flatten(&self) -> HashMap<String, Value> {
        let mut result = HashMap::new();
        self.flatten_into(&mut result);
        result
    }

    fn flatten_into(&self, result: &mut HashMap<String, Value>) {
        // First add parent bindings (so local can override)
        if let Some(parent) = &self.parent {
            parent.flatten_into(result);
        }
        // Then add local bindings
        match &self.local {
            LocalBindings::Owned(map) => {
                for (key, value) in map.borrow().iter() {
                    result.insert(key.clone(), *value);
                }
            }
            LocalBindings::Shared(map) => {
                for (key, value) in map.iter() {
                    result.insert(key.clone(), *value);
                }
            }
        }
    }

    /// Iterate over all bindings (flattened)
    /// Useful for serialization
    pub fn iter(&self) -> impl Iterator<Item = (String, Value)> {
        self.flatten().into_iter()
    }

    /// Get all keys (flattened)
    /// Useful for completion
    pub fn keys(&self) -> impl Iterator<Item = String> {
        self.flatten().into_keys()
    }
}
