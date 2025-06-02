// Blots JavaScript Runtime Library
// Built-in functions for transpiled Blots code

// Use $$ prefix to avoid conflicts with user functions
function $$map(collection, fn) {
    if (Array.isArray(collection)) {
        return collection.map(fn);
    }
    if (typeof collection === 'object' && collection !== null) {
        const result = {};
        for (const [key, value] of Object.entries(collection)) {
            result[key] = fn(value, key);
        }
        return result;
    }
    throw new Error(`Cannot map over ${typeof collection}`);
}

function $$filter(collection, predicate) {
    if (Array.isArray(collection)) {
        return collection.filter(predicate);
    }
    if (typeof collection === 'object' && collection !== null) {
        const result = {};
        for (const [key, value] of Object.entries(collection)) {
            if (predicate(value, key)) {
                result[key] = value;
            }
        }
        return result;
    }
    throw new Error(`Cannot filter ${typeof collection}`);
}

function $$collect(iterable) {
    if (Array.isArray(iterable)) {
        return iterable;
    }
    if (typeof iterable[Symbol.iterator] === 'function') {
        return Array.from(iterable);
    }
    throw new Error(`Cannot collect ${typeof iterable}`);
}

function $$reduce(collection, reducer, initialValue) {
    if (Array.isArray(collection)) {
        if (arguments.length === 2) {
            // No initial value provided
            return collection.reduce(reducer);
        } else {
            // Initial value provided
            return collection.reduce(reducer, initialValue);
        }
    }
    throw new Error(`Cannot reduce ${typeof collection}`);
}

function $$each(collection) {
    if (Array.isArray(collection)) {
        // Return a proxy that supports property access for each element
        return new Proxy(collection, {
            get(target, prop) {
                // Handle special properties and methods
                if (prop === Symbol.iterator) {
                    return target[Symbol.iterator].bind(target);
                }
                if (prop === 'valueOf' || prop === 'toString') {
                    return target[prop].bind(target);
                }
                if (typeof prop === 'string' && !isNaN(prop)) {
                    // Numeric index access
                    return target[prop];
                }
                if (prop in target && typeof target[prop] !== 'undefined') {
                    // Array methods and properties
                    const val = target[prop];
                    return typeof val === 'function' ? val.bind(target) : val;
                }
                // Property access on each element
                return target.map(item => item && item[prop]);
            }
        });
    }
    if (typeof collection === 'string') {
        return Array.from(collection);
    }
    if (typeof collection === 'object' && collection !== null) {
        return Object.entries(collection);
    }
    throw new Error(`Cannot iterate over ${typeof collection}`);
}

function $$reverse(collection) {
    if (Array.isArray(collection)) {
        return [...collection].reverse();
    }
    throw new Error(`Cannot reverse ${typeof collection}`);
}

function $$len(collection) {
    if (Array.isArray(collection) || typeof collection === 'string') {
        return collection.length;
    }
    if (typeof collection === 'object' && collection !== null) {
        return Object.keys(collection).length;
    }
    return 0;
}

function $$head(collection) {
    if (Array.isArray(collection)) {
        return collection[0];
    }
    if (typeof collection === 'string') {
        return collection[0];
    }
    throw new Error(`Cannot get head of ${typeof collection}`);
}

function $$tail(collection) {
    if (Array.isArray(collection)) {
        return collection.slice(1);
    }
    if (typeof collection === 'string') {
        return collection.slice(1);
    }
    throw new Error(`Cannot get tail of ${typeof collection}`);
}

function $$concat(...collections) {
    if (collections.every(c => Array.isArray(c))) {
        return collections.flat();
    }
    if (collections.every(c => typeof c === 'string')) {
        return collections.join('');
    }
    throw new Error('Cannot concat mixed types');
}

function $$sum(...values) {
    return values.flat().reduce((a, b) => a + b, 0);
}

function $$range(n) {
    if (typeof n !== 'number') {
        throw new Error('Range expects a number');
    }
    if (n < 1) {
        throw new Error('Range expects a non-zero positive number');
    }

    const arr = new Array(n);
    for (let i = 0; i < n; i++) {
        arr[i] = i;
    }

    return arr;
}

// Math functions
function $$abs(x) { return Math.abs(x); }
function $$floor(x) { return Math.floor(x); }
function $$ceil(x) { return Math.ceil(x); }
function $$round(x) { return Math.round(x); }
function $$sqrt(x) { return Math.sqrt(x); }
function $$sin(x) { return Math.sin(x); }
function $$cos(x) { return Math.cos(x); }
function $$tan(x) { return Math.tan(x); }
function $$asin(x) { return Math.asin(x); }
function $$acos(x) { return Math.acos(x); }
function $$atan(x) { return Math.atan(x); }
function $$exp(x) { return Math.exp(x); }
function $$ln(x) { return Math.log(x); }
function $$log10(x) { return Math.log10(x); }

function $$factorial(n) {
    if (n < 0) return NaN;
    if (n === 0 || n === 1) return 1;
    let result = 1;
    for (let i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}

// String operations
function $$split(str, delimiter) {
    return str.split(delimiter);
}

function $$join(arr, delimiter) {
    return arr.join(delimiter);
}

function $$to_uppercase(str) {
    return str.toUpperCase();
}

function $$to_lowercase(str) {
    return str.toLowerCase();
}

function $$to_string(value) {
    return String(value);
}

// Array/collection utility functions
function $$includes(collection, searchElement) {
    if (Array.isArray(collection)) {
        return collection.includes(searchElement);
    }
    if (typeof collection === 'string') {
        return collection.includes(searchElement);
    }
    throw new Error(`Cannot check includes on ${typeof collection}`);
}

function $$sort_by(collection, compareFn) {
    if (Array.isArray(collection)) {
        return [...collection].sort(compareFn);
    }
    throw new Error(`Cannot sort ${typeof collection}`);
}

// Statistical functions
function $$avg(...values) {
    const nums = values.flat().filter(v => typeof v === 'number');
    if (nums.length === 0) return 0;
    return nums.reduce((a, b) => a + b, 0) / nums.length;
}

function $$median(...values) {
    const nums = values.flat().filter(v => typeof v === 'number').sort((a, b) => a - b);
    if (nums.length === 0) return 0;
    const mid = Math.floor(nums.length / 2);
    return nums.length % 2 === 0 ? (nums[mid - 1] + nums[mid]) / 2 : nums[mid];
}

function $$percentile(collection, p) {
    if (!Array.isArray(collection)) {
        throw new Error(`Cannot calculate percentile of ${typeof collection}`);
    }
    const nums = collection.filter(v => typeof v === 'number').sort((a, b) => a - b);
    if (nums.length === 0) return 0;
    const index = (p / 100) * (nums.length - 1);
    if (Number.isInteger(index)) {
        return nums[index];
    } else {
        const lower = Math.floor(index);
        const upper = Math.ceil(index);
        const weight = index - lower;
        return nums[lower] * (1 - weight) + nums[upper] * weight;
    }
}

// Time functions
function $$time_now() {
    return Date.now() / 1000; // Return seconds like Unix timestamp
}

// Utility functions
function $$not(value) {
    return !value;
}

function $$eq(a, b) {
    return a === b;
}

// Type checking
function $$is_string(value) {
    return typeof value === 'string';
}

function $$is_number(value) {
    return typeof value === 'number';
}

function $$is_bool(value) {
    return typeof value === 'boolean';
}

function $$is_list(value) {
    return Array.isArray(value);
}

function $$is_null(value) {
    return value === null || value === undefined;
}

// Spread operator helper for arrays
function $$spreadToArray(value) {
    if (Array.isArray(value)) {
        return value;
    }
    if (typeof value === 'string') {
        return Array.from(value);
    }
    if (typeof value === 'object' && value !== null) {
        // Convert record to [key, value] pairs, sorted alphabetically to match Rust BTreeMap
        return Object.keys(value).sort().map(key => [key, value[key]]);
    }
    throw new Error(`Cannot spread ${typeof value} into array`);
}

// Each arithmetic operations - element-wise operations
function $$eachAdd(left, right) {
    const leftVals = Array.isArray(left) ? left : [left];
    const rightVals = Array.isArray(right) ? right : [right];
    
    if (Array.isArray(left) && Array.isArray(right) && left.length !== right.length) {
        throw new Error('left- and right-hand-side iterables must be the same length');
    }
    
    const maxLen = Math.max(leftVals.length, rightVals.length);
    const result = [];
    
    for (let i = 0; i < maxLen; i++) {
        const l = leftVals[i] || leftVals[0];
        const r = rightVals[i] || rightVals[0];
        result.push(l + r);
    }
    
    return result;
}

function $$eachSubtract(left, right) {
    const leftVals = Array.isArray(left) ? left : [left];
    const rightVals = Array.isArray(right) ? right : [right];
    
    if (Array.isArray(left) && Array.isArray(right) && left.length !== right.length) {
        throw new Error('left- and right-hand-side iterables must be the same length');
    }
    
    const maxLen = Math.max(leftVals.length, rightVals.length);
    const result = [];
    
    for (let i = 0; i < maxLen; i++) {
        const l = leftVals[i] || leftVals[0];
        const r = rightVals[i] || rightVals[0];
        result.push(l - r);
    }
    
    return result;
}

function $$eachMultiply(left, right) {
    const leftVals = Array.isArray(left) ? left : [left];
    const rightVals = Array.isArray(right) ? right : [right];
    
    if (Array.isArray(left) && Array.isArray(right) && left.length !== right.length) {
        throw new Error('left- and right-hand-side iterables must be the same length');
    }
    
    const maxLen = Math.max(leftVals.length, rightVals.length);
    const result = [];
    
    for (let i = 0; i < maxLen; i++) {
        const l = leftVals[i] || leftVals[0];
        const r = rightVals[i] || rightVals[0];
        result.push(l * r);
    }
    
    return result;
}

function $$eachDivide(left, right) {
    const leftVals = Array.isArray(left) ? left : [left];
    const rightVals = Array.isArray(right) ? right : [right];
    
    if (Array.isArray(left) && Array.isArray(right) && left.length !== right.length) {
        throw new Error('left- and right-hand-side iterables must be the same length');
    }
    
    const maxLen = Math.max(leftVals.length, rightVals.length);
    const result = [];
    
    for (let i = 0; i < maxLen; i++) {
        const l = leftVals[i] || leftVals[0];
        const r = rightVals[i] || rightVals[0];
        result.push(l / r);
    }
    
    return result;
}

// Constants object matching the Rust CONSTANTS
const $$constants = {
    pi: Math.PI,
    e: Math.E,
    infinity: Infinity,
    inf: Infinity,
    max: Number.MAX_VALUE,
    min: Number.MIN_VALUE
};

// Create global aliases for built-in functions, but only if they don't already exist
// This allows user-defined functions to override built-ins
const $$builtins = {
    map: $$map, filter: $$filter, reduce: $$reduce, collect: $$collect, each: $$each, reverse: $$reverse, 
    len: $$len, head: $$head, tail: $$tail, concat: $$concat, sum: $$sum, range: $$range,
    abs: $$abs, floor: $$floor, ceil: $$ceil, round: $$round, sqrt: $$sqrt, 
    sin: $$sin, cos: $$cos, tan: $$tan, asin: $$asin, acos: $$acos, atan: $$atan,
    exp: $$exp, ln: $$ln, log10: $$log10, factorial: $$factorial,
    split: $$split, join: $$join, to_uppercase: $$to_uppercase, to_lowercase: $$to_lowercase, to_string: $$to_string,
    includes: $$includes, sort_by: $$sort_by, avg: $$avg, median: $$median, percentile: $$percentile,
    time_now: $$time_now, not: $$not, eq: $$eq,
    is_string: $$is_string, is_number: $$is_number, is_bool: $$is_bool, is_list: $$is_list, is_null: $$is_null,
    constants: $$constants
};

// Set up aliases at the end of execution
setTimeout(() => {
    for (const [name, func] of Object.entries($$builtins)) {
        if (typeof globalThis[name] === 'undefined') {
            globalThis[name] = func;
        }
    }
}, 0);

// Immediately make them available for function declarations
for (const [name, func] of Object.entries($$builtins)) {
    if (typeof globalThis[name] === 'undefined') {
        globalThis[name] = func;
    }
}

// Set up inputs variable - it will be populated by the WASM evaluate function
// Provide a default empty inputs object if not set by WASM
if (typeof globalThis.inputs === 'undefined') {
    globalThis.inputs = {};
}

// Make inputs available as a bare identifier (not just globalThis.inputs)
if (typeof inputs === 'undefined') {
    var inputs = globalThis.inputs;
}