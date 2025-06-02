// Blots JavaScript Runtime Library
// Built-in functions for transpiled Blots code

// Collection operations
function map(collection, fn) {
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

function filter(predicate, collection) {
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

function collect(iterable) {
    if (Array.isArray(iterable)) {
        return iterable;
    }
    if (typeof iterable[Symbol.iterator] === 'function') {
        return Array.from(iterable);
    }
    throw new Error(`Cannot collect ${typeof iterable}`);
}

function each(collection) {
    if (Array.isArray(collection)) {
        return collection;
    }
    if (typeof collection === 'object' && collection !== null) {
        return Object.entries(collection);
    }
    throw new Error(`Cannot iterate over ${typeof collection}`);
}

function reverse(collection) {
    if (Array.isArray(collection)) {
        return [...collection].reverse();
    }
    throw new Error(`Cannot reverse ${typeof collection}`);
}

function len(collection) {
    if (Array.isArray(collection) || typeof collection === 'string') {
        return collection.length;
    }
    if (typeof collection === 'object' && collection !== null) {
        return Object.keys(collection).length;
    }
    return 0;
}

function head(collection) {
    if (Array.isArray(collection)) {
        return collection[0];
    }
    if (typeof collection === 'string') {
        return collection[0];
    }
    throw new Error(`Cannot get head of ${typeof collection}`);
}

function tail(collection) {
    if (Array.isArray(collection)) {
        return collection.slice(1);
    }
    if (typeof collection === 'string') {
        return collection.slice(1);
    }
    throw new Error(`Cannot get tail of ${typeof collection}`);
}

function concat(...collections) {
    if (collections.every(c => Array.isArray(c))) {
        return collections.flat();
    }
    if (collections.every(c => typeof c === 'string')) {
        return collections.join('');
    }
    throw new Error('Cannot concat mixed types');
}

// Numeric operations
function sum(...values) {
    return values.flat().reduce((a, b) => a + b, 0);
}

function range(n) {
    if (typeof n !== 'number') {
        throw new Error('Range expects a number');
    }
    return Array.from({ length: Math.max(0, Math.floor(n)) }, (_, i) => i);
}

// Math functions
function abs(x) { return Math.abs(x); }
function floor(x) { return Math.floor(x); }
function ceil(x) { return Math.ceil(x); }
function round(x) { return Math.round(x); }
function sqrt(x) { return Math.sqrt(x); }
function sin(x) { return Math.sin(x); }
function cos(x) { return Math.cos(x); }
function tan(x) { return Math.tan(x); }
function asin(x) { return Math.asin(x); }
function acos(x) { return Math.acos(x); }
function atan(x) { return Math.atan(x); }
function exp(x) { return Math.exp(x); }
function ln(x) { return Math.log(x); }
function log10(x) { return Math.log10(x); }

function factorial(n) {
    if (n < 0) return NaN;
    if (n === 0 || n === 1) return 1;
    let result = 1;
    for (let i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}

// String operations
function split(str, delimiter) {
    return str.split(delimiter);
}

function join(arr, delimiter) {
    return arr.join(delimiter);
}

function to_uppercase(str) {
    return str.toUpperCase();
}

function to_lowercase(str) {
    return str.toLowerCase();
}

// Time functions
function time_now() {
    return Date.now() / 1000; // Return seconds like Unix timestamp
}

// Utility functions
function not(value) {
    return !value;
}

function eq(a, b) {
    return a === b;
}

// Type checking
function is_string(value) {
    return typeof value === 'string';
}

function is_number(value) {
    return typeof value === 'number';
}

function is_bool(value) {
    return typeof value === 'boolean';
}

function is_list(value) {
    return Array.isArray(value);
}

function is_null(value) {
    return value === null || value === undefined;
}

// Make functions available globally if in browser environment
if (typeof window !== 'undefined') {
    Object.assign(window, {
        map, filter, collect, each, reverse, len, head, tail, concat,
        sum, range, abs, floor, ceil, round, sqrt, sin, cos, tan,
        asin, acos, atan, exp, ln, log10, factorial,
        split, join, to_uppercase, to_lowercase, time_now,
        not, eq, is_string, is_number, is_bool, is_list, is_null
    });
}

// Export for Node.js environment
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        map, filter, collect, each, reverse, len, head, tail, concat,
        sum, range, abs, floor, ceil, round, sqrt, sin, cos, tan,
        asin, acos, atan, exp, ln, log10, factorial,
        split, join, to_uppercase, to_lowercase, time_now,
        not, eq, is_string, is_number, is_bool, is_list, is_null
    };
}
const numbers = [1, 2, 3, 4, 5]
const doubled = map(numbers, (x) => x * 2)
const evens = filter((x) => x % 2 === 0, numbers)
const result = {original: numbers, doubled: doubled, evens: evens, sum: sum(...numbers), length: len(numbers), reversed: reverse(numbers)}
console.log(`Numbers: ${numbers}`)
console.log(`Doubled: ${doubled}`)
console.log(`Sum: ${sum(...numbers)}`)
