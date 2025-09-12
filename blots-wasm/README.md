# blots-wasm

This crate provides a WASM runtime for Blots – a small, simple, expression-oriented programming language.

```rust
pub fn evaluate(expr: &str, inputs_js: JsValue) -> Result<JsValue, JsError>;
pub fn tokenize(input: &str) -> Result<JsValue, JsError>;
pub fn get_built_in_function_names() -> Result<JsValue, JsError>;
pub fn get_constants() -> Result<JsValue, JsError>;
pub fn evaluate_inline_expressions(expressions_js: JsValue, inputs_js: JsValue,) -> Result<JsValue, JsError>;
```

## Note
If you're viewing this crate on crates.io but are looking for the ready-to-use, pre-compiled WASM/JS version, see [blots-wasm on npm](https://www.npmjs.com/package/blots-wasm).