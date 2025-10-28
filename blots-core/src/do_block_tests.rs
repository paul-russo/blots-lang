// Tests for do block shadowing behavior
#[cfg(test)]
mod do_block_shadowing_tests {
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    fn parse_and_evaluate(
        code: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<RefCell<HashMap<String, Value>>>>,
    ) -> Result<Value, anyhow::Error> {
        let pairs = get_pairs(code)?;

        let heap = heap.unwrap_or_else(|| Rc::new(RefCell::new(Heap::new())));
        let bindings = bindings.unwrap_or_else(|| Rc::new(RefCell::new(HashMap::new())));

        let mut result = Value::Null;
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    if let Some(inner_pair) = pair.into_inner().next() {
                        match inner_pair.as_rule() {
                            Rule::expression | Rule::assignment => {
                                result = evaluate_pairs(
                                    inner_pair.into_inner(),
                                    Rc::clone(&heap),
                                    Rc::clone(&bindings),
                                    0,
                                    code,
                                )?;
                            }
                            _ => {}
                        }
                    }
                }
                Rule::EOI => {}
                _ => {}
            }
        }
        Ok(result)
    }

    #[test]
    fn test_do_block_allows_shadowing() {
        // Test that do blocks can shadow outer variables
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define outer g = 25
        let _ = parse_and_evaluate("g = 25", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Define function that shadows g in do block
        let _ = parse_and_evaluate(
            "make_f = x => do { g = y => x * y; return g }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call make_f(5) and then call the result with 3
        let f = parse_and_evaluate(
            "make_f(5)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // The function should work (g was successfully shadowed)
        assert!(matches!(f, Value::Lambda(_)));

        // Now call the returned function with 3
        let result = parse_and_evaluate("make_f(5)(3)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(15.0)); // 5 * 3 = 15
    }

    #[test]
    fn test_do_block_shadowing_preserves_outer() {
        // Test that shadowing in do block doesn't affect outer variable
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define outer x = 10
        let _ = parse_and_evaluate("x = 10", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Do block that shadows x
        let _ = parse_and_evaluate(
            "result = do { x = 20; return x }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Outer x should still be 10
        let outer_x =
            parse_and_evaluate("x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(outer_x, Value::Number(10.0));

        // Result should be 20 (from the do block)
        let result = parse_and_evaluate("result", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(20.0));
    }

    #[test]
    fn test_nested_do_blocks_shadowing() {
        // Test that nested do blocks can each shadow variables
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define outer z = 1
        let _ = parse_and_evaluate("z = 1", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Nested do blocks with different z values
        let result = parse_and_evaluate(
            "do { z = 10; return do { z = 100; return z } }",
            Some(heap),
            Some(bindings),
        )
        .unwrap();

        assert_eq!(result, Value::Number(100.0));
    }

    #[test]
    fn test_do_block_multiple_shadowings() {
        // Test that do blocks can shadow multiple variables
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define outer a and b
        let _ = parse_and_evaluate("a = 1", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();
        let _ = parse_and_evaluate("b = 2", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Do block that shadows both
        let result = parse_and_evaluate(
            "do { a = 10; b = 20; return a + b }",
            Some(heap),
            Some(bindings),
        )
        .unwrap();

        assert_eq!(result, Value::Number(30.0)); // 10 + 20
    }

    #[test]
    fn test_do_block_function_capture_with_shadowing() {
        // Test that functions defined in do blocks capture the shadowed value
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define outer v = 5
        let _ = parse_and_evaluate("v = 5", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Do block that shadows v and returns a function capturing it
        let _ = parse_and_evaluate(
            "get_func = do { v = 100; f = () => v; return f }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call the function - should return the shadowed value
        let result = parse_and_evaluate("get_func()", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(100.0)); // Captured the shadowed v
    }
}
