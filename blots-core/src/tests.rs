// Tests for late binding in function calls
#[cfg(test)]
mod late_binding_tests {
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
    fn test_late_binding_simple() {
        // Test that functions can reference variables defined after the function
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define function that references g
        let _ = parse_and_evaluate(
            "f = x => g(x)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define g after f
        let _ = parse_and_evaluate(
            "g = x => x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call f(5) should work and return 10
        let result = parse_and_evaluate("f(5)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn test_late_binding_with_closure() {
        // Test that captured variables take precedence over late binding
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define y = 10
        let _ = parse_and_evaluate("y = 10", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Define function that captures y
        let _ = parse_and_evaluate(
            "f = x => x + y",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Create a new scope with y2 = 20
        let _ = parse_and_evaluate(
            "y2 = 20",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call f(5) should use captured y=10
        let result = parse_and_evaluate("f(5)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(15.0)); // 5 + 10
    }

    #[test]
    fn test_late_binding_chain() {
        // Test chained late binding
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define f that calls g
        let _ = parse_and_evaluate(
            "f = x => g(x) + 1",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define g that calls h
        let _ = parse_and_evaluate(
            "g = x => h(x) * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define h
        let _ = parse_and_evaluate(
            "h = x => x + 3",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call f(5) should work: ((5 + 3) * 2) + 1 = 17
        let result = parse_and_evaluate("f(5)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(17.0));
    }

    #[test]
    fn test_late_binding_with_recursion() {
        // Test that recursion still works with late binding
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define factorial
        let _ = parse_and_evaluate(
            "factorial = n => if n <= 1 then 1 else n * factorial(n - 1)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call factorial(5)
        let result = parse_and_evaluate("factorial(5)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(120.0));
    }

    #[test]
    fn test_late_binding_undefined_still_fails() {
        // Test that truly undefined variables still cause errors
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define function that references undefined h
        let _ = parse_and_evaluate(
            "f = x => h(x)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Calling f should fail because h is never defined
        let result = parse_and_evaluate("f(5)", Some(heap), Some(bindings));

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("unknown identifier: h")
        );
    }

    #[test]
    fn test_late_binding_with_redefinition() {
        // Test that late binding uses the current definition
        // Since Blots doesn't allow reassignment, we'll test with nested scopes
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define f that calls g
        let _ = parse_and_evaluate(
            "f = x => g(x)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define g
        let _ = parse_and_evaluate(
            "g = x => x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call f(5) - should return 10
        let result1 =
            parse_and_evaluate("f(5)", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(result1, Value::Number(10.0));

        // Test with a different g in a new context
        let new_heap = Rc::new(RefCell::new(Heap::new()));
        let new_bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define f that calls g in new context
        let _ = parse_and_evaluate(
            "f = x => g(x)",
            Some(Rc::clone(&new_heap)),
            Some(Rc::clone(&new_bindings)),
        )
        .unwrap();

        // Define g with different behavior
        let _ = parse_and_evaluate(
            "g = x => x * 3",
            Some(Rc::clone(&new_heap)),
            Some(Rc::clone(&new_bindings)),
        )
        .unwrap();

        // Call f(5) - should return 15 with new g
        let result2 = parse_and_evaluate("f(5)", Some(new_heap), Some(new_bindings)).unwrap();
        assert_eq!(result2, Value::Number(15.0));
    }

    #[test]
    fn test_argument_shadows_late_binding() {
        // Test that function arguments shadow late-bound variables
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Define g = 100
        let _ = parse_and_evaluate(
            "g = 100",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define function with parameter g that shadows outer g
        let _ = parse_and_evaluate(
            "f = g => g * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call f(5) - should use parameter g=5, not outer g=100
        let result = parse_and_evaluate("f(5)", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result, Value::Number(10.0));
    }
}
