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

// Tests for input reference syntax (#field)
#[cfg(test)]
mod input_reference_tests {
    use crate::expressions::evaluate_pairs;
    use crate::heap::{Heap, HeapPointer};
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use indexmap::IndexMap;
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

    fn setup_inputs(heap: &Rc<RefCell<Heap>>, bindings: &Rc<RefCell<HashMap<String, Value>>>) {
        // Create an inputs record with test data
        let mut inputs_map = IndexMap::new();
        inputs_map.insert("x".to_string(), Value::Number(42.0));
        inputs_map.insert("y".to_string(), Value::Number(10.0));
        inputs_map.insert(
            "name".to_string(),
            heap.borrow_mut().insert_string("Alice".to_string()),
        );

        let inputs_value = heap.borrow_mut().insert_record(inputs_map);
        bindings
            .borrow_mut()
            .insert("inputs".to_string(), inputs_value);
    }

    #[test]
    fn test_input_reference_simple() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_input_reference_string() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#name", Some(Rc::clone(&heap)), Some(bindings)).unwrap();

        if let Value::String(s) = result {
            let borrowed_heap = heap.borrow();
            let name = s.reify(&borrowed_heap).as_string().unwrap();
            assert_eq!(name, "Alice");
        } else {
            panic!("Expected string value");
        }
    }

    #[test]
    fn test_input_reference_in_expression() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x + #y", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(52.0));
    }

    #[test]
    fn test_input_reference_equivalence() {
        // Test that #field is equivalent to inputs.field
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result1 =
            parse_and_evaluate("#x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        let result2 = parse_and_evaluate("inputs.x", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result1, result2);
    }

    #[test]
    fn test_input_reference_in_function() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        // Define a function that uses input reference
        parse_and_evaluate(
            "double_x = () => #x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result = parse_and_evaluate("double_x()", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(84.0));
    }

    #[test]
    fn test_input_reference_multiple_uses() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x + #x * #y", Some(heap), Some(bindings)).unwrap();
        // 42 + (42 * 10) = 42 + 420 = 462
        assert_eq!(result, Value::Number(462.0));
    }

    #[test]
    fn test_input_reference_missing_field() {
        // Missing fields should return null, consistent with inputs.field behavior
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#missing", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_input_reference_no_inputs() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        // Don't setup inputs

        let result = parse_and_evaluate("#x", Some(heap), Some(bindings));

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("inputs not found"));
    }

    #[test]
    fn test_input_reference_in_conditional() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate(
            "if #x > 40 then #x + 10 else #y",
            Some(heap),
            Some(bindings),
        )
        .unwrap();

        assert_eq!(result, Value::Number(52.0)); // 42 + 10
    }

    #[test]
    fn test_input_reference_with_underscore() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(RefCell::new(HashMap::new()));

        // Create inputs with underscore field
        let mut inputs_map = IndexMap::new();
        inputs_map.insert("my_value".to_string(), Value::Number(123.0));
        let inputs_value = heap.borrow_mut().insert_record(inputs_map);
        bindings
            .borrow_mut()
            .insert("inputs".to_string(), inputs_value);

        let result = parse_and_evaluate("#my_value", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(123.0));
    }
}
