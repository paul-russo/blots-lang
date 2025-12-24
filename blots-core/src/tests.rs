// Tests for late binding in function calls
#[cfg(test)]
mod late_binding_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(
        code: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<Environment>>,
    ) -> Result<Value, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = heap.unwrap_or_else(|| Rc::new(RefCell::new(Heap::new())));
        let bindings = bindings.unwrap_or_else(|| Rc::new(Environment::new()));

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
    fn test_late_binding_simple() {
        // Test that functions can reference variables defined after the function
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
        let new_bindings = Rc::new(Environment::new());

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
        let bindings = Rc::new(Environment::new());

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
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::{Heap, HeapPointer};
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use indexmap::IndexMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(
        code: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<Environment>>,
    ) -> Result<Value, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = heap.unwrap_or_else(|| Rc::new(RefCell::new(Heap::new())));
        let bindings = bindings.unwrap_or_else(|| Rc::new(Environment::new()));

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

    fn setup_inputs(heap: &Rc<RefCell<Heap>>, bindings: &Rc<Environment>) {
        // Create an inputs record with test data
        let mut inputs_map = IndexMap::new();
        inputs_map.insert("x".to_string(), Value::Number(42.0));
        inputs_map.insert("y".to_string(), Value::Number(10.0));
        inputs_map.insert(
            "name".to_string(),
            heap.borrow_mut().insert_string("Alice".to_string()),
        );

        let inputs_value = heap.borrow_mut().insert_record(inputs_map);
        bindings.insert("inputs".to_string(), inputs_value);
    }

    #[test]
    fn test_input_reference_simple() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_input_reference_string() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
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
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x + #y", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(52.0));
    }

    #[test]
    fn test_input_reference_equivalence() {
        // Test that #field is equivalent to inputs.field
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        let result1 =
            parse_and_evaluate("#x", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        let result2 = parse_and_evaluate("inputs.x", Some(heap), Some(bindings)).unwrap();

        assert_eq!(result1, result2);
    }

    #[test]
    fn test_input_reference_in_function() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
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
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#x + #x * #y", Some(heap), Some(bindings)).unwrap();
        // 42 + (42 * 10) = 42 + 420 = 462
        assert_eq!(result, Value::Number(462.0));
    }

    #[test]
    fn test_input_reference_missing_field() {
        // Missing fields should return null, consistent with inputs.field behavior
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        let result = parse_and_evaluate("#missing", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_input_reference_no_inputs() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        // Don't setup inputs

        let result = parse_and_evaluate("#x", Some(heap), Some(bindings));

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("inputs not found"));
    }

    #[test]
    fn test_input_reference_in_conditional() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
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
        let bindings = Rc::new(Environment::new());

        // Create inputs with underscore field
        let mut inputs_map = IndexMap::new();
        inputs_map.insert("my_value".to_string(), Value::Number(123.0));
        let inputs_value = heap.borrow_mut().insert_record(inputs_map);
        bindings.insert("inputs".to_string(), inputs_value);

        let result = parse_and_evaluate("#my_value", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(123.0));
    }

    #[test]
    fn test_multiline_evaluation_with_inputs() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        // Test multiple statements with inputs
        let code = "a = #x * 2\nb = #y + 10\na + b";
        let result = parse_and_evaluate(code, Some(heap), Some(bindings)).unwrap();
        // a = 42 * 2 = 84, b = 10 + 10 = 20, result = 84 + 20 = 104
        assert_eq!(result, Value::Number(104.0));
    }

    #[test]
    fn test_multiline_evaluation_with_comments() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        // Test multiple statements with comments interspersed
        let code = "// Calculate doubled value\na = #x * 2\n// Calculate offset\nb = #y + 10\n// Return sum\na + b";
        let result = parse_and_evaluate(code, Some(heap), Some(bindings)).unwrap();
        // a = 42 * 2 = 84, b = 10 + 10 = 20, result = 84 + 20 = 104
        assert_eq!(result, Value::Number(104.0));
    }

    #[test]
    fn test_multiline_with_blank_lines_and_comments() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        // Test with blank lines and comments
        let code = "x = #x * 2\ny = #y + 10\n\n// hi\nx + y";
        let result = parse_and_evaluate(code, Some(heap), Some(bindings)).unwrap();
        // x = 42 * 2 = 84, y = 10 + 10 = 20, result = 84 + 20 = 104
        assert_eq!(result, Value::Number(104.0));
    }

    #[test]
    fn test_comment_only_lines() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Test with just comments and a final expression
        let code = "// First comment\n// Second comment\n42";
        let result = parse_and_evaluate(code, Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }
}

// Tests for lambda error reporting across contexts
#[cfg(test)]
mod lambda_error_context_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(
        code: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<Environment>>,
    ) -> Result<Value, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = heap.unwrap_or_else(|| Rc::new(RefCell::new(Heap::new())));
        let bindings = bindings.unwrap_or_else(|| Rc::new(Environment::new()));

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
    fn test_lambda_error_preserves_original_source() {
        // Test that when a lambda is defined in one context and called in another,
        // errors in the lambda body still reference the original source
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define a lambda that will error (references undefined variable)
        let lambda_definition = "f = x => x + undefined_var";
        parse_and_evaluate(lambda_definition, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Call the lambda in a completely different source context
        let call_code = "f(5)";
        let result = parse_and_evaluate(call_code, Some(heap), Some(bindings));

        // The call should fail
        assert!(result.is_err());

        let error = result.unwrap_err();
        let error_msg = error.to_string();

        // The error should mention the undefined variable
        assert!(error_msg.contains("undefined_var"));
    }

    #[test]
    fn test_lambda_error_with_multiple_calls() {
        // Test that multiple calls to the same lambda all report errors correctly
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define a lambda with a division by zero
        let lambda_def = "divide = (a, b) => a / b";
        parse_and_evaluate(lambda_def, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // First call - should succeed
        let call1 = "divide(10, 2)";
        let result1 = parse_and_evaluate(call1, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)));
        assert!(result1.is_ok());
        assert_eq!(result1.unwrap(), Value::Number(5.0));

        // Second call - with division by zero should succeed (returns infinity in floating point)
        let call2 = "divide(10, 0)";
        let result2 = parse_and_evaluate(call2, Some(heap), Some(bindings));
        assert!(result2.is_ok());
    }

    #[test]
    fn test_nested_lambda_error_reporting() {
        // Test that nested lambdas report errors correctly
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define a lambda that returns another lambda
        let outer_lambda = "outer = x => (y => x + y + undefined_nested)";
        parse_and_evaluate(outer_lambda, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Call outer to get inner lambda
        let get_inner = "inner = outer(5)";
        parse_and_evaluate(get_inner, Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Call inner lambda - should error
        let call_inner = "inner(10)";
        let result = parse_and_evaluate(call_inner, Some(heap), Some(bindings));

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("undefined_nested"));
    }
}

// Tests for lambda parameter isolation
// Ensures that lambda parameters are not incorrectly captured from outer scope
#[cfg(test)]
mod lambda_parameter_isolation_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::{SerializableValue, Value};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(
        code: &str,
        heap: Option<Rc<RefCell<Heap>>>,
        bindings: Option<Rc<Environment>>,
    ) -> Result<Value, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = heap.unwrap_or_else(|| Rc::new(RefCell::new(Heap::new())));
        let bindings = bindings.unwrap_or_else(|| Rc::new(Environment::new()));

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
    fn test_lambda_parameters_not_captured_from_outer_scope() {
        // Regression test for bug where lambda parameters were incorrectly captured
        // from outer scope instead of being excluded from closure
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define x in outer scope
        parse_and_evaluate("x = 42", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        // Define a lambda with parameter x - should NOT capture outer x
        parse_and_evaluate("f = x => x + 1", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Get the lambda value and check its scope
        let f_value = bindings.get("f").unwrap();
        {
            let heap_ref = heap.borrow();
            let lambda_def = f_value.as_lambda(&heap_ref).unwrap();

            // The lambda's scope should NOT contain x
            assert!(
                !lambda_def.scope.contains_key("x"),
                "Lambda should not capture its own parameter 'x' from outer scope"
            );
        }

        // Call f(10) should return 11, not 43 (which would happen if outer x was captured)
        let result = parse_and_evaluate("f(10)", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(11.0));
    }

    #[test]
    fn test_lambda_multiple_parameters_not_captured() {
        // Test that all lambda parameters are excluded from closure
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define variables in outer scope with same names as parameters
        parse_and_evaluate("a = 1", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        parse_and_evaluate("b = 2", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        // Define lambda with parameters a and b
        parse_and_evaluate("add = (a, b) => a + b", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Get the lambda value and check its scope
        let add_value = bindings.get("add").unwrap();
        {
            let heap_ref = heap.borrow();
            let lambda_def = add_value.as_lambda(&heap_ref).unwrap();

            // The lambda's scope should NOT contain a or b
            assert!(
                !lambda_def.scope.contains_key("a"),
                "Lambda should not capture its parameter 'a'"
            );
            assert!(
                !lambda_def.scope.contains_key("b"),
                "Lambda should not capture its parameter 'b'"
            );
        }

        // Call add(10, 20) should return 30, not 3 (which would use captured values)
        let result = parse_and_evaluate("add(10, 20)", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(30.0));
    }

    #[test]
    fn test_nested_lambda_parameters_not_captured() {
        // Test that nested lambdas don't capture their own parameters
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define x in outer scope
        parse_and_evaluate("x = 100", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        // Define nested lambda - inner lambda should not capture its own x parameter
        parse_and_evaluate(
            "outer = x => (y => x + y)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get outer lambda and check its scope
        let outer_value = bindings.get("outer").unwrap();
        {
            let heap_ref = heap.borrow();
            let outer_lambda = outer_value.as_lambda(&heap_ref).unwrap();

            // Outer lambda should NOT capture x from global scope
            assert!(
                !outer_lambda.scope.contains_key("x"),
                "Outer lambda should not capture its own parameter 'x'"
            );
        }

        // Call outer(5) to get inner lambda
        parse_and_evaluate("inner = outer(5)", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
            .unwrap();

        // Call inner(3) should return 8 (5 + 3), not 103 (100 + 3)
        let result = parse_and_evaluate("inner(3)", Some(heap), Some(bindings)).unwrap();
        assert_eq!(result, Value::Number(8.0));
    }

    #[test]
    fn test_lambda_serialization_preserves_parameter_names() {
        // Test that when serializing lambdas, parameter names are not substituted
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define x in outer scope
        parse_and_evaluate("x = 42", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        // Define lambda with parameter x
        parse_and_evaluate(
            "increment = x => x + 1",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get the lambda and serialize it
        let increment_value = bindings.get("increment").unwrap();
        let serialized = increment_value
            .to_serializable_value(&heap.borrow())
            .unwrap();

        // Check that the serialized body contains "x", not "42"
        if let SerializableValue::Lambda(lambda_def) = serialized {
            assert!(
                lambda_def.body.contains("x"),
                "Serialized lambda body should contain parameter name 'x', got: {}",
                lambda_def.body
            );
            assert!(
                !lambda_def.body.contains("42"),
                "Serialized lambda body should not contain value '42', got: {}",
                lambda_def.body
            );
            assert_eq!(
                lambda_def.body, "x + 1",
                "Serialized lambda body should be 'x + 1', got: {}",
                lambda_def.body
            );
        } else {
            panic!("Expected Lambda, got: {:?}", serialized);
        }
    }

    #[test]
    fn test_curried_function_captures_correctly() {
        // Test that curried functions capture outer parameters but not their own
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Define curried add function
        parse_and_evaluate(
            "curry_add = x => y => x + y",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call curry_add(5) to get a partially applied function
        parse_and_evaluate(
            "add_five = curry_add(5)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get add_five and check its scope
        let add_five_value = bindings.get("add_five").unwrap();
        {
            let heap_ref = heap.borrow();
            let add_five_lambda = add_five_value.as_lambda(&heap_ref).unwrap();

            // add_five should capture x=5 from outer scope
            assert!(
                add_five_lambda.scope.contains_key("x"),
                "add_five should capture 'x' from outer scope"
            );
            assert_eq!(
                add_five_lambda.scope.get("x").unwrap().as_number().unwrap(),
                5.0
            );

            // but should NOT capture its own parameter y
            assert!(
                !add_five_lambda.scope.contains_key("y"),
                "add_five should not capture its own parameter 'y'"
            );
        }

        // Serialize and check body
        let serialized = add_five_value
            .to_serializable_value(&heap.borrow())
            .unwrap();

        if let SerializableValue::Lambda(lambda_def) = serialized {
            // Body should be "5 + y" (x captured as 5, y preserved as parameter)
            assert_eq!(
                lambda_def.body, "5 + y",
                "Serialized body should be '5 + y', got: {}",
                lambda_def.body
            );
        } else {
            panic!("Expected Lambda, got: {:?}", serialized);
        }
    }
}

// Tests for the where operator
#[cfg(test)]
mod where_operator_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::{Heap, HeapPointer};
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(code: &str) -> Result<(Value, Rc<RefCell<Heap>>), crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

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
        Ok((result, heap))
    }

    #[test]
    fn test_where_basic_filtering() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3, 4, 5] where x => x > 3").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0], Value::Number(4.0));
        assert_eq!(list[1], Value::Number(5.0));
    }

    #[test]
    fn test_where_with_index() {
        let (result, heap) = parse_and_evaluate("[10, 20, 30, 40] where (val, idx) => idx > 0").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(20.0));
        assert_eq!(list[1], Value::Number(30.0));
        assert_eq!(list[2], Value::Number(40.0));
    }

    #[test]
    fn test_where_empty_list() {
        let (result, heap) = parse_and_evaluate("[] where x => x > 3").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_where_all_filtered_out() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3] where x => x > 10").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_where_none_filtered_out() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3] where x => true").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(1.0));
        assert_eq!(list[1], Value::Number(2.0));
        assert_eq!(list[2], Value::Number(3.0));
    }

    #[test]
    fn test_where_scalar_error() {
        let result = parse_and_evaluate("5 where x => x > 3");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("where operator requires a list on the left side"));
    }

    #[test]
    fn test_where_non_function_error() {
        let result = parse_and_evaluate("[1, 2, 3] where 5");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("can't call a non-function"));
    }

    #[test]
    fn test_where_non_boolean_return_error() {
        let result = parse_and_evaluate("[1, 2, 3] where x => x");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("expected a boolean"));
    }

    #[test]
    fn test_where_with_string_comparison() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let pairs = crate::parser::get_pairs("[\"apple\", \"banana\", \"cherry\"] where s => s == \"banana\"").unwrap();
        let bindings = Rc::new(Environment::new());

        let mut result = Value::Null;
        for pair in pairs {
            if let crate::parser::Rule::statement = pair.as_rule() {
                if let Some(inner_pair) = pair.into_inner().next() {
                    if let crate::parser::Rule::expression = inner_pair.as_rule() {
                        result = crate::expressions::evaluate_pairs(
                            inner_pair.into_inner(),
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            0,
                            "[\"apple\", \"banana\", \"cherry\"] where s => s == \"banana\"",
                        ).unwrap();
                    }
                }
            }
        }

        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 1);

        if let Value::String(s) = list[0] {
            let string_value = s.reify(&borrowed_heap).as_string().unwrap();
            assert_eq!(string_value, "banana");
        } else {
            panic!("Expected string value");
        }
    }

    #[test]
    fn test_where_complex_predicate() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3, 4, 5, 6] where x => x % 2 == 0").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(2.0));
        assert_eq!(list[1], Value::Number(4.0));
        assert_eq!(list[2], Value::Number(6.0));
    }

    #[test]
    fn test_where_with_built_in_function() {
        let (result, heap) = parse_and_evaluate("[1, -2, 3, -4, 5] where x => x > 0").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(1.0));
        assert_eq!(list[1], Value::Number(3.0));
        assert_eq!(list[2], Value::Number(5.0));
    }
}

// Tests for flat chaining of via/into/where operators
#[cfg(test)]
mod flat_chaining_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(code: &str) -> Result<(Value, Rc<RefCell<Heap>>), crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

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
        Ok((result, heap))
    }

    #[test]
    fn test_via_then_where() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3, 4, 5] via x => x * 2 where y => y > 5").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(6.0));
        assert_eq!(list[1], Value::Number(8.0));
        assert_eq!(list[2], Value::Number(10.0));
    }

    #[test]
    fn test_where_then_via() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3, 4, 5] where x => x > 3 via y => y * 10").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0], Value::Number(40.0));
        assert_eq!(list[1], Value::Number(50.0));
    }

    #[test]
    fn test_via_where_via_chain() {
        let (result, heap) = parse_and_evaluate("[1, 2, 3, 4, 5, 6] via x => x * 2 where y => y > 5 via z => z + 1").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 4);
        assert_eq!(list[0], Value::Number(7.0));
        assert_eq!(list[1], Value::Number(9.0));
        assert_eq!(list[2], Value::Number(11.0));
        assert_eq!(list[3], Value::Number(13.0));
    }

    #[test]
    fn test_via_where_into() {
        let (result, _heap) = parse_and_evaluate("[1, 2, 3, 4, 5] via x => x * 2 where x => x > 5 into sum").unwrap();
        assert_eq!(result, Value::Number(24.0));
    }

    #[test]
    fn test_where_with_index_then_via() {
        let (result, heap) = parse_and_evaluate("[10, 20, 30, 40] where (val, idx) => idx > 0 via x => x / 10").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(2.0));
        assert_eq!(list[1], Value::Number(3.0));
        assert_eq!(list[2], Value::Number(4.0));
    }

    #[test]
    fn test_nested_via_requires_parens() {
        // This should work with parens
        let (result, heap) = parse_and_evaluate("[[1, 2], [3, 4]] via row => (row via x => x * 10)").unwrap();
        let borrowed_heap = heap.borrow();
        let outer_list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(outer_list.len(), 2);

        let first_row = outer_list[0].as_list(&borrowed_heap).unwrap();
        assert_eq!(first_row[0], Value::Number(10.0));
        assert_eq!(first_row[1], Value::Number(20.0));

        let second_row = outer_list[1].as_list(&borrowed_heap).unwrap();
        assert_eq!(second_row[0], Value::Number(30.0));
        assert_eq!(second_row[1], Value::Number(40.0));
    }

    #[test]
    fn test_complex_chain_with_different_param_names() {
        let (result, heap) = parse_and_evaluate(
            "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] where a => a % 2 == 0 via b => b * 3 where c => c > 10"
        ).unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 4);
        assert_eq!(list[0], Value::Number(12.0)); // 4 * 3
        assert_eq!(list[1], Value::Number(18.0)); // 6 * 3
        assert_eq!(list[2], Value::Number(24.0)); // 8 * 3
        assert_eq!(list[3], Value::Number(30.0)); // 10 * 3
    }
}

// Tests for via and into operator error messages
#[cfg(test)]
mod via_into_error_message_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(code: &str) -> Result<Value, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

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
    fn test_via_callback_error_message_with_list() {
        // Test that via errors say "in via callback" not "in built-in function map"
        let result = parse_and_evaluate("[1, 2, null] via x => x + 1");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"via\" callback"),
            "Error should mention 'via callback', got: {}",
            error_msg
        );
        assert!(
            !error_msg.contains("in built-in function \"map\""),
            "Error should not mention 'built-in function map', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_callback_error_message_scalar() {
        // Test scalar via also shows "via callback" context
        let result = parse_and_evaluate("null via x => x + 1");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"via\" callback"),
            "Error should mention 'via callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_callback_error_message_list_to_list() {
        // Test list-to-list via (zipped) also shows "via callback" context
        let result = parse_and_evaluate("[1, 2] via [x => x + 1, x => x + null]");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"via\" callback"),
            "Error should mention 'via callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_callback_error_includes_function_context() {
        // Test that the error also includes the function name context
        let result = parse_and_evaluate("[1, 2, null] via x => x + 1");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in anonymous function"),
            "Error should mention 'anonymous function', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_callback_error_with_named_function() {
        // Test that named functions show their name in the error
        let result = parse_and_evaluate("add_one = x => x + 1\n[1, 2, null] via add_one");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"via\" callback"),
            "Error should mention 'via callback', got: {}",
            error_msg
        );
        assert!(
            error_msg.contains("function \"add_one\""),
            "Error should mention function name, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_callback_error_with_index_parameter() {
        // Test that via with index parameter also shows proper error context
        let result = parse_and_evaluate("[1, 2, null] via (x, idx) => x + idx");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"via\" callback"),
            "Error should mention 'via callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_via_with_index_parameter_works() {
        // Test that via with index parameter actually works (regression test)
        let (result, heap) = {
            let pairs = crate::parser::get_pairs("[10, 20, 30] via (x, idx) => x + idx").unwrap();
            let heap = Rc::new(RefCell::new(crate::heap::Heap::new()));
            let bindings = Rc::new(Environment::new());
            let mut result = Value::Null;
            for pair in pairs {
                if let crate::parser::Rule::statement = pair.as_rule() {
                    if let Some(inner_pair) = pair.into_inner().next() {
                        result = crate::expressions::evaluate_pairs(
                            inner_pair.into_inner(),
                            Rc::clone(&heap),
                            Rc::clone(&bindings),
                            0,
                            "[10, 20, 30] via (x, idx) => x + idx",
                        )
                        .unwrap();
                    }
                }
            }
            (result, heap)
        };
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(10.0)); // 10 + 0
        assert_eq!(list[1], Value::Number(21.0)); // 20 + 1
        assert_eq!(list[2], Value::Number(32.0)); // 30 + 2
    }

    #[test]
    fn test_into_callback_error_message_with_list() {
        // Test that into errors say "in into callback"
        let result = parse_and_evaluate("[1, 2, 3] into list => list + \"oops\"");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"into\" callback"),
            "Error should mention 'into callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_into_callback_error_message_scalar() {
        // Test scalar into also shows "into callback" context
        let result = parse_and_evaluate("5 into x => x + \"oops\"");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"into\" callback"),
            "Error should mention 'into callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_into_callback_error_includes_function_context() {
        // Test that the error also includes the function name context
        let result = parse_and_evaluate("[1, 2, 3] into list => list + \"oops\"");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in anonymous function"),
            "Error should mention 'anonymous function', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_into_callback_error_with_named_function() {
        // Test that named functions show their name in the error
        let result = parse_and_evaluate("bad_fn = list => list + \"oops\"\n[1, 2, 3] into bad_fn");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"into\" callback"),
            "Error should mention 'into callback', got: {}",
            error_msg
        );
        assert!(
            error_msg.contains("function \"bad_fn\""),
            "Error should mention function name, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_where_callback_error_message_with_list() {
        // Test that where errors say "in where callback" not "in built-in function filter"
        // Use x + 1 > 0 because x > 0 with null returns false (doesn't error)
        let result = parse_and_evaluate("[1, 2, null] where x => x + 1 > 0");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"where\" callback"),
            "Error should mention 'where callback', got: {}",
            error_msg
        );
        assert!(
            !error_msg.contains("in built-in function \"filter\""),
            "Error should not mention 'built-in function filter', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_where_callback_error_includes_function_context() {
        // Test that the error also includes the function name context
        let result = parse_and_evaluate("[1, 2, null] where x => x + 1 > 0");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in anonymous function"),
            "Error should mention 'anonymous function', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_where_callback_error_with_named_function() {
        // Test that named functions show their name in the error
        let result = parse_and_evaluate("is_positive = x => x + 1 > 0\n[1, 2, null] where is_positive");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"where\" callback"),
            "Error should mention 'where callback', got: {}",
            error_msg
        );
        assert!(
            error_msg.contains("function \"is_positive\""),
            "Error should mention function name, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_where_callback_error_with_index_parameter() {
        // Test that where with index parameter also shows proper error context
        let result = parse_and_evaluate("[1, 2, null] where (x, idx) => x + 1 > idx");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"where\" callback"),
            "Error should mention 'where callback', got: {}",
            error_msg
        );
    }

    #[test]
    fn test_where_callback_error_on_non_boolean_return() {
        // Test that where errors on non-boolean return include "where callback" context
        let result = parse_and_evaluate("[1, 2, 3] where x => x * 2");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("in \"where\" callback"),
            "Error should mention 'where callback', got: {}",
            error_msg
        );
    }
}
