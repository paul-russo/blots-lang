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
    use crate::intern::Symbol;
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
        bindings.insert(Symbol::intern("inputs"), inputs_value);
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
    fn test_input_reference_in_function_called_through_hof() {
        // `#x` inside a lambda must resolve when the lambda is invoked from inside a
        // higher-order builtin, where the call frame is built by the builtin rather than a
        // direct call expression.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        setup_inputs(&heap, &bindings);

        parse_and_evaluate(
            "scale = n => n * #x",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result = parse_and_evaluate(
            "map([1, 2, 3], scale)",
            Some(Rc::clone(&heap)),
            Some(bindings),
        )
        .unwrap();

        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list[0], Value::Number(42.0));
        assert_eq!(list[1], Value::Number(84.0));
        assert_eq!(list[2], Value::Number(126.0));
    }

    /// Bind an `inputs` record containing only `x` into the given environment.
    fn setup_inputs_with_x(heap: &Rc<RefCell<Heap>>, bindings: &Rc<Environment>, x: f64) {
        let mut inputs_map = IndexMap::new();
        inputs_map.insert("x".to_string(), Value::Number(x));

        let inputs_value = heap.borrow_mut().insert_record(inputs_map);
        bindings.insert(Symbol::intern("inputs"), inputs_value);
    }

    #[test]
    fn test_input_reference_lambda_uses_call_time_inputs() {
        // A lambda using `#x` defined under one inputs record and called under another (sharing
        // the same heap) reads the inputs of the environment it is called in. This is the
        // in-memory half of the portability contract: input references are late-bound.
        let heap = Rc::new(RefCell::new(Heap::new()));

        let producer = Rc::new(Environment::new());
        setup_inputs_with_x(&heap, &producer, 42.0);
        parse_and_evaluate(
            "f = () => #x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&producer)),
        )
        .unwrap();
        let f_value = producer.get(Symbol::intern("f")).unwrap();

        let consumer = Rc::new(Environment::new());
        setup_inputs_with_x(&heap, &consumer, 100.0);
        consumer.insert(Symbol::intern("f"), f_value);

        let result = parse_and_evaluate("f()", Some(heap), Some(consumer)).unwrap();
        assert_eq!(result, Value::Number(200.0));
    }

    #[test]
    fn test_inputs_identifier_lambda_uses_call_time_inputs() {
        // `inputs.x` behaves identically to the `#x` shorthand: `inputs` is never captured into
        // a lambda's scope, so the lambda reads the inputs of the environment it is called in.
        let heap = Rc::new(RefCell::new(Heap::new()));

        let producer = Rc::new(Environment::new());
        setup_inputs_with_x(&heap, &producer, 42.0);
        parse_and_evaluate(
            "g = () => inputs.x * 2",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&producer)),
        )
        .unwrap();
        let g_value = producer.get(Symbol::intern("g")).unwrap();

        let consumer = Rc::new(Environment::new());
        setup_inputs_with_x(&heap, &consumer, 100.0);
        consumer.insert(Symbol::intern("g"), g_value);

        let result = parse_and_evaluate("g()", Some(heap), Some(consumer)).unwrap();
        assert_eq!(result, Value::Number(200.0));
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
        bindings.insert(Symbol::intern("inputs"), inputs_value);

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
        parse_and_evaluate(
            lambda_definition,
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
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
        parse_and_evaluate(
            lambda_def,
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
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
        parse_and_evaluate(
            outer_lambda,
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Call outer to get inner lambda
        let get_inner = "inner = outer(5)";
        parse_and_evaluate(
            get_inner,
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
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
    use crate::intern::Symbol;
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
        parse_and_evaluate(
            "f = x => x + 1",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get the lambda value and check its scope
        let f_value = bindings.get(Symbol::intern("f")).unwrap();
        {
            let heap_ref = heap.borrow();
            let lambda_def = f_value.as_lambda(&heap_ref).unwrap();

            // The lambda's scope should NOT contain x
            assert!(
                !lambda_def.scope.contains_key(Symbol::intern("x")),
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
        parse_and_evaluate(
            "add = (a, b) => a + b",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get the lambda value and check its scope
        let add_value = bindings.get(Symbol::intern("add")).unwrap();
        {
            let heap_ref = heap.borrow();
            let lambda_def = add_value.as_lambda(&heap_ref).unwrap();

            // The lambda's scope should NOT contain a or b
            assert!(
                !lambda_def.scope.contains_key(Symbol::intern("a")),
                "Lambda should not capture its parameter 'a'"
            );
            assert!(
                !lambda_def.scope.contains_key(Symbol::intern("b")),
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
        parse_and_evaluate(
            "x = 100",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Define nested lambda - inner lambda should not capture its own x parameter
        parse_and_evaluate(
            "outer = x => (y => x + y)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        // Get outer lambda and check its scope
        let outer_value = bindings.get(Symbol::intern("outer")).unwrap();
        {
            let heap_ref = heap.borrow();
            let outer_lambda = outer_value.as_lambda(&heap_ref).unwrap();

            // Outer lambda should NOT capture x from global scope
            assert!(
                !outer_lambda.scope.contains_key(Symbol::intern("x")),
                "Outer lambda should not capture its own parameter 'x'"
            );
        }

        // Call outer(5) to get inner lambda
        parse_and_evaluate(
            "inner = outer(5)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
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
        let increment_value = bindings.get(Symbol::intern("increment")).unwrap();
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
        let add_five_value = bindings.get(Symbol::intern("add_five")).unwrap();
        {
            let heap_ref = heap.borrow();
            let add_five_lambda = add_five_value.as_lambda(&heap_ref).unwrap();

            // add_five should capture x=5 from outer scope
            assert!(
                add_five_lambda.scope.contains_key(Symbol::intern("x")),
                "add_five should capture 'x' from outer scope"
            );
            assert_eq!(
                add_five_lambda
                    .scope
                    .get(Symbol::intern("x"))
                    .unwrap()
                    .as_number()
                    .unwrap(),
                5.0
            );

            // but should NOT capture its own parameter y
            assert!(
                !add_five_lambda.scope.contains_key(Symbol::intern("y")),
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

    fn parse_and_evaluate(
        code: &str,
    ) -> Result<(Value, Rc<RefCell<Heap>>), crate::error::RuntimeError> {
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
        let (result, heap) =
            parse_and_evaluate("[10, 20, 30, 40] where (val, idx) => idx > 0").unwrap();
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
        let pairs = crate::parser::get_pairs(
            "[\"apple\", \"banana\", \"cherry\"] where s => s == \"banana\"",
        )
        .unwrap();
        let bindings = Rc::new(Environment::new());

        let mut result = Value::Null;
        for pair in pairs {
            if let crate::parser::Rule::statement = pair.as_rule()
                && let Some(inner_pair) = pair.into_inner().next()
                && let crate::parser::Rule::expression = inner_pair.as_rule()
            {
                result = crate::expressions::evaluate_pairs(
                    inner_pair.into_inner(),
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    0,
                    "[\"apple\", \"banana\", \"cherry\"] where s => s == \"banana\"",
                )
                .unwrap();
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
        let (result, heap) =
            parse_and_evaluate("[1, 2, 3, 4, 5, 6] where x => x % 2 == 0").unwrap();
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

    fn parse_and_evaluate(
        code: &str,
    ) -> Result<(Value, Rc<RefCell<Heap>>), crate::error::RuntimeError> {
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
        let (result, heap) =
            parse_and_evaluate("[1, 2, 3, 4, 5] via x => x * 2 where y => y > 5").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0], Value::Number(6.0));
        assert_eq!(list[1], Value::Number(8.0));
        assert_eq!(list[2], Value::Number(10.0));
    }

    #[test]
    fn test_where_then_via() {
        let (result, heap) =
            parse_and_evaluate("[1, 2, 3, 4, 5] where x => x > 3 via y => y * 10").unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list.len(), 2);
        assert_eq!(list[0], Value::Number(40.0));
        assert_eq!(list[1], Value::Number(50.0));
    }

    #[test]
    fn test_via_where_via_chain() {
        let (result, heap) =
            parse_and_evaluate("[1, 2, 3, 4, 5, 6] via x => x * 2 where y => y > 5 via z => z + 1")
                .unwrap();
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
        let (result, _heap) =
            parse_and_evaluate("[1, 2, 3, 4, 5] via x => x * 2 where x => x > 5 into sum").unwrap();
        assert_eq!(result, Value::Number(24.0));
    }

    #[test]
    fn test_where_with_index_then_via() {
        let (result, heap) =
            parse_and_evaluate("[10, 20, 30, 40] where (val, idx) => idx > 0 via x => x / 10")
                .unwrap();
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
        let (result, heap) =
            parse_and_evaluate("[[1, 2], [3, 4]] via row => (row via x => x * 10)").unwrap();
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
                if let crate::parser::Rule::statement = pair.as_rule()
                    && let Some(inner_pair) = pair.into_inner().next()
                {
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
        let result =
            parse_and_evaluate("is_positive = x => x + 1 > 0\n[1, 2, null] where is_positive");
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

// Tests for error span positions in function calls
#[cfg(test)]
mod function_error_span_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn parse_and_evaluate(code: &str) -> Result<Value, RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| RuntimeError::new(e.to_string()))?;

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
    fn test_error_span_points_to_function_body_not_call_site() {
        // When an error occurs inside a function body, the error span should point to
        // the actual error location (inside the function), not the call site.
        //
        // Source: "f = x => asdf + x\nf(5)"
        // The error is "unknown identifier: asdf" at position 9-13 (0-indexed)
        // NOT at the call site "f(5)" on line 2

        let source = "f = x => asdf + x\nf(5)";
        //            0123456789...
        //                     ^--- "asdf" starts at position 9

        let result = parse_and_evaluate(source);
        assert!(
            result.is_err(),
            "Should have error for undefined identifier 'asdf'"
        );

        let error = result.unwrap_err();

        // Error message should mention the undefined identifier
        assert!(
            error.message.contains("asdf"),
            "Error message should mention 'asdf', got: {}",
            error.message
        );

        // Check the error span
        let span = error.span.expect("Error should have a span");

        // "asdf" is at positions 9-13 in the source
        // The span should point to "asdf", not to "f(5)" which starts at position 18
        assert_eq!(
            span.start_byte, 9,
            "Error span should start at position 9 (start of 'asdf'), got {}",
            span.start_byte
        );
        assert_eq!(
            span.end_byte, 13,
            "Error span should end at position 13 (end of 'asdf'), got {}",
            span.end_byte
        );
    }

    #[test]
    fn test_error_span_in_nested_function_call() {
        // Test that errors in nested function calls also point to the actual error location
        let source = "f = x => x + undefined_var\ng = y => f(y)\ng(5)";
        //            0         1         2
        //            0123456789012345678901234567
        //                         ^--- "undefined_var" starts at position 13

        let result = parse_and_evaluate(source);
        assert!(
            result.is_err(),
            "Should have error for undefined identifier"
        );

        let error = result.unwrap_err();
        let span = error.span.expect("Error should have a span");

        // "undefined_var" starts at position 13 in line 1
        // The error should NOT point to g(5) on line 3
        assert!(
            span.start_byte < 27, // Line 1 ends at position 26
            "Error should point to 'undefined_var' in function f (position {}), not to call site",
            span.start_byte
        );

        // Verify it's pointing to "undefined_var" specifically
        assert_eq!(
            span.start_byte, 13,
            "Error span should start at position 13 (start of 'undefined_var'), got {}",
            span.start_byte
        );
    }

    #[test]
    fn test_error_span_with_named_function() {
        // Test that named functions also preserve error spans correctly
        let source = "my_func = x => unknown_thing + x\nmy_func(10)";
        //            0         1         2         3
        //            0123456789012345678901234567890123

        let result = parse_and_evaluate(source);
        assert!(result.is_err());

        let error = result.unwrap_err();
        let span = error.span.expect("Error should have a span");

        // "unknown_thing" starts at position 15
        assert_eq!(
            span.start_byte, 15,
            "Error span should start at position 15 (start of 'unknown_thing'), got {}",
            span.start_byte
        );

        // Verify the message mentions the function name
        assert!(
            error.message.contains("my_func"),
            "Error message should mention function name 'my_func', got: {}",
            error.message
        );
    }
}

// Tests for unknown identifier error messages suggesting constants
#[cfg(test)]
mod unknown_identifier_suggestion_tests {
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
    fn test_unknown_identifier_pi_suggests_constants() {
        let result = parse_and_evaluate("pi * 2");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("Did you mean constants.pi?"),
            "Error should suggest constants.pi, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_unknown_identifier_e_suggests_constants() {
        let result = parse_and_evaluate("e ^ 2");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("Did you mean constants.e?"),
            "Error should suggest constants.e, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_unknown_identifier_no_suggestion_for_non_constant() {
        let result = parse_and_evaluate("foo + 1");
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("unknown identifier: foo"),
            "Error should mention unknown identifier, got: {}",
            error_msg
        );
        assert!(
            !error_msg.contains("Did you mean"),
            "Error should not suggest anything for non-constant, got: {}",
            error_msg
        );
    }

    #[test]
    fn test_constants_pi_works() {
        let result = parse_and_evaluate("constants.pi");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert_eq!(value, Value::Number(std::f64::consts::PI));
    }

    #[test]
    fn test_constants_e_works() {
        let result = parse_and_evaluate("constants.e");
        assert!(result.is_ok());
        let value = result.unwrap();
        assert_eq!(value, Value::Number(std::f64::consts::E));
    }
}

// Tests for negative (end-relative) indexing
#[cfg(test)]
mod negative_indexing_tests {
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
    fn test_list_negative_index_last() {
        let result = parse_and_evaluate("[1, 2, 3][-1]").unwrap();
        assert_eq!(result, Value::Number(3.0));
    }

    #[test]
    fn test_list_negative_index_second_to_last() {
        let result = parse_and_evaluate("[1, 2, 3][-2]").unwrap();
        assert_eq!(result, Value::Number(2.0));
    }

    #[test]
    fn test_list_negative_index_first() {
        let result = parse_and_evaluate("[1, 2, 3][-3]").unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_list_negative_index_out_of_bounds() {
        let result = parse_and_evaluate("[1, 2, 3][-4]").unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_list_positive_index_still_works() {
        let result = parse_and_evaluate("[1, 2, 3][0]").unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_list_negative_zero_index() {
        // -0 should be treated as 0 (first element)
        let result = parse_and_evaluate("[1, 2, 3][-0]").unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_string_negative_index_last() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let pairs =
            get_pairs("\"abc\"[-1]").map_err(|e| crate::error::RuntimeError::new(e.to_string()));
        let pairs = pairs.unwrap();
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
                                    "\"abc\"[-1]",
                                )
                                .unwrap();
                            }
                            _ => {}
                        }
                    }
                }
                Rule::EOI => {}
                _ => {}
            }
        }
        let borrowed_heap = heap.borrow();
        assert_eq!(result.as_string(&borrowed_heap).unwrap(), "c");
    }

    #[test]
    fn test_string_negative_index_out_of_bounds() {
        let result = parse_and_evaluate("\"abc\"[-4]").unwrap();
        assert_eq!(result, Value::Null);
    }
}

// Tests for binary and hexadecimal number notation
#[cfg(test)]
mod binary_hex_number_tests {
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
    fn test_binary_basic() {
        let result = parse_and_evaluate("0b1010", None, None).unwrap();
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn test_binary_zero() {
        let result = parse_and_evaluate("0b0", None, None).unwrap();
        assert_eq!(result, Value::Number(0.0));
    }

    #[test]
    fn test_binary_one() {
        let result = parse_and_evaluate("0b1", None, None).unwrap();
        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_binary_negative() {
        let result = parse_and_evaluate("-0b1010", None, None).unwrap();
        assert_eq!(result, Value::Number(-10.0));
    }

    #[test]
    fn test_binary_explicit_positive() {
        let result = parse_and_evaluate("+0b1010", None, None).unwrap();
        assert_eq!(result, Value::Number(10.0));
    }

    #[test]
    fn test_binary_with_underscores() {
        let result = parse_and_evaluate("0b1010_1010", None, None).unwrap();
        assert_eq!(result, Value::Number(170.0));
    }

    #[test]
    fn test_binary_negative_zero() {
        let result = parse_and_evaluate("-0b0", None, None).unwrap();
        assert_eq!(result, Value::Number(0.0));
    }

    #[test]
    fn test_hex_basic() {
        let result = parse_and_evaluate("0xFF", None, None).unwrap();
        assert_eq!(result, Value::Number(255.0));
    }

    #[test]
    fn test_hex_lowercase() {
        let result = parse_and_evaluate("0xff", None, None).unwrap();
        assert_eq!(result, Value::Number(255.0));
    }

    #[test]
    fn test_hex_mixed_case() {
        let result = parse_and_evaluate("0xFf", None, None).unwrap();
        assert_eq!(result, Value::Number(255.0));
    }

    #[test]
    fn test_hex_zero() {
        let result = parse_and_evaluate("0x0", None, None).unwrap();
        assert_eq!(result, Value::Number(0.0));
    }

    #[test]
    fn test_hex_negative() {
        let result = parse_and_evaluate("-0xFF", None, None).unwrap();
        assert_eq!(result, Value::Number(-255.0));
    }

    #[test]
    fn test_hex_explicit_positive() {
        let result = parse_and_evaluate("+0xFF", None, None).unwrap();
        assert_eq!(result, Value::Number(255.0));
    }

    #[test]
    fn test_hex_with_underscores() {
        let result = parse_and_evaluate("0xFF_FF", None, None).unwrap();
        assert_eq!(result, Value::Number(65535.0));
    }

    #[test]
    fn test_hex_large_value() {
        let result = parse_and_evaluate("0xFFFFFFFF", None, None).unwrap();
        assert_eq!(result, Value::Number(4294967295.0));
    }

    #[test]
    fn test_hex_negative_zero() {
        let result = parse_and_evaluate("-0x0", None, None).unwrap();
        assert_eq!(result, Value::Number(0.0));
    }

    #[test]
    fn test_binary_in_arithmetic() {
        let result = parse_and_evaluate("0b1010 + 5", None, None).unwrap();
        assert_eq!(result, Value::Number(15.0));
    }

    #[test]
    fn test_hex_in_arithmetic() {
        let result = parse_and_evaluate("0xFF * 2", None, None).unwrap();
        assert_eq!(result, Value::Number(510.0));
    }

    #[test]
    fn test_binary_and_hex_together() {
        let result = parse_and_evaluate("0b1010 + 0xFF", None, None).unwrap();
        assert_eq!(result, Value::Number(265.0));
    }

    #[test]
    fn test_binary_in_comparison() {
        let result = parse_and_evaluate("0b1010 == 10", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_hex_in_comparison() {
        let result = parse_and_evaluate("0xFF == 255", None, None).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_binary_in_list() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let result =
            parse_and_evaluate("[0b1, 0b10, 0b11]", Some(Rc::clone(&heap)), Some(bindings))
                .unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list[0], Value::Number(1.0));
        assert_eq!(list[1], Value::Number(2.0));
        assert_eq!(list[2], Value::Number(3.0));
    }

    #[test]
    fn test_hex_in_list() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let result =
            parse_and_evaluate("[0x1, 0xA, 0xFF]", Some(Rc::clone(&heap)), Some(bindings)).unwrap();
        let borrowed_heap = heap.borrow();
        let list = result.as_list(&borrowed_heap).unwrap();
        assert_eq!(list[0], Value::Number(1.0));
        assert_eq!(list[1], Value::Number(10.0));
        assert_eq!(list[2], Value::Number(255.0));
    }
}

// Tests for statement-boundary heap compaction
#[cfg(test)]
mod heap_compaction_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::{Heap, HeapValue};
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

    /// Fill the heap with string allocations that nothing references.
    fn insert_garbage(heap: &Rc<RefCell<Heap>>, count: usize) {
        for i in 0..count {
            heap.borrow_mut().insert_string(format!("garbage {}", i));
        }
    }

    #[test]
    fn compact_drops_garbage_and_keeps_bindings() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "xs = [1, 2, 3]",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        insert_garbage(&heap, 100);
        let len_before = heap.borrow().len();

        heap.borrow_mut().compact(&bindings, &mut []);

        assert!(heap.borrow().len() < len_before);
        let result = parse_and_evaluate(
            "sum(xs)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Number(6.0));
    }

    #[test]
    fn compact_preserves_lambda_captured_scope() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "make = n => do { ys = [n, n * 2, n * 3]; return x => x + sum(ys) }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        parse_and_evaluate(
            "g = make(10)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        insert_garbage(&heap, 100);

        heap.borrow_mut().compact(&bindings, &mut []);

        let result =
            parse_and_evaluate("g(1)", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(result, Value::Number(61.0));
    }

    #[test]
    fn compact_preserves_nested_lists_records_and_equality() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "a = { items: [[1, 2], [3, 4]], meta: { tag: \"t\" } }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        parse_and_evaluate(
            "b = { items: [[1, 2], [3, 4]], meta: { tag: \"t\" } }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        insert_garbage(&heap, 100);

        heap.borrow_mut().compact(&bindings, &mut []);

        let equal =
            parse_and_evaluate("a == b", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings)))
                .unwrap();
        assert_eq!(equal, Value::Bool(true));

        let nested_sum = parse_and_evaluate(
            "sum(a.items[0]) + sum(a.items[1])",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(nested_sum, Value::Number(10.0));

        let tag_matches = parse_and_evaluate(
            "a.meta.tag == \"t\"",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(tag_matches, Value::Bool(true));
    }

    #[test]
    fn compact_keeps_constants_record_pinned_at_slot_zero() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        insert_garbage(&heap, 100);
        heap.borrow_mut().compact(&bindings, &mut []);

        match heap.borrow().get(0) {
            Some(HeapValue::Record(record)) => assert!(record.contains_key("pi")),
            other => panic!("expected the constants record at slot 0, got {:?}", other),
        }

        let result = parse_and_evaluate(
            "constants.pi",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Number(std::f64::consts::PI));
    }

    #[test]
    fn compact_rewrites_extra_roots() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        insert_garbage(&heap, 100);
        let mut root = {
            heap.borrow_mut()
                .insert_list(vec![Value::Number(7.0), Value::Number(8.0)])
        };
        insert_garbage(&heap, 100);

        heap.borrow_mut().compact(&bindings, &mut [&mut root]);

        let borrowed_heap = heap.borrow();
        let list = root.as_list(&borrowed_heap).unwrap();
        assert_eq!(list, &vec![Value::Number(7.0), Value::Number(8.0)]);
    }

    #[test]
    fn compact_if_grown_respects_minimum_size_and_growth_thresholds() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        // Small heaps are never compacted, even when everything in them is garbage.
        insert_garbage(&heap, 10);
        assert!(!heap.borrow_mut().compact_if_grown(&bindings, &mut []));

        // Dead payload bytes trigger compaction even when the slot count stays small: a handful
        // of large unreferenced lists is enough.
        for _ in 0..40 {
            heap.borrow_mut()
                .insert_list(vec![Value::Number(0.0); 4096]);
        }
        let len_before = heap.borrow().len();
        assert!(heap.borrow_mut().compact_if_grown(&bindings, &mut []));
        assert!(heap.borrow().len() < len_before);

        // Immediately afterwards the heap has not grown again, so nothing happens.
        assert!(!heap.borrow_mut().compact_if_grown(&bindings, &mut []));
    }
}

// Tests pinning the slot-resolved call path: parameters and captures are read by position
// inside lambda bodies, while shadowed, late-bound, and deserialized references keep their
// dynamic, by-name resolution.
#[cfg(test)]
mod slot_frame_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::intern::Symbol;
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
    fn test_do_block_shadowing_of_parameter() {
        // Before the block-local assignment the name resolves to the parameter slot; after it,
        // the reference must see the do-block local instead.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "f = x => do { before = x * 10; x = 7; after = x * 10; return [before, after] }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        parse_and_evaluate(
            "r = f(3)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let before = parse_and_evaluate(
            "r[0]",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        let after = parse_and_evaluate(
            "r[1]",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(before, Value::Number(30.0));
        assert_eq!(after, Value::Number(70.0));
    }

    #[test]
    fn test_shorthand_record_key_reads_parameter() {
        // Shorthand record keys are looked up by name at evaluation time, so the call frame
        // must answer name-based queries for its positional argument slots.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "f = name => { name }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result = parse_and_evaluate(
            "f(\"alice\").name == \"alice\"",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_recursion_through_self_binding() {
        // The recursive name is an unbound capture at definition time, so each call falls back
        // to the by-name lookup and finds the frame's self-binding.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "fact = n => if n <= 1 then 1 else n * fact(n - 1)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result = parse_and_evaluate(
            "fact(5)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Number(120.0));
    }

    #[test]
    fn test_captured_null_is_not_treated_as_unbound() {
        // `y` is captured with the value null and is out of scope by the time the closure is
        // called: the capture slot must answer (with null) rather than fall back to a by-name
        // lookup, which would fail.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "g = do { y = null; return () => y }",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result =
            parse_and_evaluate("g()", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_unbound_capture_falls_back_to_late_binding() {
        // `z` is unbound when the closure is created, so the capture slot stays empty and the
        // reference resolves through the call-site chain to the later definition.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "h = () => z",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        parse_and_evaluate("z = 42", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();

        let result =
            parse_and_evaluate("h()", Some(Rc::clone(&heap)), Some(Rc::clone(&bindings))).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_optional_and_rest_parameters_fill_slots() {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "f = (a, b?) => if b == null then a else a + b",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        parse_and_evaluate(
            "g = (first, ...rest) => first + len(rest)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();

        let result = parse_and_evaluate(
            "f(1) + 10 * f(1, 2) + 100 * g(1) + 1000 * g(1, 2, 3)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        // f(1) = 1, f(1, 2) = 3, g(1) = 1 (empty rest), g(1, 2, 3) = 3.
        assert_eq!(result, Value::Number(1.0 + 30.0 + 100.0 + 3000.0));
    }

    #[test]
    fn test_self_call_with_spread_arguments() {
        // A function that calls itself knows how many arguments it passes, unless one of them
        // is a spread: those are only counted once they are spread.
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        for definition in [
            "last = (n, acc) => if n == 0 then acc else last(...[n - 1, acc + n])",
            "inner = (n, acc) => if n == 0 then acc else 1 + inner(n - 1, ...[acc])",
            "surplus = (n, acc) => if n == 0 then acc else surplus(...[n - 1, acc, 1])",
        ] {
            parse_and_evaluate(
                definition,
                Some(Rc::clone(&heap)),
                Some(Rc::clone(&bindings)),
            )
            .unwrap();
        }

        let result = parse_and_evaluate(
            "last(...[100, 0]) + 10000 * inner(10, 5)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Number(5050.0 + 150000.0));

        let error = parse_and_evaluate(
            "surplus(1, 0)",
            Some(Rc::clone(&heap)),
            Some(Rc::clone(&bindings)),
        )
        .unwrap_err()
        .to_string();
        assert!(
            error.contains("takes exactly 2 arguments, but 3 were given"),
            "{error}"
        );
    }

    #[test]
    fn test_deserialized_function_with_unresolved_body_is_callable() {
        // Deserialized lambdas re-parse their body outside any lambda parse arm, so their
        // identifiers stay unresolved and must keep working through the dynamic name lookup.
        let producer_heap = Rc::new(RefCell::new(Heap::new()));
        let producer_bindings = Rc::new(Environment::new());

        parse_and_evaluate(
            "base = 100",
            Some(Rc::clone(&producer_heap)),
            Some(Rc::clone(&producer_bindings)),
        )
        .unwrap();
        parse_and_evaluate(
            "f = (a, b) => base + a * 10 + b",
            Some(Rc::clone(&producer_heap)),
            Some(Rc::clone(&producer_bindings)),
        )
        .unwrap();

        let f_value = producer_bindings.get(Symbol::intern("f")).unwrap();
        let serialized =
            SerializableValue::from_value(&f_value, &producer_heap.borrow()).unwrap();

        // Rebuild the function in a fresh consumer document and call it with arguments.
        let consumer_heap = Rc::new(RefCell::new(Heap::new()));
        let consumer_bindings = Rc::new(Environment::new());
        let g_value = serialized.to_value(&mut consumer_heap.borrow_mut()).unwrap();
        consumer_bindings.insert(Symbol::intern("g"), g_value);

        let result = parse_and_evaluate(
            "g(3, 4)",
            Some(Rc::clone(&consumer_heap)),
            Some(Rc::clone(&consumer_bindings)),
        )
        .unwrap();
        assert_eq!(result, Value::Number(134.0));
    }
}

// Tests pinning arithmetic over whole lists: a run of operators is evaluated in one pass over
// the elements and may feed an aggregate function directly, and none of that is allowed to be
// observable.
#[cfg(test)]
mod list_arithmetic_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_pairs;
    use crate::heap::Heap;
    use crate::parser::{Rule, get_pairs};
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code` and render the value of the last one.
    fn evaluate(code: &str) -> Result<String, crate::error::RuntimeError> {
        let pairs = get_pairs(code).map_err(|e| crate::error::RuntimeError::new(e.to_string()))?;
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        let mut result = Value::Null;
        for pair in pairs {
            if pair.as_rule() != Rule::statement {
                continue;
            }

            let Some(inner) = pair.into_inner().next() else {
                continue;
            };
            if matches!(inner.as_rule(), Rule::expression | Rule::assignment) {
                result = evaluate_pairs(
                    inner.into_inner(),
                    Rc::clone(&heap),
                    Rc::clone(&bindings),
                    0,
                    code,
                )?;
            }
        }

        Ok(result.stringify_internal(&heap.borrow()))
    }

    fn assert_evaluates_to(code: &str, expected: &str) {
        assert_eq!(evaluate(code).unwrap(), expected, "{code}");
    }

    #[test]
    fn chains_match_the_operators_applied_one_at_a_time() {
        assert_evaluates_to(
            "xs = range(1, 6)\n(xs * 2 + 1) / 4",
            "[0.75, 1.25, 1.75, 2.25, 2.75]",
        );
        assert_evaluates_to("xs = range(1, 6)\n10 - xs * xs % 7", "[9, 6, 8, 8, 6]");
        assert_evaluates_to(
            "xs = range(1, 6)\n2 ^ (xs - 1) - xs / 2",
            "[0.5, 1, 2.5, 6, 13.5]",
        );
        assert_evaluates_to(
            "xs = [1, 2, 3]\nys = [10, 20, 30]\n(xs + ys) * ys - xs / ys",
            "[109.9, 439.9, 989.9]",
        );
    }

    #[test]
    fn chains_longer_than_a_block_of_elements_are_complete() {
        assert_evaluates_to(
            "xs = range(1000)\nys = xs * 2 + 1\n[len(ys), ys[0], ys[127], ys[128], ys[999]]",
            "[1000, 1, 255, 257, 1999]",
        );
        assert_evaluates_to("xs = range(1000)\nsum(xs * 2 + 1)", "1000000");
        assert_evaluates_to("xs = range(1000)\nsum((xs + xs) * xs % 7)", "2000");
    }

    #[test]
    fn aggregates_of_chains_match_aggregates_of_lists() {
        let cases = [
            ("sum", "35"),
            ("avg", "7"),
            ("prod", "10395"),
            ("min", "3"),
            ("max", "11"),
            ("median", "7"),
            ("len", "5"),
        ];
        for (function, expected) in cases {
            assert_evaluates_to(
                &format!("xs = range(1, 6)\n{function}(xs * 2 + 1)"),
                expected,
            );
            assert_evaluates_to(
                &format!("xs = range(1, 6)\nys = xs * 2 + 1\n{function}(ys)"),
                expected,
            );
        }
    }

    #[test]
    fn aggregates_follow_floating_point_conventions() {
        assert_evaluates_to("sum([0.1, 0.2, 0.3] * 1)", "0.6000000000000001");
        assert_evaluates_to("max([1, 2, 3] * (0 / 0))", "-inf");
        assert_evaluates_to("min([1, 2, 3] * (0 / 0))", "inf");
        assert_evaluates_to("sum([1, 2, 3] * infinity - infinity)", "NaN");
        assert_evaluates_to("1 / sum([0] * -1)", "-inf");
        assert_evaluates_to("1 / prod([0, 5] * -1 + 0)", "-inf");
    }

    #[test]
    fn operands_of_a_chain_keep_their_values() {
        assert_evaluates_to(
            "xs = [1, 2, 3]\nys = xs * 2 + 1\nzs = ys - xs\n[xs, ys, zs, sum(ys * 2 + xs)]",
            "[[1, 2, 3], [3, 5, 7], [2, 3, 4], 36]",
        );
        assert_evaluates_to(
            "f = t => do { a = t * 2; b = a + 1; c = (a - b) * 3 + a; return [sum(b * 2 + a), c, a, t] }\nf([1, 2, 3])",
            "[42, [-1, 1, 3], [2, 4, 6], [1, 2, 3]]",
        );
    }

    #[test]
    fn empty_lists_reach_the_aggregate_as_lists() {
        assert_evaluates_to("[] * 2 + 1", "[]");

        let error = evaluate("sum([] * 2 + 1)").unwrap_err().to_string();
        assert!(
            error.contains("sum requires at least one number"),
            "{error}"
        );
    }

    #[test]
    fn values_that_are_not_numbers_take_the_general_path() {
        assert_evaluates_to("[\"a\", \"b\"] + [\"c\", \"d\"]", "[ac, bd]");
        assert_evaluates_to("[1, \"b\"] + [2, \"d\"]", "[3, bd]");

        let error = evaluate("xs = [1, 2, \"x\"]\nsum(xs * 2 + 1)")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("expected a number, but got a string"),
            "{error}"
        );

        let error = evaluate("xs = [1, 2, 3]\nsum((xs * 2 + 1) - [1, null, 3])")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("expected a number, but got a null"),
            "{error}"
        );

        let error = evaluate("xs = [1, 2, 3]\nxs * 2 + [1, 2]")
            .unwrap_err()
            .to_string();
        assert!(error.contains("lists must be the same length"), "{error}");

        let error = evaluate("xs = [1, 2, 3]\nsum(xs * 2 + 1, 4)")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("expected a number, but got a list"),
            "{error}"
        );
    }
}

// Programs far larger than anyone writes by hand (generated code, tables pasted in as
// literals) must not run into the sizes of the instruction operands.
#[cfg(test)]
mod large_program_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code` and render the value of the last one.
    fn evaluate(code: &str) -> Result<String, RuntimeError> {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let source: Rc<str> = code.into();

        let mut result = Value::Null;
        for statement in statements {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };
            result = evaluate_ast(
                &expr,
                Rc::clone(&heap),
                Rc::clone(&bindings),
                0,
                Rc::clone(&source),
            )?;
        }

        Ok(result.stringify_internal(&heap.borrow()))
    }

    fn assert_evaluates_to(code: &str, expected: &str) {
        assert_eq!(evaluate(code).unwrap(), expected);
    }

    fn joined(count: usize, item: impl Fn(usize) -> String) -> String {
        (0..count).map(item).collect::<Vec<_>>().join(", ")
    }

    #[test]
    fn a_call_takes_more_arguments_than_a_function_has_registers() {
        for count in [255, 256, 257, 1000, 70_000] {
            let args = joined(count, |i| i.to_string());
            let total = count * (count - 1) / 2;

            assert_evaluates_to(&format!("sum({args})"), &total.to_string());
            assert_evaluates_to(
                &format!("f = (first, ...rest) => first + sum(rest)\nf({args})"),
                &total.to_string(),
            );
            assert_evaluates_to(&format!("sum({args}, ...[1, 2])"), &(total + 3).to_string());
        }
    }

    #[test]
    fn a_call_with_many_arguments_is_checked_like_any_other() {
        let args = joined(300, |i| i.to_string());

        let error = evaluate(&format!("f = (a, b) => a + b\nf({args})"))
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("takes exactly 2 arguments, but 300 were given"),
            "{error}"
        );

        let error = evaluate(&format!("typeof({args})"))
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("takes exactly 1 arguments, but 300 were given"),
            "{error}"
        );

        let error = evaluate(&format!("f = 5\nf({args})"))
            .unwrap_err()
            .to_string();
        assert!(error.contains("can't call a non-function: 5"), "{error}");
    }

    #[test]
    fn a_function_with_many_parameters_calls_itself() {
        for count in [2, 3, 64, 65, 66, 127, 128, 129, 255, 256, 257, 300] {
            // Every call passes the parameters after the first on rotated by one place.
            let rest = count - 1;
            let turns = 5;
            let params = joined(rest, |i| format!("p{i}"));
            let rotated = joined(rest, |i| format!("p{}", (i + 1) % rest));
            let args = joined(rest, |i| i.to_string());
            let expected = format!("[{}]", joined(rest, |i| ((i + turns) % rest).to_string()));

            // In tail position the call replaces the frame; as an operand it cannot.
            for call in [
                format!("f(n - 1, {rotated})"),
                format!("f(n - 1, {rotated}) + 0"),
            ] {
                assert_evaluates_to(
                    &format!(
                        "f = (n, {params}) => if n == 0 then [{params}] else {call}\nf({turns}, {args})"
                    ),
                    &expected,
                );
            }
        }
    }

    #[test]
    fn a_function_refers_to_many_strings_and_names() {
        // Every string and name is used twice: before and after the function has seen all of
        // them.
        for count in [3, 16, 17, 18, 40, 5000] {
            let there_and_back = |item: &dyn Fn(usize) -> String| {
                let there = joined(count, item);
                let back = joined(count, |i| item(count - 1 - i));
                format!("[{there}, {back}]")
            };

            assert_evaluates_to(
                &there_and_back(&|i| format!("\"s{i}\"")),
                &there_and_back(&|i| format!("s{i}")),
            );

            let fields = joined(count, |i| format!("f{i}: {i}"));
            assert_evaluates_to(
                &format!(
                    "r = {{{fields}}}\n{}",
                    there_and_back(&|i| format!("r.f{i}"))
                ),
                &there_and_back(&|i| i.to_string()),
            );

            let definitions = (0..count)
                .map(|i| format!("v{i} = {i} * 2\n"))
                .collect::<String>();
            assert_evaluates_to(
                &format!("{definitions}{}", there_and_back(&|i| format!("v{i}"))),
                &there_and_back(&|i| (i * 2).to_string()),
            );
        }
    }

    #[test]
    fn a_function_builds_more_records_than_it_has_layouts() {
        let count = 70_000;

        let rows = joined(count, |i| format!("{{id: {i}, half: {i} / 2}}"));
        assert_evaluates_to(
            &format!("rows = [{rows}]\n[len(rows), rows[0].id, rows[69999].id, rows[69999].half]"),
            "[70000, 0, 69999, 34999.5]",
        );

        // Each row takes two layouts, and the last one that is left goes to either the record
        // or the record inside it, depending on how many came before.
        let nested = joined(count, |i| format!("{{id: {i}, at: {{x: {i}, y: 1}}}}"));
        for before in ["", "{z: 0}, "] {
            assert_evaluates_to(
                &format!(
                    "rows = [{before}{nested}]\nlast = rows[-1]\n[rows[-37233], rows[-37232], last.at.x - last.id, last.at]"
                ),
                "[{id: 32767, at: {x: 32767, y: 1}}, {id: 32768, at: {x: 32768, y: 1}}, 0, {x: 69999, y: 1}]",
            );
        }

        let distinct = joined(count, |i| format!("{{k{i}: {i}, k{i}: {i} + 1}}"));
        assert_evaluates_to(
            &format!(
                "rows = [{distinct}]\n[len(rows), rows[1], rows[65535], rows[65536], rows[69999].k69999]"
            ),
            "[70000, {k1: 2}, {k65535: 65536}, {k65536: 65537}, 70000]",
        );
    }
}

// A function that ends in a call to itself starts over in the frame it has, and computes the
// arguments of that call right into its parameters where it can. Every argument still has to
// see the parameters as they were when the call began.
#[cfg(test)]
mod tail_call_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code` and render the value of the last one.
    fn evaluate(code: &str) -> Result<String, RuntimeError> {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let source: Rc<str> = code.into();

        let mut result = Value::Null;
        for statement in statements {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };
            result = evaluate_ast(
                &expr,
                Rc::clone(&heap),
                Rc::clone(&bindings),
                0,
                Rc::clone(&source),
            )?;
        }

        Ok(result.stringify_internal(&heap.borrow()))
    }

    fn assert_evaluates_to(code: &str, expected: &str) {
        assert_eq!(evaluate(code).unwrap(), expected, "{code}");
    }

    #[test]
    fn arguments_read_the_parameters_of_the_call_they_are_in() {
        for (call, expected) in [
            // Parameters trade places, in pairs and in a circle.
            ("f(n - 1, b, a, c)", "[2, 1, 3]"),
            ("f(n - 1, c, a, b)", "[1, 2, 3]"),
            ("f(n - 1, b, c, a)", "[1, 2, 3]"),
            ("f(n - 1, c, b, a)", "[3, 2, 1]"),
            // An argument reads a parameter that an earlier argument replaces.
            ("f(n - 1, a + 1, a * 10, c)", "[4, 30, 3]"),
            ("f(n - 1, a + 1, b, a + c)", "[4, 2, 9]"),
            ("f(n - 1, b, a + b, c)", "[5, 8, 3]"),
            ("f(n - 1, a + b, a, c)", "[7, 4, 3]"),
            ("f(n - 1, a + b + c, a - b, b - c)", "[11, -3, 7]"),
            ("f(n - 1, a, a, a)", "[1, 1, 1]"),
            ("f(n - 1, c, c, c)", "[3, 3, 3]"),
            // An argument reads its own parameter more than once.
            ("f(n - 1, (a + 1) * a, b, c)", "[42, 2, 3]"),
            ("f(n - 1, a, [b, b + 1][b - b], c * c - c)", "[1, 2, 870]"),
            (
                "f(n - 1, if a > b then a - b else b - a, max(a, b), c)",
                "[1, 2, 3]",
            ),
            // The count itself moves on before the others have read it.
            ("f(n - 1, a + n, b * n, c - n)", "[7, 12, -3]"),
        ] {
            assert_evaluates_to(
                &format!("f = (n, a, b, c) => if n == 0 then [a, b, c] else {call}\nf(3, 1, 2, 3)"),
                expected,
            );
        }
    }

    #[test]
    fn code_that_runs_for_an_argument_reads_the_parameters_of_the_call() {
        for (argument, expected) in [
            // A callee looks the caller's parameter up by name.
            ("peek()", "[3, 2]"),
            ("peek() + peek()", "[3, 4]"),
            ("twice(1)", "[3, 5]"),
            // A callback and a closure read the parameter where they are written.
            ("sum(map([1, 2], x => x + a))", "[3, 7]"),
            ("reduce([1, 2], (total, x) => total + x * a, b)", "[3, 9]"),
            ("(() => a)()", "[3, 2]"),
            ("a into (x => x + a)", "[3, 4]"),
            ("sum([1, 2] via (x => x * a))", "[3, 6]"),
            ("do { twice_a = a + a; return twice_a + 1 }", "[3, 5]"),
        ] {
            assert_evaluates_to(
                &format!(
                    "peek = () => a\ntwice = x => x + a * 2\nf = (n, a, b) => if n == 0 then [a, b] else f(n - 1, a + 1, {argument})\nf(3, 0, 0)"
                ),
                expected,
            );
        }
    }

    #[test]
    fn every_parameter_of_a_wide_function_is_replaced_at_once() {
        for count in [1, 2, 3, 63, 64, 65, 66, 70, 130, 254] {
            // Each parameter takes on the sum of itself and its neighbour, and the last one
            // that of itself and the first.
            let turns = 3;
            let params = (0..count).map(|i| format!("p{i}")).collect::<Vec<_>>();
            let sums = (0..count)
                .map(|i| format!("p{i} + p{}", (i + 1) % count))
                .collect::<Vec<_>>();
            let start = (0..count).map(|i| i.to_string()).collect::<Vec<_>>();

            let mut values = (0..count).collect::<Vec<_>>();
            for _ in 0..turns {
                values = (0..count)
                    .map(|i| values[i] + values[(i + 1) % count])
                    .collect();
            }

            let expected = values.iter().map(usize::to_string).collect::<Vec<_>>();
            assert_evaluates_to(
                &format!(
                    "f = (n, {params}) => if n == 0 then [{params}] else f(n - 1, {sums})\nf({turns}, {start})",
                    params = params.join(", "),
                    sums = sums.join(", "),
                    start = start.join(", "),
                ),
                &format!("[{}]", expected.join(", ")),
            );
        }
    }

    #[test]
    fn a_call_that_is_too_deep_fails_with_the_arguments_in_place() {
        // The call that goes beyond the depth limit is an ordinary one, whichever way its
        // arguments were computed.
        for call in ["f(n - 1, a + 1, b)", "f(n - 1, b, a)", "f(n - 1, a + b, a)"] {
            let definition = format!("f = (n, a, b) => if n == 0 then [a, b] else {call}");

            assert!(evaluate(&format!("{definition}\nf(999, 0, 1)")).is_ok());

            let error = evaluate(&format!("{definition}\nf(1001, 0, 1)"))
                .unwrap_err()
                .to_string();
            assert!(error.contains("maximum call depth"), "{error}");
        }
    }
}

// The callback of a loop that is compiled in place starts by computing what is the same on
// every pass once, ahead of the passes. Nothing may tell that from computing it on every pass.
#[cfg(test)]
mod loop_invariant_tests {
    use crate::bytecode::{Instr, Op};
    use crate::compiler::compile_chunk;
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::values::Value;
    use crate::vm::Vm;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code` and render the value of the last one.
    fn evaluate(code: &str) -> Result<String, RuntimeError> {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let source: Rc<str> = code.into();

        let mut result = Value::Null;
        for statement in statements {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };
            result = evaluate_ast(
                &expr,
                Rc::clone(&heap),
                Rc::clone(&bindings),
                0,
                Rc::clone(&source),
            )?;
        }

        Ok(result.stringify_internal(&heap.borrow()))
    }

    fn assert_evaluates_to(code: &str, expected: &str) {
        assert_eq!(evaluate(code).unwrap(), expected, "{code}");
    }

    fn assert_fails_with(code: &str, expected: &str) {
        let error = evaluate(code).unwrap_err().to_string();
        assert!(error.contains(expected), "{code}: {error}");
    }

    /// The instructions that read an element or a field in the innermost loop of the
    /// statement `code`: how many of them run once, ahead of the passes, and how many run on
    /// every pass.
    fn reads_of_innermost_loop(code: &str) -> (usize, usize) {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let [Statement::Expression(expr)] = &statements[..] else {
            panic!("{code} is not a single expression");
        };
        let chunk = compile_chunk(expr, true, &Environment::new()).unwrap();

        let mut instructions = Vec::new();
        let mut pc = 0;
        while pc < chunk.code.len() {
            // SAFETY: `pc` only lands on instruction words, as number operands are skipped.
            let instr = unsafe { Instr::decode(chunk.code[pc]) };
            instructions.push((pc, instr));
            pc += 1 + instr.operand_words();
        }

        // A loop is complete before the loop around it goes on to its next pass, so the first
        // instruction that starts a next pass belongs to the innermost loop.
        let (back, next) = instructions
            .iter()
            .copied()
            .find(|(_, instr)| matches!(instr.op, Op::ForRangeNext | Op::ForListNext))
            .expect("a loop that is compiled in place");
        let start = (back as isize + 1 + next.sbx()) as usize;
        let (entry, _) = instructions
            .iter()
            .copied()
            .rfind(|(pc, instr)| {
                *pc < start && matches!(instr.op, Op::ForRange | Op::ForList) && instr.a == next.a
            })
            .expect("the instruction that enters the loop");

        let reads = |from: usize, to: usize| {
            instructions
                .iter()
                .filter(|(pc, instr)| {
                    (from..to).contains(pc) && matches!(instr.op, Op::Index | Op::GetField)
                })
                .count()
        };

        (reads(entry, start), reads(start, back))
    }

    /// Evaluate the statement `code` against what `bindings` holds, with its loops compiled
    /// in place or with their callbacks called like any function, and render what comes of
    /// it: the value, or the error with the place it points at.
    fn outcome(
        code: &str,
        heap: &Rc<RefCell<Heap>>,
        bindings: &Rc<Environment>,
        inline: bool,
    ) -> String {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let [Statement::Expression(expr)] = &statements[..] else {
            panic!("{code} is not a single expression");
        };

        let result = compile_chunk(expr, inline, bindings).and_then(|chunk| {
            Vm::new(Rc::clone(heap), Rc::clone(bindings)).run_chunk(&chunk, code.into(), 0)
        });

        match result {
            Ok(value) => value.stringify_internal(&heap.borrow()),
            Err(error) => format!("{} at {:?}", error.message, error.span),
        }
    }

    // A call evaluates what it calls, then what it passes from left to right, and only then
    // finds out whether it can call that. Here every part of a call is, independently of the
    // others, the same on every pass or not, and something that fails or not, so that what a
    // loop ends in tells the order in which the parts were evaluated. A callback that is
    // called like any function computes nothing ahead of the passes, which makes it what a
    // loop compiled in place has to agree with.
    #[test]
    fn a_call_in_a_callback_evaluates_its_parts_in_order() {
        const CALLEES: [&str; 14] = [
            "h",
            "nope_f",
            "r.f",
            "r.g",
            "r.zz",
            "z.f",
            "fl[j]",
            "fl[x]",
            "x.f",
            "max",
            "5",
            "((p, q) => p - q)",
            "(if x > 0 then h else r.f)",
            "u",
        ];
        const ARGS: [&str; 8] = [
            "1",
            "m[0][1]",
            "z[0]",
            "nope",
            "x",
            "x.f",
            "m[x][0]",
            "...[1, 2]",
        ];
        const PLACES: [&str; 8] = [
            "C",
            "C + m[1][1]",
            "m[1][1] + C",
            "z[1] + C",
            "x + C",
            "x.g + C",
            "max(C, z[1])",
            "if C > z[1] then 1 else 2",
        ];
        const LOOPS: [&str; 4] = [
            "map([0, 1], x => B)",
            "reduce(range(2), (acc, x) => acc + B, 0)",
            "filter([0, 1], x => B)",
            "map([], x => B)",
        ];

        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        for definition in [
            "m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]",
            "z = null",
            "r = {a: 2, f: (p, q) => p - q, g: 5}",
            "fl = [(p, q) => p + q, (p, q) => p * q]",
            "h = (p, q) => p * 10 + q",
            "u = p => p + 1",
            "j = 1",
        ] {
            let bound = outcome(definition, &heap, &bindings, true);
            assert!(!bound.contains(" at "), "{definition}: {bound}");
        }

        let mut lists_of_args = vec![String::new()];
        for first in ARGS {
            lists_of_args.push(first.to_string());
            for second in ARGS {
                lists_of_args.push(format!("{first}, {second}"));
            }
        }

        let mut outcomes = std::collections::HashSet::new();
        for callee in CALLEES {
            for args in &lists_of_args {
                let call = format!("{callee}({args})");
                for place in PLACES {
                    let body = place.replace('C', &call);
                    for form in LOOPS {
                        let code = form.replace('B', &body);
                        let called = outcome(&code, &heap, &bindings, false);
                        let in_place = outcome(&code, &heap, &bindings, true);

                        assert_eq!(in_place, called, "{code}");
                        outcomes.insert(called.split(" at ").next().unwrap().to_string());
                    }
                }
            }
        }

        // The programs end in many different ways, or the order would not show.
        assert!(outcomes.len() >= 30, "{} outcomes", outcomes.len());
    }

    #[test]
    fn what_is_the_same_on_every_pass_is_read_once() {
        for (code, expected) in [
            // The row is read ahead of the loop over its columns.
            (
                "map(range(2), i => reduce(range(2), (acc, j) => acc + m[i][j] * 10, 0))",
                (1, 1),
            ),
            (
                "map(range(2), i => map(range(2), j => reduce(range(2), (acc, k) => acc + a[i][k] * b[k][j], 0)))",
                (1, 3),
            ),
            // So are the fields and elements on the way to what differs, and what does not
            // differ at all.
            ("map(xs, x => r.a.b[x])", (2, 1)),
            ("map(xs, x => x + r.a.b[1])", (3, 0)),
            ("map(xs, x => r.a.b[1] * 10 + r.c[x])", (4, 1)),
            ("filter(xs, x => x > r.limit)", (1, 0)),
            ("map(xs, x => max(m[0], x))", (1, 0)),
            ("map(xs, x => if x > m[0] then m[1] else x)", (1, 1)),
            // A function that is called by name is looked up ahead of the passes as well, and
            // what it is given follows that.
            ("map(xs, x => f(m[0], x))", (1, 0)),
            // Nothing is read ahead of something the pass does first that may fail.
            ("map(xs, x => x[0] + m[0])", (0, 2)),
            ("map(xs, x => f(x) + m[0])", (0, 1)),
            ("map(xs, x => (x.f)(m[0]))", (0, 2)),
            // Nor is what differs from pass to pass.
            ("map(xs, x => x.a + x.b)", (0, 2)),
            ("reduce(xs, (acc, x) => acc[0] + x, [0])", (0, 1)),
            ("map(xs, (x, i) => m[i] + m[x])", (0, 2)),
        ] {
            assert_eq!(reads_of_innermost_loop(code), expected, "{code}");
        }
    }

    #[test]
    fn what_a_callback_reads_from_outside_is_the_same_on_every_pass() {
        for (code, expected) in [
            // A row of a matrix, under a loop over its columns.
            (
                "m = [[1, 2], [3, 4]]\nmap(range(2), i => reduce(range(2), (acc, j) => acc + m[i][j] * 10, 0))",
                "[30, 70]",
            ),
            (
                "f = (m, k) => map(range(2), i => reduce(range(2), (acc, j) => acc + m[i][j] * k, 0))\nf([[1, 2], [3, 4]], 10)",
                "[30, 70]",
            ),
            // Fields and elements, on either side of what differs.
            (
                "r = {a: {b: [5, 6]}}\nmap([0, 1], i => r.a.b[1] * 10 + r.a.b[i])",
                "[65, 66]",
            ),
            (
                "r = {a: {b: [5, 6]}}\nmap([0, 1], i => r.a.b[i] * 10 + r.a.b[1])",
                "[56, 66]",
            ),
            (
                "r = {k: \"b\", b: 3}\nmap([1, 2], x => r[r.k] - x)",
                "[2, 1]",
            ),
            // The whole body, and the whole condition.
            ("a = [7, 8]\nmap([1, 2], x => a[1])", "[8, 8]"),
            ("a = [7, 8]\nfilter([1, 2], x => a[0] < a[1])", "[1, 2]"),
            ("a = [7, 8]\nevery([1, 2], x => a[0] > a[1])", "false"),
            ("a = [7, 8]\nsome([1, 2], x => a[0] + 1 == a[1])", "true"),
            ("a = [7, 8]\nreduce([1, 2], (acc, x) => a[1], 0)", "8"),
            // An argument of a call, a condition, and the list of a loop in the body.
            ("a = [7, 8]\nmap([1, 2], x => max(a[0], x * 4))", "[7, 8]"),
            (
                "a = [7, 8]\nmap([6, 9], x => if a[1] > x then a[0] else x)",
                "[7, 9]",
            ),
            (
                "m = [[1, 2], [3, 4]]\nmap([10, 20], x => map(m[1], y => y + x))",
                "[[13, 14], [23, 24]]",
            ),
            (
                "m = [[1, 2], [3, 4]]\nmap([10, 20], x => reduce(m[0], (acc, y) => acc + y + x, m[1][1]))",
                "[27, 47]",
            ),
        ] {
            assert_evaluates_to(code, expected);
        }
    }

    #[test]
    fn a_list_computed_ahead_of_the_passes_is_not_written_by_them() {
        assert_evaluates_to(
            "a = [1, 2, 3]\nmap([1, 2], x => sum(a * 2 + x))",
            "[15, 18]",
        );
        assert_evaluates_to(
            "a = [1, 2, 3]\nmap([1, 2], x => a * 2 + x)",
            "[[3, 5, 7], [4, 6, 8]]",
        );
        assert_evaluates_to(
            "a = [1, 2, 3]\nmap([1, 2], x => (a * 2 + a) * x - 1)",
            "[[2, 5, 8], [5, 11, 17]]",
        );
    }

    #[test]
    fn a_variable_of_the_loop_is_not_taken_for_one_bound_outside() {
        // The parameter of the callback hides a variable of the same name.
        assert_evaluates_to(
            "i = 1\nmap([[10, 20], [30, 40]], a => map(range(2), i => a[i]))",
            "[[10, 20], [30, 40]]",
        );
        // A variable of a block is bound anew on every pass of the loop around the block.
        assert_evaluates_to(
            "map([1, 2], x => do { a = [x, x * 2]; return map([0, 1], j => a[1] + a[j]) })",
            "[[3, 4], [6, 8]]",
        );
        assert_evaluates_to(
            "a = [100, 200]\nmap([1, 2], x => do { a = [x, x * 2]; return a[1] + x })",
            "[3, 6]",
        );
        // A variable that is bound again before the loop has its last value in the loop.
        assert_evaluates_to(
            "do { t = [1, 2]; t = t * 10; return map([0, 1], i => t[1] + i) }",
            "[20, 21]",
        );
        // The accumulator differs from pass to pass.
        assert_evaluates_to("reduce([1, 2, 3], (acc, x) => [acc[0] + x], [0])", "[6]");
    }

    #[test]
    fn a_loop_without_passes_computes_nothing() {
        for (code, expected) in [
            ("map([], x => missing[0] + x)", "[]"),
            ("map(range(0), x => null[0])", "[]"),
            ("filter([], x => null.f)", "[]"),
            ("reduce([], (acc, x) => \"s\" - 1, 7)", "7"),
            ("every(range(2, 2), x => 1[0])", "true"),
            ("some([], x => 1[0])", "false"),
            (
                "a = 5\nmap([1, 2], x => map(range(0), y => a[0] + y))",
                "[[], []]",
            ),
        ] {
            assert_evaluates_to(code, expected);
        }
    }

    #[test]
    fn the_first_thing_to_fail_in_a_pass_is_what_fails() {
        for (code, expected) in [
            ("map([1], x => x[0] + null[0])", "but got a number"),
            ("map([1], x => null[0] + x[0])", "but got a null"),
            (
                "map([1], x => x.f + null[0])",
                "expected a record, but got a number",
            ),
            ("map([1], x => (x + 1) * null[0])", "but got a null"),
            ("map([1], x => (\"s\" - x) * null[0])", "but got a string"),
            ("map([1], x => null[x] + \"s\"[null])", "but got a null"),
            ("map([1], x => max(x.f, null.g))", "but got a number"),
            ("map([1], x => max(null.g, x.f))", "but got a null"),
            (
                "map([1], x => if x.f > null.g then 1 else 2)",
                "but got a number",
            ),
            (
                "map([1], x => if null.g > x.f then 1 else 2)",
                "but got a null",
            ),
            (
                "map([1], x => map(x.f, y => y) + null.g)",
                "but got a number",
            ),
            (
                "map([1], x => reduce(null.g, (acc, y) => y, x.f))",
                "but got a null",
            ),
            (
                "map([1], x => reduce(range(x.f), (acc, y) => y, null.g))",
                "but got a number",
            ),
            (
                "map([1], x => missing + null.g)",
                "unknown identifier: missing",
            ),
            ("map([1], x => null.g + missing)", "but got a null"),
        ] {
            assert_fails_with(code, expected);
        }
    }

    #[test]
    fn what_fails_ahead_of_the_passes_fails_as_part_of_the_callback() {
        assert_fails_with(
            "map([1, 2], x => null[0] + x)",
            "in built-in function \"map\": in anonymous function: expected a record, list, or string, but got a null",
        );
        assert_fails_with(
            "f = xs => reduce(xs, (acc, x) => acc + null.g, 0)\nf([1])",
            "in function \"f\": in built-in function \"reduce\": in anonymous function: expected a record, but got a null",
        );
        assert_fails_with(
            "map([1], x => filter([1], y => missing[0] > y))",
            "in built-in function \"map\": in anonymous function: in built-in function \"filter\": in anonymous function: unknown identifier: missing",
        );
    }
}

// A power of two divides a number without a division. The quotient is the product with its
// reciprocal, and what a whole number leaves is in its lowest bits, so a condition that asks
// about the remainder reads them and never computes it. Nothing may tell either from dividing.
#[cfg(test)]
mod power_of_two_tests {
    use crate::bytecode::{Instr, Op};
    use crate::compiler::compile_chunk;
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::intern::Symbol;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code` against `bindings`, up to the value of the last one.
    fn evaluate(
        code: &str,
        heap: &Rc<RefCell<Heap>>,
        bindings: &Rc<Environment>,
    ) -> Result<Value, RuntimeError> {
        let statements =
            parse_program(code).unwrap_or_else(|| panic!("the fast parser does not read {code}"));
        let source: Rc<str> = code.into();

        let mut result = Value::Null;
        for statement in statements {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };
            result = evaluate_ast(
                &expr,
                Rc::clone(heap),
                Rc::clone(bindings),
                0,
                Rc::clone(&source),
            )?;
        }

        Ok(result)
    }

    /// What the statements of `code` come to, rendered: the value, or the error with the
    /// place it points at.
    fn outcome(code: &str) -> String {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());

        match evaluate(code, &heap, &bindings) {
            Ok(value) => value.stringify_internal(&heap.borrow()),
            Err(error) => format!("{} at {:?}", error.message, error.span),
        }
    }

    /// The operations of the instructions that the expression `code` compiles to.
    fn operations(code: &str) -> Vec<Op> {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let [Statement::Expression(expr)] = &statements[..] else {
            panic!("{code} is not a single expression");
        };
        let chunk = compile_chunk(expr, true, &Environment::new()).unwrap();

        let mut operations = Vec::new();
        let mut pc = 0;
        while pc < chunk.code.len() {
            // SAFETY: `pc` only lands on instruction words, as number operands are skipped.
            let instr = unsafe { Instr::decode(chunk.code[pc]) };
            operations.push(instr.op);
            pc += 1 + instr.operand_words();
        }

        operations
    }

    /// Numbers of every kind: the ones at the edges, whole numbers around every power of two
    /// and halfway between them, and a couple of thousand whose bits are picked at random,
    /// which covers every sign and every exponent there is.
    fn numbers() -> Vec<f64> {
        let mut numbers = vec![
            0.0,
            0.1,
            0.5,
            1.0 / 3.0,
            1e15,
            1e16,
            1e19,
            1e300,
            f64::EPSILON,
            f64::MIN_POSITIVE,
            f64::MIN_POSITIVE / 2.0,
            f64::MIN_POSITIVE * 1.5,
            f64::from_bits(1),
            f64::from_bits(2),
            f64::from_bits(3),
            f64::MAX,
            f64::MAX / 2.0,
            f64::INFINITY,
            f64::NAN,
        ];

        for whole in 0..=70 {
            numbers.extend([whole as f64, whole as f64 + 0.5, whole as f64 + 0.25]);
        }

        for exponent in 0..=70 {
            let power = 2f64.powi(exponent);
            numbers.extend([-3.0, -2.0, -1.0, -0.5, 0.5, 1.0, 2.0, 3.0].map(|near| power + near));
            numbers.push(power * 1.5);
        }

        let mut bits = 0x9E37_79B9_7F4A_7C15_u64;
        for _ in 0..2000 {
            bits ^= bits << 13;
            bits ^= bits >> 7;
            bits ^= bits << 17;
            numbers.push(f64::from_bits(bits));

            // The same, as a whole number of every size that there is a remainder to read from.
            numbers.push((bits >> (bits % 64)) as f64);
        }

        let negated = numbers.iter().map(|number| -number).collect::<Vec<_>>();
        numbers.extend(negated);

        numbers
    }

    /// Whether nothing can tell `actual` from `expected`: they are the same number down to
    /// the sign of a zero, or neither of them is a number.
    fn same(actual: f64, expected: f64) -> bool {
        actual.to_bits() == expected.to_bits() || (actual.is_nan() && expected.is_nan())
    }

    /// The numbers in the list that `code` comes to, with `xs` bound to `numbers`.
    fn over(numbers: &[f64], code: &str) -> Vec<f64> {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let list = numbers.iter().copied().map(Value::Number).collect();
        let xs = heap.borrow_mut().insert_list(list);
        bindings.insert(Symbol::intern("xs"), xs);

        let value = evaluate(code, &heap, &bindings).unwrap();
        let heap = heap.borrow();

        value
            .as_list(&heap)
            .unwrap()
            .iter()
            .map(|value| value.as_number().unwrap())
            .collect()
    }

    #[test]
    fn a_division_by_a_power_of_two_is_a_multiplication() {
        let divides = |code: &str| {
            let operations = operations(code);

            (
                operations.contains(&Op::DivK),
                operations.contains(&Op::MulK),
            )
        };

        for divisor in [
            "2",
            "4",
            "8",
            "0.5",
            "0.25",
            "1",
            "1024",
            "4503599627370496",
            "2.2250738585072014e-308",
            "4.49423283715579e307",
        ] {
            assert_eq!(
                divides(&format!("x / {divisor}")),
                (false, true),
                "{divisor}"
            );
        }

        // A number that is no power of two, and a power of two whose reciprocal is not one
        // that multiplying by gets the same out of: these are still divided by.
        for divisor in [
            "3",
            "10",
            "0.1",
            "6",
            "0",
            "1e308",
            "5e-324",
            "1.1125369292536007e-308",
            "8.98846567431158e307",
        ] {
            assert_eq!(
                divides(&format!("x / {divisor}")),
                (true, false),
                "{divisor}"
            );
        }
    }

    #[test]
    fn it_comes_to_the_number_that_dividing_comes_to() {
        let numbers = numbers();

        for exponent in [
            -1074, -1073, -1023, -1022, -1021, -52, -3, -2, -1, 0, 1, 2, 3, 10, 31, 32, 52, 53, 63,
            64, 1021, 1022, 1023,
        ] {
            let divisor = 2f64.powi(exponent);
            let expected = numbers.iter().map(|number| number / divisor);

            for code in [
                // One number at a time, in a function and in a callback compiled in place.
                format!("halve = x => x / {divisor:e}\nmap(xs, halve)"),
                format!("map(xs, x => x / {divisor:e})"),
                // A whole list at once, by itself and in a run of operators.
                format!("xs / {divisor:e}"),
                format!("(xs * 1 / {divisor:e}) + 0"),
            ] {
                let actual = over(&numbers, &code);
                assert_eq!(actual.len(), numbers.len(), "{code}");

                for ((actual, expected), number) in
                    actual.into_iter().zip(expected.clone()).zip(&numbers)
                {
                    // Adding zero in the last of the programs turns a negative zero around.
                    let expected = if code.ends_with("+ 0") {
                        expected * 1.0 + 0.0
                    } else {
                        expected
                    };

                    assert!(
                        same(actual, expected),
                        "{number:e} / {divisor:e} is {expected:e}, not {actual:e} ({code})"
                    );
                }
            }
        }
    }

    #[test]
    fn what_is_not_a_number_is_divided() {
        assert_eq!(outcome("[6, 8, 1] / 2"), "[3, 4, 0.5]");
        assert_eq!(outcome("sum([6, 8, 1] / 4)"), "3.75");
        assert_eq!(outcome("xs = [6, 8, 1]\n(xs + 2) / 2 - 1"), "[3, 4, 0.5]");

        // What cannot be divided fails the way it does for any other divisor.
        for dividend in [
            "\"a\"",
            "null",
            "true",
            "[1, \"a\"]",
            "[1, null, 3]",
            "{a: 1}",
            "(x => x)",
            "[[1, 2], [3, \"a\"]]",
        ] {
            for shape in [
                "x = {dividend}\nx / {divisor}",
                "f = x => x / {divisor}\nf({dividend})",
                "x = {dividend}\n1 + x / {divisor} * 3",
                "x = {dividend}\nsum(x / {divisor})",
                "map([{dividend}], x => x / {divisor})",
            ] {
                let program = |divisor: &str| {
                    shape
                        .replace("{dividend}", dividend)
                        .replace("{divisor}", divisor)
                };

                // A divisor of the same length, as the error points at the whole division.
                for (power_of_two, other) in [("2", "3"), ("8", "7"), ("0.5", "0.3")] {
                    let (expected, actual) =
                        (outcome(&program(other)), outcome(&program(power_of_two)));

                    assert!(
                        expected.contains("expected a number"),
                        "{shape}: {expected}"
                    );
                    assert_eq!(actual, expected, "{shape} of {dividend} by {power_of_two}");
                }
            }
        }
    }

    #[test]
    fn a_condition_on_the_remainder_by_a_power_of_two_computes_no_remainder() {
        let fused = |condition: &str| {
            let operations = operations(&format!("if {condition} then 1 else 2"));

            (
                operations.contains(&Op::JmpNotRemEq),
                operations.contains(&Op::JmpNotRemNe),
                operations.contains(&Op::ModK),
            )
        };

        for (divisor, remainder) in [
            ("2", "0"),
            ("2", "1"),
            ("1", "0"),
            ("4", "3"),
            ("1024", "512"),
            ("4503599627370496", "4503599627370495"),
        ] {
            assert_eq!(
                fused(&format!("x % {divisor} == {remainder}")),
                (true, false, false),
                "{divisor} {remainder}"
            );
            assert_eq!(
                fused(&format!("(x + 1) % {divisor} != {remainder}")),
                (false, true, false),
                "{divisor} {remainder}"
            );
        }

        // Every other remainder is computed: by what is no power of two or too large a one,
        // one that a power of two never leaves, and one that is ordered rather than matched.
        for condition in [
            "x % 3 == 0",
            "x % 6 != 2",
            "x % 0 == 0",
            "x % 0.5 == 0",
            "x % 1.5 == 1",
            "x % 9007199254740992 == 0",
            "x % 2 == 2",
            "x % 2 == 0.5",
            "x % 4 != 4",
            "x % 2 < 1",
            "x % 2 >= 1",
            "x % y == 0",
            "x % 2 == y",
            "2 % x == 0",
            "0 == x % 2",
        ] {
            let (equal, unequal, _) = fused(condition);
            assert!(!equal && !unequal, "{condition}");
        }
    }

    #[test]
    fn the_condition_holds_for_the_numbers_that_leave_the_remainder() {
        let numbers = numbers();

        for exponent in [0, 1, 2, 3, 4, 10, 31, 32, 33, 51, 52] {
            let divisor = 2f64.powi(exponent);
            let remainders = [0.0, 1.0, 2.0, 3.0, divisor / 2.0, divisor - 1.0]
                .into_iter()
                .filter(|remainder| *remainder < divisor && remainder.fract() == 0.0);

            for remainder in remainders {
                for (operator, equal) in [("==", true), ("!=", false)] {
                    let condition = format!("x % {divisor} {operator} {remainder}");

                    for code in [
                        format!("map(xs, x => if {condition} then 1 else 0)"),
                        format!("test = x => if {condition} then 1 else 0\nmap(xs, test)"),
                        format!(
                            "test = (x, n) => if n == 0 then 0 else if {condition} then 1 else test(x, n - 1)\nmap(xs, x => test(x, 2))"
                        ),
                    ] {
                        let actual = over(&numbers, &code);
                        assert_eq!(actual.len(), numbers.len(), "{code}");

                        for (actual, number) in actual.into_iter().zip(&numbers) {
                            let left = number % divisor;

                            assert_eq!(
                                actual == 1.0,
                                (left == remainder) == equal,
                                "{number:e} % {divisor} is {left}, to be {operator} {remainder} ({code})"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn what_is_not_a_number_has_its_remainder_taken_and_compared() {
        // What has no remainder, and a remainder that is no condition (the list of answers
        // that a list comes to), fail the way they do with a divisor that the remainder is
        // computed for.
        for dividend in [
            "\"a\"",
            "null",
            "true",
            "[1, 2, 3]",
            "[4, 8]",
            "[]",
            "[1, \"a\"]",
            "[[1, 2], [3, 4]]",
            "{a: 1}",
            "(x => x)",
        ] {
            for shape in [
                "x = {dividend}\nif x % {divisor} {operator} 0 then 1 else 2",
                "f = x =>\n  if x == 1 then 0\n  else if x % {divisor} {operator} 0 then 1\n  else 2\nf({dividend})",
                "f = x => if x % {divisor} {operator} 0 then 1 else 2\nf({dividend})",
                "f = (x, n) => if n == 0 then 0 else if x % {divisor} {operator} 0 then f(x, n - 1) else f(x, n - 1)\nf({dividend}, 3)",
                "map([1, {dividend}], x => if x % {divisor} {operator} 0 then 1 else 2)",
                "x = {dividend}\nmap([1, 2], y => if x % {divisor} {operator} 0 then y else -y)",
                "filter([{dividend}], x => if x % {divisor} {operator} 0 then true else false)",
            ] {
                for operator in ["==", "!="] {
                    let program = |divisor: &str| {
                        shape
                            .replace("{dividend}", dividend)
                            .replace("{divisor}", divisor)
                            .replace("{operator}", operator)
                    };

                    for (power_of_two, other) in [("2", "3"), ("4", "5"), ("8", "7")] {
                        let (expected, actual) =
                            (outcome(&program(other)), outcome(&program(power_of_two)));

                        assert!(expected.contains("expected a "), "{shape}: {expected}");
                        assert_eq!(actual, expected, "{shape} of {dividend} by {power_of_two}");
                    }
                }
            }
        }
    }
}

// A condition computes on the way to its answer what its branches often compute again, and
// the two operands of an operator can be one expression twice. The first value is kept and
// taken where the expression is repeated, as long as its names still mean the same variables
// there. Nothing may tell a value that was taken from one that was computed.
#[cfg(test)]
mod kept_value_tests {
    use crate::ast::Expr;
    use crate::bytecode::{Instr, Op};
    use crate::compiler::compile_lambda;
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// Evaluate the statements of `code`, up to the value of the last one.
    fn evaluate(code: &str, heap: &Rc<RefCell<Heap>>) -> Result<Value, RuntimeError> {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let bindings = Rc::new(Environment::new());
        let source: Rc<str> = code.into();

        let mut result = Value::Null;
        for statement in statements {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };
            result = evaluate_ast(
                &expr,
                Rc::clone(heap),
                Rc::clone(&bindings),
                0,
                Rc::clone(&source),
            )?;
        }

        Ok(result)
    }

    /// What the statements of `code` come to, rendered: the value, or the error with the
    /// place it points at.
    fn outcome(code: &str) -> String {
        let heap = Rc::new(RefCell::new(Heap::new()));

        match evaluate(code, &heap) {
            Ok(value) => value.stringify_internal(&heap.borrow()),
            Err(error) => format!("{} at {:?}", error.message, error.span),
        }
    }

    fn assert_evaluates_to(code: &str, expected: &str) {
        assert_eq!(outcome(code), expected, "{code}");
    }

    /// How many instructions with the operation `op` the function `code` compiles to.
    fn count(code: &str, op: Op) -> usize {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let [Statement::Expression(expr)] = &statements[..] else {
            panic!("{code} is not a single expression");
        };
        let Expr::Lambda { args, body, .. } = &expr.node else {
            panic!("{code} is not a function");
        };
        let proto = compile_lambda(&Rc::new(args.clone()), body, true).unwrap();

        let mut count = 0;
        let mut pc = 0;
        while pc < proto.code.len() {
            // SAFETY: `pc` only lands on instruction words, as number operands are skipped.
            let instr = unsafe { Instr::decode(proto.code[pc]) };
            count += usize::from(instr.op == op);
            pc += 1 + instr.operand_words();
        }

        count
    }

    #[test]
    fn a_branch_takes_what_its_condition_computed() {
        for (code, op, expected) in [
            // The squares of a point, compared to a bound and then put to use.
            (
                "(zr, zi) => if zr * zr + zi * zi > 4 then 0 else zr * zr - zi * zi",
                Op::Mul,
                2,
            ),
            // A field and an element, in either branch and in both.
            ("(a, b) => if a.x > b then a.x else b", Op::GetField, 1),
            (
                "(xs, i) => if xs[i] > 0 then 0 else 0 - xs[i]",
                Op::Index,
                1,
            ),
            (
                "(a, b) => if a % b > 1 then a % b + 1 else a % b - 1",
                Op::Mod,
                1,
            ),
            // The whole condition.
            ("(a, b) => if a + b then a + b else 0", Op::Add, 1),
            // A part of a part, where only that is repeated, and the larger part where both are.
            (
                "(a, b) => if (a - b) * (a + b) > 4 then a - b else 0",
                Op::Sub,
                1,
            ),
            (
                "(a, b) => if (a - b) * 3 > 4 then (a - b) * 3 + (a - b) else 0",
                Op::Sub,
                1,
            ),
            // The condition of a branch, and a branch of a branch.
            (
                "(a, b) => if a * b > 10 then 1 else if a * b > 5 then 2 else a * b",
                Op::Mul,
                1,
            ),
            (
                "(a, b) => if a > b then (if a * b > 5 then a * b else b) else a",
                Op::Mul,
                1,
            ),
            // In what a branch calls with, builds and binds.
            (
                "(n, a) => if n * a > 100 then n else f(n * a, [n * a], {k: n * a})",
                Op::Mul,
                1,
            ),
            (
                "(n, a) => if n * a > 100 then n else do { m = n * a; return m + n * a }",
                Op::Mul,
                1,
            ),
            // In a callback that is compiled in place, and around one.
            (
                "(xs, k) => map(xs, x => if x * k > 10 then x * k else 0)",
                Op::Mul,
                1,
            ),
            (
                "(xs, k) => if k * k > 10 then map(xs, x => x + k * k) else xs",
                Op::Mul,
                1,
            ),
        ] {
            assert_eq!(count(code, op), expected, "{code}");
        }
    }

    #[test]
    fn an_operand_that_repeats_the_other_is_computed_once() {
        for (code, op, expected) in [
            ("(a, b) => (a - b) * (a - b)", Op::Sub, 1),
            ("(a, i) => a[i] + a[i]", Op::Index, 1),
            (
                "(a, b) => (a.x * b.y + 1) / (a.x * b.y + 1)",
                Op::GetField,
                2,
            ),
            // Operands that only look alike in part are two computations.
            ("(a, b) => (a - b) * (b - a)", Op::Sub, 2),
            ("(a, b) => (a - b) * (a - b + 1)", Op::Sub, 2),
        ] {
            assert_eq!(count(code, op), expected, "{code}");
        }
    }

    #[test]
    fn what_calls_a_function_is_computed_every_time() {
        for (code, op, expected) in [
            ("a => if f(a) > 3 then f(a) else 0", Op::Call, 2),
            ("a => if f(a) + 1 > 3 then f(a) + 1 else 0", Op::Call, 2),
            ("a => (f(a) + 1) * (f(a) + 1)", Op::Call, 2),
            (
                "(xs, g) => ((xs via g) + 1) * ((xs via g) + 1)",
                Op::AddK,
                2,
            ),
            (
                "(xs, g) => if sum(xs via g) > 3 then xs via g else xs",
                Op::Via,
                2,
            ),
            (
                "(x, g) => if (x into g) + 1 > 3 then (x into g) + 1 else 0",
                Op::AddK,
                2,
            ),
            (
                "(xs, g) => if (xs where g)[0] > 3 then (xs where g)[0] else 0",
                Op::Index,
                2,
            ),
        ] {
            assert_eq!(count(code, op), expected, "{code}");
        }
    }

    #[test]
    fn a_remainder_that_the_condition_does_not_compute_is_computed_by_the_branch() {
        assert_eq!(
            count("x => if x % 2 == 0 then x % 2 else x % 2 + 1", Op::ModK),
            2
        );
        assert_evaluates_to(
            "f = x => if x % 2 == 0 then [x % 2] else x % 2 + 1\n[f(4), f(7), f(-3)]",
            "[[0], 2, 0]",
        );
        assert_evaluates_to(
            "f = (x, y) => if (x + y) % 4 != 1 then [(x + y) % 4, x + y] else x + y\n[f(1, 2), f(2, 3), f(-3, 0)]",
            "[[3, 3], 5, [-3, -3]]",
        );
    }

    #[test]
    fn a_function_with_many_registers_keeps_nothing() {
        let function = |params: usize| {
            let params = (0..params)
                .map(|i| format!("p{i}"))
                .collect::<Vec<_>>()
                .join(", ");

            format!("({params}) => if p0 * p1 > 4 then p0 * p1 else 0")
        };

        assert_eq!(count(&function(100), Op::Mul), 1);
        assert_eq!(count(&function(127), Op::Mul), 1);
        assert_eq!(count(&function(128), Op::Mul), 2);
        assert_eq!(count(&function(250), Op::Mul), 2);

        for params in [100, 127, 128, 250] {
            let args = (0..params)
                .map(|i| (i + 2).to_string())
                .collect::<Vec<_>>()
                .join(", ");

            assert_evaluates_to(&format!("f = {}\nf({args})", function(params)), "6");
        }
    }

    #[test]
    fn a_name_that_means_another_variable_reads_that_variable() {
        for (code, expected) in [
            // A parameter of a callback, compiled in place or called.
            (
                "f = (a, b) => if a * b > 3 then map([1, 2], a => a * b) else 0\nf(2, 5)",
                "[5, 10]",
            ),
            (
                "f = (a, b) => if a * b > 3 then reduce([1, 2], (b, x) => a * b + x, 1) else 0\nf(2, 5)",
                "8",
            ),
            (
                "f = (a, b) => if a * b > 3 then ((a) => a * b)(7) else 0\nf(2, 5)",
                "35",
            ),
            (
                "f = (a, b) => if a * b > 3 then [1, 2] via (b => a * b) else 0\nf(2, 5)",
                "[2, 4]",
            ),
            // A variable of a block, before and after it is bound.
            (
                "f = (a, b) => if a * b > 3 then do { x = a * b; a = 10; return [x, a * b] } else 0\nf(2, 5)",
                "[10, 50]",
            ),
            (
                "f = (a, b) => if a * b > 3 then do { a = a * b; b = a * b; return [a, b, a * b] } else 0\nf(2, 5)",
                "[10, 50, 500]",
            ),
            (
                "f = (a, b) => if a * b > 3 then do { t = do { a = b; return a * b }; return [t, a * b] } else 0\nf(2, 5)",
                "[25, 10]",
            ),
            // A variable that the branch of a condition in the branch hides again.
            (
                "f = (a, b) => if a * b > 3 then do { a = 3; return if a * b > 3 then do { a = 4; return a * b } else a * b } else 0\n[f(2, 5), f(4, 1)]",
                "[20, 3]",
            ),
            // The same names in the same places still take the value.
            (
                "f = (a, b) => if a * b > 3 then do { c = a * b; d = map([1], x => a * b + x); return [c, d, a * b] } else a * b\n[f(2, 5), f(1, 2)]",
                "[[10, [11], 10], 2]",
            ),
        ] {
            assert_evaluates_to(code, expected);
        }
    }

    #[test]
    fn a_value_that_is_kept_is_not_written_over() {
        for (code, expected) in [
            // A list that arithmetic over lists would otherwise put its result into.
            (
                "f = xs => if sum(xs * 2) > 3 then [xs * 2 + 1, xs * 2, (xs * 2) * (xs * 2)] else xs * 2 - 1\n[f([1, 2]), f([0, 1])]",
                "[[[3, 5], [2, 4], [4, 16]], [-1, 1]]",
            ),
            (
                "f = xs => (xs + 1) * (xs + 1) - (xs + 1)\nf([1, 2, 3])",
                "[2, 6, 12]",
            ),
            (
                "f = (s, t) => (s + t) + (s + t)\nf(\"ab\", \"c\")",
                "abcabc",
            ),
            // A value that a call in the branch works with.
            (
                "f = (n, k) => if n * k > 50 then n * k else f(n * k, k) + n * k\nf(1, 3)",
                "120",
            ),
            (
                "f = (zr, zi, i) => if zr * zr + zi * zi > 4 or i == 0 then [zr * zr, zi * zi, i] else f(zr * zr - zi * zi + 1, 2 * zr * zi, i - 1)\nf(0, 0, 9)",
                "[25, 0, 6]",
            ),
        ] {
            assert_evaluates_to(code, expected);
        }
    }

    #[test]
    fn a_value_is_kept_for_as_long_as_the_branches_last() {
        // What follows the branches computes the expression anew, by which time the register
        // that kept its value holds something else.
        for (code, expected) in [
            (
                "f = (a, b) => [if a * b > 3 then a * b else 0, a + b, a * b]\nf(2, 5)",
                "[10, 7, 10]",
            ),
            (
                "f = (a, b) => (if a * b > 3 then a * b else 0) + (a - b) * (a * b)\nf(2, 5)",
                "-20",
            ),
            (
                "f = (a, b) => do { x = if a * b > 3 then a * b else 0; y = a + b; return [x, y, a * b] }\nf(2, 5)",
                "[10, 7, 10]",
            ),
            (
                "f = (a, b) => if (if a * b > 3 then a * b else 0) > a + b then [a + b, a * b] else [a * b, a + b]\n[f(2, 5), f(1, 1)]",
                "[[7, 10], [1, 2]]",
            ),
            (
                "f = (a, b) => map([1, 2], x => (if a * b > x then a * b else x) + (x + 1) * (a * b))\nf(2, 5)",
                "[30, 40]",
            ),
        ] {
            assert_evaluates_to(code, expected);
        }
    }

    #[test]
    fn many_conditions_keep_values_at_once() {
        // More conditions around one point than there are values kept at once, each with a
        // value for its branches, and more values in one condition than it has kept.
        let depth = 24;
        let mut body = "[a, b]".to_string();
        for level in (0..depth).rev() {
            body = format!(
                "if a * {level} + b > {limit} then [{level}, a * {level} + b] else {body}",
                limit = 100 - level
            );
        }

        assert_evaluates_to(
            &format!("f = (a, b) => {body}\n[f(1, 200), f(4, 3), f(0, 0)]"),
            "[[0, 200], [20, 83], [0, 0]]",
        );

        assert_evaluates_to(
            "f = (a, b) => if (a + 1) * (a + 2) * (a + 3) * (a + 4) * (a + 5) * (a + 6) > b then [a + 1, a + 2, a + 3, a + 4, a + 5, a + 6] else [a + 6, a + 5, a + 4, a + 3, a + 2, a + 1]\n[f(1, 0), f(1, 1e9)]",
            "[[2, 3, 4, 5, 6, 7], [7, 6, 5, 4, 3, 2]]",
        );
    }

    #[test]
    fn a_condition_fails_where_it_does_whatever_its_branches_repeat() {
        // An expression of the same length in the place of the repeated one, as the error
        // points at the code.
        for (kept, other) in [
            (
                "f = a => if a.x > 1 then a.x else 0\nf(5)",
                "f = a => if a.x > 1 then a.y else 0\nf(5)",
            ),
            (
                "f = (a, b) => if a * b > 1 then a * b else a * b + 1\nf(2, \"s\")",
                "f = (a, b) => if a * b > 1 then a + b else a + b + 1\nf(2, \"s\")",
            ),
            (
                "f = (a, i) => if a[i] + a[0] > 1 then a[i] else a[0]\nf([1, 2], 7)",
                "f = (a, i) => if a[i] + a[0] > 1 then a[1] else a[1]\nf([1, 2], 7)",
            ),
            (
                "f = (a, b) => (a - b) * (a - b)\nf(1, null)",
                "f = (a, b) => (a - b) * (a + b)\nf(1, null)",
            ),
            (
                "map([1, null], x => if x * 2 > 1 then x * 2 else 0)",
                "map([1, null], x => if x * 2 > 1 then x * 3 else 0)",
            ),
        ] {
            let (actual, expected) = (outcome(kept), outcome(other));

            assert!(expected.contains(" at "), "{other}: {expected}");
            assert_eq!(actual, expected, "{kept}");
        }

        // What only a branch computes fails in the branch.
        assert!(
            outcome("f = (a, b) => if a > 0 then a * b else 0\n[f(0, null), f(1, null)]")
                .contains("expected a number")
        );
        assert_evaluates_to("f = (a, b) => if a > 0 then a * b else 0\nf(0, null)", "0");
    }
}

// A list that is made from another one reads the elements of that list where it can instead
// of copying them: one that starts with a spread, `concat`, `tail`, `slice` and `chunk`.
// A list that keeps growing this way takes time in proportion to its length, not to the
// square of it. Nothing may tell a list that shares its elements from one that has its own.
#[cfg(test)]
mod shared_list_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::{Heap, HeapValue};
    use crate::list::List;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A program that is evaluated a few statements at a time.
    struct Session {
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<Environment>,
    }

    impl Session {
        fn new() -> Self {
            Self {
                heap: Rc::new(RefCell::new(Heap::new())),
                bindings: Rc::new(Environment::new()),
            }
        }

        /// Evaluate the statements of `code`, up to the value of the last one.
        fn evaluate(&self, code: &str) -> Result<Value, RuntimeError> {
            let statements = parse_program(code).expect("a program the fast parser reads");
            let source: Rc<str> = code.into();

            let mut result = Value::Null;
            for statement in statements {
                let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement
                else {
                    continue;
                };
                result = evaluate_ast(
                    &expr,
                    Rc::clone(&self.heap),
                    Rc::clone(&self.bindings),
                    0,
                    Rc::clone(&source),
                )?;
            }

            Ok(result)
        }

        /// What the statements of `code` come to, rendered: the value, or the error.
        fn outcome(&self, code: &str) -> String {
            match self.evaluate(code) {
                Ok(value) => value.stringify_internal(&self.heap.borrow()),
                Err(error) => error.message,
            }
        }

        /// The list that `code` comes to.
        fn list(&self, code: &str) -> List {
            let value = self.evaluate(code).unwrap();

            value.as_list(&self.heap.borrow()).unwrap().clone()
        }

        /// Drop everything that the variables of the program do not lead to.
        fn compact(&self) {
            self.heap.borrow_mut().compact(&self.bindings, &mut []);
        }

        /// The number of buffers that the lists of at least `len` elements in the heap read
        /// their elements from.
        fn buffers(&self, len: usize) -> usize {
            let heap = self.heap.borrow();
            let mut distinct: Vec<&List> = Vec::new();

            for slot in 0..heap.len() {
                if let Some(HeapValue::List(list)) = heap.get(slot)
                    && list.len() >= len
                    && !distinct.iter().any(|known| known.shares_buffer_with(list))
                {
                    distinct.push(list);
                }
            }

            distinct.len()
        }
    }

    fn outcome(code: &str) -> String {
        Session::new().outcome(code)
    }

    #[test]
    fn a_list_that_starts_with_a_spread_holds_what_it_is_made_of() {
        for (code, expected) in [
            ("[...[1, 2], 3]", "[1, 2, 3]"),
            ("[...[], 3]", "[3]"),
            ("[...[1, 2]]", "[1, 2]"),
            ("[...[]]", "[]"),
            ("[...[1, 2], ...[3, 4]]", "[1, 2, 3, 4]"),
            ("[...[1, 2], 3, ...[4], 5]", "[1, 2, 3, 4, 5]"),
            ("[...[1], ...[2], ...[3]]", "[1, 2, 3]"),
            ("[0, ...[1, 2], 3]", "[0, 1, 2, 3]"),
            ("[...\"ab\", 3]", "[a, b, 3]"),
            ("[...[1], ...\"ab\"]", "[1, a, b]"),
            ("[...{a: 1}, 2]", "[[a, 1], 2]"),
            ("[...[1], ...{a: 1}, 2]", "[1, [a, 1], 2]"),
            ("xs = [1, 2]\n[...xs, ...xs]", "[1, 2, 1, 2]"),
            ("xs = [1, 2]\n[...xs, xs]", "[1, 2, [1, 2]]"),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }

        assert_eq!(outcome("[...1, 2]"), "expected a list, record, or string",);
        assert_eq!(
            outcome("[...[1], ...null]"),
            "expected a list, record, or string",
        );
    }

    #[test]
    fn lists_that_follow_the_same_list_do_not_show_up_in_each_other() {
        let session = Session::new();

        session
            .evaluate(
                "base = [1, 2, 3]\n\
                 a = [...base, 4]\n\
                 b = [...base, 5]\n\
                 c = [...a, 6]\n\
                 d = [...a, 7]\n\
                 e = [...c, 8]\n\
                 f = [...a, 9, 10]\n\
                 g = concat(c, [11])\n\
                 h = concat(e, [12], 13)\n\
                 i = [...e, 14]",
            )
            .unwrap();

        assert_eq!(
            session.outcome("[base, a, b, c, d, e, f, g, h, i]"),
            "[[1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 5], [1, 2, 3, 4, 6], [1, 2, 3, 4, 7], \
             [1, 2, 3, 4, 6, 8], [1, 2, 3, 4, 9, 10], [1, 2, 3, 4, 6, 11], \
             [1, 2, 3, 4, 6, 8, 12, 13], [1, 2, 3, 4, 6, 8, 14]]"
        );
    }

    #[test]
    fn every_step_of_a_fold_keeps_the_list_it_made() {
        assert_eq!(
            outcome(
                "reduce(range(5), (state, i) => {now: [...state.now, i], before: [...state.before, state.now]}, {now: [], before: []}).before"
            ),
            "[[], [0], [0, 1], [0, 1, 2], [0, 1, 2, 3]]"
        );

        assert_eq!(
            outcome(
                "reduce(range(6), (state, i) => {now: [...tail(state.now), i], before: [...state.before, state.now]}, {now: [7, 8, 9], before: []}).before"
            ),
            "[[7, 8, 9], [8, 9, 0], [9, 0, 1], [0, 1, 2], [1, 2, 3], [2, 3, 4]]"
        );

        assert_eq!(
            outcome("map(range(4), i => [...[1, 2], i])"),
            "[[1, 2, 0], [1, 2, 1], [1, 2, 2], [1, 2, 3]]"
        );
    }

    #[test]
    fn a_list_that_grows_in_a_fold_is_not_copied_at_every_step() {
        for (code, len, first, last) in [
            (
                "reduce(range(3000), (acc, i) => [...acc, i], [])",
                3000,
                0.0,
                2999.0,
            ),
            (
                "reduce(range(3000), (acc, i) => [...acc, i, -i], [])",
                6000,
                0.0,
                -2999.0,
            ),
            (
                "reduce(range(3000), (acc, i) => concat(acc, [i]), [])",
                3000,
                0.0,
                2999.0,
            ),
            (
                "reduce(range(3000), (acc, i) => concat(acc, [i], [-i]), [])",
                6000,
                0.0,
                -2999.0,
            ),
            (
                "reduce(range(3000), (acc, i) => [...acc, ...[i]], [])",
                3000,
                0.0,
                2999.0,
            ),
            (
                "reduce(range(3000), (acc, i) => if i % 2 == 0 then [...acc, i] else acc, [])",
                1500,
                0.0,
                2998.0,
            ),
            (
                "grow = (n, acc) => if n == 0 then acc else grow(n - 1, [...acc, n])\ngrow(900, [])",
                900,
                900.0,
                1.0,
            ),
        ] {
            let session = Session::new();
            let list = session.list(code);

            assert_eq!(list.len(), len, "{code}");
            assert_eq!(list[0], Value::Number(first), "{code}");
            assert_eq!(list[len - 1], Value::Number(last), "{code}");

            // A buffer for every time that the list outgrows the one before, which is twice
            // as large every time, rather than one for each of the steps.
            let buffers = session.buffers(3);
            assert!(buffers <= 40, "{code}: {buffers} buffers");

            let sharing = {
                let heap = session.heap.borrow();
                (0..heap.len())
                    .filter(|slot| match heap.get(*slot) {
                        Some(HeapValue::List(other)) => other.shares_buffer_with(&list),
                        _ => false,
                    })
                    .count()
            };
            assert!(
                sharing > len / 8,
                "{code}: {sharing} lists share the last buffer"
            );
        }
    }

    #[test]
    fn a_queue_that_drops_from_the_front_is_not_copied_at_every_step() {
        let session = Session::new();
        let queue =
            session.list("reduce(range(5000), (queue, i) => [...tail(queue), i], [0, 0, 0, 0])");

        assert_eq!(queue.len(), 4);
        assert_eq!(queue[3], Value::Number(4999.0));
        assert_eq!(queue[0], Value::Number(4996.0));

        // A tail reads the elements of its list, and the element that follows it is written
        // after them for as long as there is room: a few steps go by for every buffer.
        let buffers = session.buffers(3);
        assert!(buffers < 2600, "{buffers} buffers");
    }

    #[test]
    fn parts_of_a_list_read_the_elements_of_the_list() {
        let session = Session::new();
        session.evaluate("whole = range(10)").unwrap();
        let whole = session.list("whole");

        for (code, expected) in [
            ("tail(whole)", "[1, 2, 3, 4, 5, 6, 7, 8, 9]"),
            ("slice(whole, 2, 5)", "[2, 3, 4]"),
            ("slice(whole, 0, 10)", "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"),
            ("slice(whole, 10, 10)", "[]"),
            ("slice(whole, 4, 4)", "[]"),
            ("chunk(whole, 4)[1]", "[4, 5, 6, 7]"),
            ("chunk(whole, 4)[2]", "[8, 9]"),
            ("tail(tail(slice(whole, 1, 6)))", "[3, 4, 5]"),
        ] {
            let part = session.list(code);

            assert!(part.shares_buffer_with(&whole), "{code}");
            assert_eq!(session.outcome(code), expected, "{code}");
        }

        assert_eq!(session.outcome("whole"), "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]");
    }

    #[test]
    fn parts_of_a_list_are_what_they_were_before_lists_shared_their_elements() {
        for (code, expected) in [
            ("tail([])", "[]"),
            ("tail([1])", "[]"),
            ("tail([1, 2])", "[2]"),
            ("tail(\"\")", ""),
            ("tail(\"abc\")", "bc"),
            (
                "tail(1)",
                "in built-in function \"tail\": argument must be a list or string",
            ),
            ("slice([1, 2, 3], 1, 3)", "[2, 3]"),
            ("slice([1, 2, 3], 0, 0)", "[]"),
            ("slice([1, 2, 3], 3, 3)", "[]"),
            (
                "slice([1, 2, 3], 2, 1)",
                "in built-in function \"slice\": index out of bounds",
            ),
            (
                "slice([1, 2, 3], 0, 4)",
                "in built-in function \"slice\": index out of bounds",
            ),
            (
                "slice([1, 2, 3], 4, 4)",
                "in built-in function \"slice\": index out of bounds",
            ),
            ("slice([1, 2, 3], -1, 2)", "[1, 2]"),
            ("slice([1, 2, 3], 0, -1)", "[]"),
            ("slice([1, 2, 3], 1.9, 2.9)", "[2]"),
            (
                "slice([1, 2, 3], 0, 1e300)",
                "in built-in function \"slice\": index out of bounds",
            ),
            ("slice(\"abcd\", 1, 3)", "bc"),
            (
                "slice(1, 1, 3)",
                "in built-in function \"slice\": argument must be a list or string",
            ),
            ("chunk([], 2)", "[]"),
            ("chunk([1, 2, 3], 1)", "[[1], [2], [3]]"),
            ("chunk([1, 2, 3], 2)", "[[1, 2], [3]]"),
            ("chunk([1, 2, 3], 5)", "[[1, 2, 3]]"),
            ("chunk([1, 2, 3], 1e300)", "[[1, 2, 3]]"),
            (
                "chunk([1, 2, 3], 0)",
                "in built-in function \"chunk\": chunk size must be greater than 0",
            ),
            (
                "chunk(1, 2)",
                "in built-in function \"chunk\": expected a list, but got a number",
            ),
            ("concat([1], [2])", "[1, 2]"),
            ("concat([1], 2)", "[1, 2]"),
            ("concat(1, [2])", "[1, 2]"),
            ("concat(1, 2)", "[1, 2]"),
            ("concat([], [])", "[]"),
            ("concat([1], [])", "[1]"),
            ("concat([], [1])", "[1]"),
            ("concat([1], [2], [3], 4)", "[1, 2, 3, 4]"),
            ("concat([1], [[2]])", "[1, [2]]"),
            ("concat([1], \"ab\")", "[1, ab]"),
            ("concat(\"ab\", [1])", "[ab, 1]"),
            ("xs = [1, 2]\nconcat(xs, xs)", "[1, 2, 1, 2]"),
            ("xs = [1, 2]\nconcat(xs, xs, xs)", "[1, 2, 1, 2, 1, 2]"),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }
    }

    #[test]
    fn a_part_that_ends_its_list_is_followed_without_changing_the_list() {
        let session = Session::new();

        session
            .evaluate(
                "whole = [...[1, 2, 3], 4]\n\
                 end = slice(whole, 2, 4)\n\
                 longer = [...end, 5]\n\
                 other = [...whole, 6]\n\
                 middle = [...slice(whole, 1, 3), 7]",
            )
            .unwrap();

        assert_eq!(
            session.outcome("[whole, end, longer, other, middle]"),
            "[[1, 2, 3, 4], [3, 4], [3, 4, 5], [1, 2, 3, 4, 6], [2, 3, 7]]"
        );
    }

    #[test]
    fn lists_that_share_their_elements_survive_compaction() {
        let session = Session::new();

        session
            .evaluate(
                "names = map(range(200), i => \"n\" + to_string(i))\n\
                 few = slice(names, 100, 104)\n\
                 more = [...few, \"x\"]\n\
                 rest = tail(few)\n\
                 grown = reduce(range(100), (acc, i) => [...acc, [i, to_string(i)]], [])\n\
                 last = slice(grown, 97, 100)\n\
                 kept = (whole => [slice(whole, 500, 503), slice(whole, 501, 505)])(map(range(1000), i => {n: i}))",
            )
            .unwrap();

        let rendered = |session: &Session| {
            session.outcome(
                "[few, more, rest, last, kept, len(names), names[199], len(grown), grown[50]]",
            )
        };
        let expected = "[[n100, n101, n102, n103], [n100, n101, n102, n103, x], [n101, n102, n103], \
                        [[97, 97], [98, 98], [99, 99]], \
                        [[{n: 500}, {n: 501}, {n: 502}], [{n: 501}, {n: 502}, {n: 503}, {n: 504}]], \
                        200, n199, 100, [50, 50]]";
        assert_eq!(rendered(&session), expected);

        let slots = session.heap.borrow().len();
        session.compact();
        assert!(session.heap.borrow().len() < slots);
        assert_eq!(rendered(&session), expected);

        // The lists can be followed and taken apart like before.
        session
            .evaluate(
                "again = [...more, \"y\"]\n\
                 further = [...grown, [100, \"100\"]]\n\
                 inner = [...kept[1], {n: 0}]",
            )
            .unwrap();
        session.compact();

        assert_eq!(rendered(&session), expected);
        assert_eq!(
            session.outcome("[again, len(further), further[100], grown[99], inner, tail(rest)]"),
            "[[n100, n101, n102, n103, x, y], 101, [100, 100], [99, 99], \
             [{n: 501}, {n: 502}, {n: 503}, {n: 504}, {n: 0}], [n102, n103]]"
        );
    }

    #[test]
    fn compaction_gives_up_the_elements_that_no_list_reads_any_more() {
        let session = Session::new();

        session
            .evaluate("part = (whole => slice(whole, 5000, 5010))(map(range(100000), i => [i]))")
            .unwrap();
        let slots = session.heap.borrow().len();
        assert!(slots > 100_000);

        session.compact();

        // The lists that the dropped elements were are gone with them.
        assert!(
            session.heap.borrow().len() < 100,
            "{} slots",
            session.heap.borrow().len()
        );
        assert_eq!(
            session.outcome("part"),
            "[[5000], [5001], [5002], [5003], [5004], [5005], [5006], [5007], [5008], [5009]]"
        );
    }

    #[test]
    fn a_list_that_is_held_while_the_heap_is_compacted_keeps_its_elements() {
        let session = Session::new();
        session
            .evaluate(
                "names = map(range(50), i => \"n\" + to_string(i))\nfew = slice(names, 10, 13)",
            )
            .unwrap();

        let held = session.list("few");
        let before: Vec<Value> = held.to_vec();

        for _ in 0..50 {
            session.heap.borrow_mut().insert_string("garbage");
        }
        session.compact();

        assert_eq!(held.to_vec(), before);
        assert_eq!(
            session.outcome("[few, names[12], len(names)]"),
            "[[n10, n11, n12], n12, 50]"
        );
        assert_eq!(session.outcome("[...few, \"x\"]"), "[n10, n11, n12, x]");
        assert_eq!(held.to_vec(), before);
    }

    #[test]
    fn a_copy_of_a_heap_has_lists_of_its_own() {
        let session = Session::new();
        session
            .evaluate("xs = [...[1, 2], 3]\nys = [...xs, 4]\npart = tail(ys)")
            .unwrap();

        let copy = Session {
            heap: Rc::new(RefCell::new(session.heap.borrow().clone())),
            bindings: Rc::clone(&session.bindings),
        };

        let original = session.list("ys");
        let copied = copy.list("ys");
        assert_eq!(original, copied);
        assert!(!original.shares_buffer_with(&copied));

        assert_eq!(copy.outcome("[...ys, 5]"), "[1, 2, 3, 4, 5]");
        assert_eq!(session.outcome("[...ys, 6]"), "[1, 2, 3, 4, 6]");
        assert_eq!(
            copy.outcome("[xs, ys, part]"),
            "[[1, 2, 3], [1, 2, 3, 4], [2, 3, 4]]"
        );
        assert_eq!(
            session.outcome("[xs, ys, part]"),
            "[[1, 2, 3], [1, 2, 3, 4], [2, 3, 4]]"
        );
    }
}

#[cfg(test)]
mod unique_tests {
    use crate::environment::Environment;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// What the last statement of `code` comes to, rendered: the value, or the error.
    fn outcome(code: &str) -> String {
        let heap = Rc::new(RefCell::new(Heap::new()));
        let bindings = Rc::new(Environment::new());
        let source: Rc<str> = code.into();

        let mut result = String::new();
        for statement in parse_program(code).expect("a program the fast parser reads") {
            let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement else {
                continue;
            };

            result = match evaluate_ast(
                &expr,
                Rc::clone(&heap),
                Rc::clone(&bindings),
                0,
                Rc::clone(&source),
            ) {
                Ok(value) => value.stringify_internal(&heap.borrow()),
                Err(error) => error.message,
            };
        }

        result
    }

    #[test]
    fn the_first_of_the_values_that_are_equal_is_kept_in_its_place() {
        assert_eq!(outcome("unique([3, 1, 3, 2, 1])"), "[3, 1, 2]");
        assert_eq!(outcome("unique([])"), "[]");
        assert_eq!(
            outcome("unique(map(range(100), i => (i * 7) % 13))"),
            "[0, 7, 1, 8, 2, 9, 3, 10, 4, 11, 5, 12, 6]"
        );
        assert_eq!(
            outcome("unique(map(range(100), i => to_string(i % 11)))"),
            "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
        );
    }

    #[test]
    fn many_distinct_values_are_all_kept() {
        assert_eq!(
            outcome("unique(map(range(20000), i => (i * 7919) % 5003)) into len"),
            "5003"
        );
        assert_eq!(
            outcome(
                "xs = map(range(20000), i => (i * 7919) % 5003)\nunique(xs) == unique([...xs, ...xs]) into all"
            ),
            "true"
        );
        assert_eq!(
            outcome("unique(map(range(6000), i => \"k\" + to_string(i % 1500))) into len"),
            "1500"
        );
        assert_eq!(
            outcome("unique(map(range(6000), i => [i % 30, {k: i % 7}])) into len"),
            "210"
        );
    }

    #[test]
    fn values_of_different_kinds_are_never_equal() {
        assert_eq!(
            outcome(
                "unique([0, \"0\", false, null, [], {}, [0], {a: 0}, \"\", 0, \"0\", false, null, [], {}, [0], {a: 0}, \"\"]) into len"
            ),
            "9"
        );
        assert_eq!(
            outcome(
                "unique(map(range(60), i => if i % 2 == 0 then i % 10 else to_string(i % 10))) into len"
            ),
            "10"
        );
    }

    #[test]
    fn zero_is_equal_to_its_negative_and_the_first_of_them_is_kept() {
        assert_eq!(outcome("unique([0, -0, 0]) via x => 1 / x"), "[inf]");
        assert_eq!(outcome("unique([-0, 0, -0]) via x => 1 / x"), "[-inf]");
        assert_eq!(
            outcome("unique([...range(1, 20), -0, 0, [-0], [0], {z: 0}, {z: -0}]) into len"),
            "22"
        );
    }

    #[test]
    fn a_number_that_is_not_a_number_is_equal_to_nothing() {
        assert_eq!(outcome("unique([0/0, 0/0, 1, 0/0]) into len"), "4");
        assert_eq!(
            outcome("unique(map(range(40), i => if i % 2 == 0 then 0/0 else i % 6)) into len"),
            "23"
        );
        assert_eq!(
            outcome(
                "unique(map(range(40), i => if i % 2 == 0 then [1, {a: 0/0}] else [1, {a: i % 6}])) into len"
            ),
            "23"
        );
    }

    #[test]
    fn records_are_equal_whatever_order_their_keys_were_written_in() {
        assert_eq!(
            outcome("unique([{a: 1, b: 2}, {b: 2, a: 1}, {a: 2, b: 1}]) into len"),
            "2"
        );
        assert_eq!(
            outcome(
                "unique(map(range(90), i => if i % 2 == 0 then {a: i % 9, b: \"x\"} else {b: \"x\", a: i % 9})) into len"
            ),
            "9"
        );
    }

    #[test]
    fn functions_are_equal_when_they_are_written_the_same_way() {
        assert_eq!(
            outcome(
                "unique([x => x + 1, x => x + 1, y => y + 1, x => x + 2, sqrt, sqrt, sin, 1, 2, 3, 4, 5, 6, 7, 8, x => x + 1, sqrt, y => y + 1, (x, y) => x, (x, y) => x]) into len"
            ),
            "14"
        );
    }

    #[test]
    fn values_that_differ_deep_inside_are_told_apart() {
        assert_eq!(
            outcome(
                "deepen = (v, n) => if n == 0 then v else deepen([v], n - 1)\nunique(map(range(300), i => deepen(i % 25, 20))) into len"
            ),
            "25"
        );
        assert_eq!(
            outcome(
                "deepen = (v, n) => if n == 0 then v else deepen({in: v}, n - 1)\nunique(map(range(300), i => deepen(i % 25, 20))) into len"
            ),
            "25"
        );
    }

    #[test]
    fn only_a_list_has_distinct_values() {
        assert_eq!(
            outcome("unique(\"aab\")"),
            "in built-in function \"unique\": expected a list, but got a string"
        );
        assert_eq!(
            outcome("unique(3)"),
            "in built-in function \"unique\": expected a list, but got a number"
        );
    }
}

#[cfg(test)]
mod call_entry_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::{Heap, HeapValue};
    use crate::intern::Symbol;
    use crate::values::{Entry, LambdaDef, Value};
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A function that calls itself by its name once for every level it is given.
    const COUNT: &str = "count = n => if n <= 0 then 0 else count(n - 1) + 1";

    /// What a definition has for the code that a call of it runs.
    #[derive(Debug, PartialEq)]
    enum Entered {
        /// Nothing that any run found out.
        Nowhere,

        /// The code that the function was compiled to.
        Body,

        /// The code in which the function calls itself directly.
        Variant,
    }

    /// A program that is evaluated a statement at a time, each in a run of its own.
    struct Session {
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<Environment>,
    }

    impl Session {
        fn new() -> Self {
            Self {
                heap: Rc::new(RefCell::new(Heap::new())),
                bindings: Rc::new(Environment::new()),
            }
        }

        /// A program that has the values of this one, and no variables yet.
        fn elsewhere(&self) -> Self {
            Self {
                heap: Rc::clone(&self.heap),
                bindings: Rc::new(Environment::new()),
            }
        }

        /// Evaluate the statements of `code`, up to the value of the last one.
        fn evaluate(&self, code: &str) -> Result<Value, RuntimeError> {
            let statements = parse_program(code).expect("a program the fast parser reads");
            let source: Rc<str> = code.into();

            let mut result = Value::Null;
            for statement in statements {
                let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement
                else {
                    continue;
                };
                result = evaluate_ast(
                    &expr,
                    Rc::clone(&self.heap),
                    Rc::clone(&self.bindings),
                    0,
                    Rc::clone(&source),
                )?;
            }

            Ok(result)
        }

        /// What the statements of `code` come to, rendered: the value, or the error.
        fn outcome(&self, code: &str) -> String {
            match self.evaluate(code) {
                Ok(value) => value.stringify_internal(&self.heap.borrow()),
                Err(error) => error.message,
            }
        }

        /// The value of variable `name`.
        fn variable(&self, name: &str) -> Value {
            self.bindings
                .get(Symbol::intern(name))
                .expect("a variable of the program")
        }

        /// Look at the definition of the function that variable `name` is bound to.
        fn definition<T>(&self, name: &str, look: impl FnOnce(&LambdaDef) -> T) -> T {
            let Value::Lambda(pointer) = self.variable(name) else {
                panic!("{name} is not a function");
            };

            match self.heap.borrow().get(pointer.index()) {
                Some(HeapValue::Lambda(def)) => look(def),
                _ => panic!("{name} is not a function"),
            }
        }

        /// Change the definition of the function that variable `name` is bound to where it
        /// is, the way whoever holds the heap can between two runs.
        fn redefine(&self, name: &str, change: impl FnOnce(&mut LambdaDef)) {
            let Value::Lambda(pointer) = self.variable(name) else {
                panic!("{name} is not a function");
            };

            match self.heap.borrow_mut().get_mut(pointer.index()) {
                Some(HeapValue::Lambda(def)) => {
                    change(Rc::get_mut(def).expect("a definition that only the heap holds"))
                }
                _ => panic!("{name} is not a function"),
            }
        }

        /// The number of the run that found out what a call of the function runs that
        /// variable `name` is bound to.
        fn found_in(&self, name: &str) -> u64 {
            self.definition(name, |def| def.scope.entry.run())
        }

        /// What a run found a call of the function to run that variable `name` is bound to.
        fn entered(&self, name: &str) -> Entered {
            self.definition(name, |def| {
                let entry = &def.scope.entry;
                let body = def.scope.code.get().map(Rc::as_ptr);

                if entry.run() == Entry::NO_RUN {
                    Entered::Nowhere
                } else if body == Some(entry.code()) {
                    Entered::Body
                } else {
                    Entered::Variant
                }
            })
        }
    }

    #[test]
    fn a_function_that_can_only_run_its_body_is_entered_there_from_the_start() {
        let session = Session::new();

        session.evaluate("inc = x => x + 1").unwrap();
        assert_eq!(session.entered("inc"), Entered::Body);

        assert_eq!(
            session.outcome("map(range(3), i => inc(inc(i)))"),
            "[2, 3, 4]"
        );
        assert_eq!(session.entered("inc"), Entered::Body);
    }

    #[test]
    fn every_run_that_calls_a_function_finds_out_for_itself_what_the_call_runs() {
        let session = Session::new();

        session.evaluate("inc = x => x + 1").unwrap();
        let made = session.found_in("inc");
        assert_ne!(made, Entry::NO_RUN);

        assert_eq!(session.outcome("inc(1)"), "2");
        let called = session.found_in("inc");
        assert!(called > made);

        assert_eq!(session.outcome("2 + 2"), "4");
        assert_eq!(session.found_in("inc"), called);

        assert_eq!(session.outcome("inc(inc(1))"), "3");
        assert!(session.found_in("inc") > called);
    }

    #[test]
    fn a_function_that_calls_itself_by_name_is_entered_where_it_calls_itself_directly() {
        let session = Session::new();

        session.evaluate(COUNT).unwrap();
        assert_eq!(session.entered("count"), Entered::Nowhere);

        assert_eq!(session.outcome("count(5)"), "5");
        assert_eq!(session.entered("count"), Entered::Variant);

        assert_eq!(
            session.outcome("twice = n => count(n) + count(n)\n[twice(7), twice(0), twice(30)]"),
            "[14, 0, 60]"
        );
        assert_eq!(session.entered("count"), Entered::Variant);
    }

    #[test]
    fn functions_that_call_each_other_by_name_are_entered_at_their_bodies() {
        let session = Session::new();

        session
            .evaluate(
                "even = n => if n == 0 then true else odd(n - 1)\nodd = n => if n == 0 then false else even(n - 1)",
            )
            .unwrap();
        assert_eq!(session.entered("even"), Entered::Nowhere);
        assert_eq!(session.entered("odd"), Entered::Nowhere);

        assert_eq!(
            session.outcome("[even(10), odd(10), even(7), odd(7)]"),
            "[true, false, false, true]"
        );
        assert_eq!(session.entered("even"), Entered::Body);
        assert_eq!(session.entered("odd"), Entered::Body);
    }

    #[test]
    fn calls_that_enter_a_function_directly_are_counted_towards_the_depth_limit() {
        let session = Session::new();

        session
            .evaluate(
                "f = n => if n <= 0 then 0 else g(n - 1) + 1\ng = n => if n <= 0 then 0 else f(n - 1) + 1",
            )
            .unwrap();

        assert_eq!(session.outcome("f(1000)"), "1000");
        assert!(
            session
                .outcome("f(1001)")
                .ends_with("maximum call depth of 1000 exceeded")
        );
        assert_eq!(session.outcome("f(1000)"), "1000");
    }

    #[test]
    fn a_function_that_gets_another_name_is_found_out_about_again() {
        let session = Session::new();

        session.evaluate(COUNT).unwrap();
        assert_eq!(session.outcome("count(5)"), "5");
        assert_eq!(session.entered("count"), Entered::Variant);

        // Under another name, the function no longer knows a call of `count` to be a call of
        // itself.
        session.evaluate("tally = count").unwrap();
        assert_eq!(session.entered("tally"), Entered::Nowhere);

        assert_eq!(session.outcome("tally(5)"), "5");
        assert_eq!(session.entered("tally"), Entered::Body);
        assert_eq!(session.outcome("count(6)"), "6");
        assert_eq!(session.entered("count"), Entered::Body);
    }

    #[test]
    fn a_function_that_gets_another_name_in_the_run_that_calls_it_is_found_out_about_again() {
        let session = Session::new();

        session.evaluate(COUNT).unwrap();
        assert_eq!(
            session.outcome(
                "do { before = count(3); tally = count; return [before, tally(4), count(5)] }"
            ),
            "[3, 4, 5]"
        );
        assert_eq!(session.entered("count"), Entered::Body);

        assert_eq!(
            session.outcome(
                "(fn => do { before = fn(3); count = fn; return [before, count(4), fn(5)] })(count)"
            ),
            "[3, 4, 5]"
        );
        assert_eq!(session.entered("count"), Entered::Variant);
    }

    #[test]
    fn a_function_that_gets_a_name_while_it_runs_goes_on_under_the_name_it_was_called_by() {
        let session = Session::new();

        session
            .evaluate("h = n => if n <= 0 then missing else do { k = h; return k(n - 1) + 1 }")
            .unwrap();
        assert_eq!(
            session.outcome("h(2)"),
            "in function \"h\": in function \"k\": in function \"k\": unknown identifier: missing"
        );
        assert_eq!(
            session.outcome("h(1)"),
            "in function \"k\": in function \"k\": unknown identifier: missing"
        );
    }

    #[test]
    fn an_error_is_put_down_to_the_name_that_the_function_has_by_then() {
        let session = Session::new();

        session
            .evaluate("f = n => if n <= 0 then missing else f(n - 1) + 1")
            .unwrap();
        assert_eq!(
            session.outcome("f(1)"),
            "in function \"f\": in function \"f\": unknown identifier: missing"
        );

        session.evaluate("g = f").unwrap();
        assert_eq!(
            session.outcome("g(1)"),
            "in function \"g\": in function \"g\": unknown identifier: missing"
        );
        assert_eq!(
            session.outcome("f(1)"),
            "in function \"g\": in function \"g\": unknown identifier: missing"
        );
    }

    #[test]
    fn a_function_that_its_holder_renames_between_runs_is_found_out_about_again() {
        let session = Session::new();

        session.evaluate(COUNT).unwrap();
        assert_eq!(session.outcome("count(3)"), "3");
        assert_eq!(session.entered("count"), Entered::Variant);

        // Where the function is `tally` and `count` is another function, it calls that one.
        let other = session.elsewhere();
        other.evaluate("count = n => 100").unwrap();
        other
            .bindings
            .insert(Symbol::intern("tally"), session.variable("count"));
        session.redefine("count", |def| def.name = Some(Symbol::intern("tally")));

        assert_eq!(other.outcome("tally(3)"), "101");
        assert_eq!(other.entered("tally"), Entered::Body);
        assert_eq!(
            other.outcome("[tally(0), tally(1), tally(9)]"),
            "[0, 101, 101]"
        );
    }

    #[test]
    fn a_function_that_its_holder_names_through_the_definition_is_entered_nowhere_yet() {
        let session = Session::new();

        session.evaluate(COUNT).unwrap();
        assert_eq!(session.outcome("count(3)"), "3");
        assert_eq!(session.entered("count"), Entered::Variant);

        let value = session.variable("count");
        session.redefine("count", |def| def.set_name(Symbol::intern("tally"), value));
        assert_eq!(session.entered("count"), Entered::Nowhere);

        assert_eq!(session.outcome("count(3)"), "3");
        assert_eq!(session.entered("count"), Entered::Body);
    }

    #[test]
    fn a_function_that_its_holder_gives_another_body_between_runs_runs_that_body() {
        let session = Session::new();

        session.evaluate("f = x => x + 1\ng = x => x * 10").unwrap();
        assert_eq!(session.outcome("f(1)"), "2");
        assert_eq!(session.entered("f"), Entered::Body);

        let (args, body) =
            session.definition("g", |def| (Rc::clone(&def.args), Rc::clone(&def.body)));
        session.redefine("f", |def| {
            def.args = args;
            def.body = body;
        });

        assert_eq!(session.outcome("f(1)"), "10");
        assert_eq!(session.outcome("[f(2), f(f(2))]"), "[20, 200]");
        assert_eq!(session.outcome("map([1, 2], f)"), "[10, 20]");
    }

    #[test]
    fn a_copy_of_a_definition_is_entered_nowhere_yet() {
        let session = Session::new();

        session.evaluate("inc = x => x + 1").unwrap();
        assert_eq!(session.entered("inc"), Entered::Body);

        let copy = session.definition("inc", LambdaDef::clone);
        assert_eq!(copy.scope.entry.run(), Entry::NO_RUN);
        assert!(copy.scope.entry.code().is_null());
    }
}

#[cfg(test)]
mod shared_string_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::{Heap, HeapValue};
    use crate::text::Text;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A program that is evaluated a few statements at a time.
    struct Session {
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<Environment>,
    }

    impl Session {
        fn new() -> Self {
            Self {
                heap: Rc::new(RefCell::new(Heap::new())),
                bindings: Rc::new(Environment::new()),
            }
        }

        /// Evaluate the statements of `code`, up to the value of the last one.
        fn evaluate(&self, code: &str) -> Result<Value, RuntimeError> {
            let statements = parse_program(code).expect("a program the fast parser reads");
            let source: Rc<str> = code.into();

            let mut result = Value::Null;
            for statement in statements {
                let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement
                else {
                    continue;
                };
                result = evaluate_ast(
                    &expr,
                    Rc::clone(&self.heap),
                    Rc::clone(&self.bindings),
                    0,
                    Rc::clone(&source),
                )?;
            }

            Ok(result)
        }

        /// What the statements of `code` come to, rendered: the value, or the error.
        fn outcome(&self, code: &str) -> String {
            match self.evaluate(code) {
                Ok(value) => value.stringify_internal(&self.heap.borrow()),
                Err(error) => error.message,
            }
        }

        /// The text of the string that `code` comes to.
        fn text(&self, code: &str) -> Text {
            let value = self.evaluate(code).unwrap();

            value.as_text(&self.heap.borrow()).unwrap().clone()
        }

        /// Drop everything that the variables of the program do not lead to.
        fn compact(&self) {
            self.heap.borrow_mut().compact(&self.bindings, &mut []);
        }

        /// The number of buffers that the strings of at least `len` bytes in the heap read
        /// their characters from.
        fn buffers(&self, len: usize) -> usize {
            let heap = self.heap.borrow();
            let mut distinct: Vec<&Text> = Vec::new();

            for slot in 0..heap.len() {
                if let Some(HeapValue::String(text)) = heap.get(slot)
                    && text.len() >= len
                    && !distinct.iter().any(|known| known.shares_buffer_with(text))
                {
                    distinct.push(text);
                }
            }

            distinct.len()
        }

        /// The number of strings in the heap that read their characters where `text` does.
        fn sharing(&self, text: &Text) -> usize {
            let heap = self.heap.borrow();

            (0..heap.len())
                .filter(|slot| match heap.get(*slot) {
                    Some(HeapValue::String(other)) => other.shares_buffer_with(text),
                    _ => false,
                })
                .count()
        }
    }

    fn outcome(code: &str) -> String {
        Session::new().outcome(code)
    }

    /// The letters of the alphabet `times` times over, as a program spells them.
    fn letters(times: usize) -> String {
        format!("\"{}\"", "abcdefghijklmnopqrstuvwxyz".repeat(times))
    }

    #[test]
    fn strings_that_are_added_hold_the_characters_of_both() {
        for (code, expected) in [
            ("\"ab\" + \"cd\"", "abcd"),
            ("\"\" + \"cd\"", "cd"),
            ("\"ab\" + \"\"", "ab"),
            ("\"\" + \"\"", ""),
            ("\"é\" + \"日本\" + \"😀\"", "é日本😀"),
            ("s = \"ab\"\ns + s", "abab"),
            ("s = \"ab\"\ns + s + s", "ababab"),
            ("[\"a\", \"b\"] + \"c\"", "[ac, bc]"),
            ("\"c\" + [\"a\", \"b\"]", "[ca, cb]"),
            ("[\"a\", \"b\"] + [\"c\", \"\"]", "[ac, b]"),
            ("[\"\", \"\"] + \"\"", "[, ]"),
            ("len(\"abcdefghijklmnopqrstuv\" + \"w\")", "23"),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }

        for (code, expected) in [
            ("\"a\" + 1", "expected a string, but got a number"),
            ("1 + \"a\"", "expected a number, but got a string"),
            ("\"a\" + null", "expected a string, but got a null"),
            ("\"\" + 1", "expected a string, but got a number"),
            ("\"a\" + [1]", "can't add string and number"),
            ("[1] + \"a\"", "can't add number and string"),
            ("[\"a\", 1] + [\"b\", \"c\"]", "can't add number and string"),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }
    }

    #[test]
    fn a_string_that_nothing_is_added_to_is_the_string_that_results() {
        let session = Session::new();
        session
            .evaluate(&format!("long = {}\nshort = \"ab\"", letters(3)))
            .unwrap();

        let slots = session.heap.borrow().len();
        assert_eq!(session.outcome("long + \"\" == long"), "true");
        assert_eq!(session.outcome("len(\"\" + long + \"\")"), "78");
        assert_eq!(session.outcome("short + \"\""), "ab");

        // Only the empty strings that the program spells out were made.
        assert_eq!(session.heap.borrow().len(), slots + 4);
    }

    #[test]
    fn strings_that_follow_the_same_string_do_not_show_up_in_each_other() {
        let session = Session::new();

        // A string that keeps being added to has room for more behind it, so the first
        // string that follows it is written there. Every other one is a copy.
        session
            .evaluate(&format!(
                "base = reduce(range(8), (acc, i) => acc + to_string(i), {})\n\
                 a = base + \"A\"\n\
                 b = base + \"B\"\n\
                 c = a + \"C\"\n\
                 d = a + \"D\"\n\
                 e = c + \"E\"\n\
                 f = a + \"F\" + \"G\"\n\
                 g = base + base\n\
                 h = e + e",
                letters(1)
            ))
            .unwrap();

        let base = "abcdefghijklmnopqrstuvwxyz01234567";
        assert_eq!(
            session.outcome("[base, a, b, c, d, e, f]"),
            format!("[{base}, {base}A, {base}B, {base}AC, {base}AD, {base}ACE, {base}AFG]")
        );
        assert_eq!(session.outcome("g"), format!("{base}{base}"));
        assert_eq!(session.outcome("h"), format!("{base}ACE{base}ACE"));
        assert!(session.text("a").shares_buffer_with(&session.text("base")));
        assert!(!session.text("b").shares_buffer_with(&session.text("base")));
    }

    #[test]
    fn every_step_of_a_fold_keeps_the_string_it_made() {
        assert_eq!(
            outcome(
                "reduce(range(5), (state, i) => {now: state.now + to_string(i), before: [...state.before, state.now]}, {now: \"\", before: []}).before"
            ),
            "[, 0, 01, 012, 0123]"
        );

        let long = letters(2);
        let text = "abcdefghijklmnopqrstuvwxyz".repeat(2);
        assert_eq!(
            outcome(&format!(
                "reduce(range(4), (state, i) => {{now: state.now + to_string(i), before: [...state.before, state.now]}}, {{now: {long}, before: []}}).before"
            )),
            format!("[{text}, {text}0, {text}01, {text}012]")
        );
        assert_eq!(
            outcome(&format!(
                "reduce(range(4), (state, i) => {{now: tail(state.now) + to_string(i), before: [...state.before, state.now]}}, {{now: {long}, before: []}}).before"
            )),
            format!(
                "[{}, {}0, {}01, {}012]",
                text,
                &text[1..],
                &text[2..],
                &text[3..]
            )
        );
        assert_eq!(
            outcome(&format!("map(range(3), i => {long} + to_string(i))")),
            format!("[{text}0, {text}1, {text}2]")
        );
    }

    #[test]
    fn a_string_that_grows_in_a_fold_is_not_copied_at_every_step() {
        for (code, len, start, end) in [
            (
                "reduce(range(3000), (acc, i) => acc + \"ab\", \"\")",
                6000,
                "abab",
                "abab",
            ),
            (
                "reduce(range(3000), (acc, i) => acc + to_string(i % 10), \"\")",
                3000,
                "0123",
                "6789",
            ),
            (
                "reduce(range(3000), (acc, i) => acc + \"a\" + \"b\", \"\")",
                6000,
                "abab",
                "abab",
            ),
            (
                "reduce(range(3000), (acc, i) => if i % 2 == 0 then acc + \"xy\" else acc, \"\")",
                3000,
                "xyxy",
                "xyxy",
            ),
            (
                "grow = (n, acc) => if n == 0 then acc else grow(n - 1, acc + \"ab\")\ngrow(900, \"\")",
                1800,
                "abab",
                "abab",
            ),
            (
                "reduce(range(3000), (acc, i) => acc + \"é\", \"\")",
                6000,
                "éé",
                "éé",
            ),
        ] {
            let session = Session::new();
            let text = session.text(code);

            assert_eq!(text.len(), len, "{code}");
            assert!(text.starts_with(start), "{code}");
            assert!(text.ends_with(end), "{code}");

            // A buffer for every time that the string outgrows the one before, which is
            // twice as large every time, rather than one for each of the steps.
            let buffers = session.buffers(30);
            assert!(buffers <= 12, "{code}: {buffers} buffers");

            let sharing = session.sharing(&text);
            assert!(
                sharing > len / 8,
                "{code}: {sharing} strings share the last buffer"
            );
        }
    }

    #[test]
    fn a_string_that_is_added_to_once_takes_the_room_of_its_characters() {
        let session = Session::new();
        let text = session.text(&format!("{} + \"!\"", letters(2)));

        assert_eq!(text.len(), 53);
        assert_eq!(session.sharing(&text), 1);
        // Its share of the buffer is all of it: the characters and what a buffer starts with.
        assert!(
            session.heap.borrow().approx_bytes() < 400,
            "{} bytes",
            session.heap.borrow().approx_bytes()
        );
    }

    #[test]
    fn parts_of_a_string_read_the_characters_of_the_string() {
        let session = Session::new();
        session
            .evaluate(&format!(
                "whole = {}\npadded = \"  \" + whole + \" \t\"\nlisted = whole + \",\" + whole",
                letters(4)
            ))
            .unwrap();
        let whole = session.text("whole");
        let alphabet = "abcdefghijklmnopqrstuvwxyz";

        for (code, expected) in [
            (
                "tail(whole)",
                format!("{}{}", &alphabet[1..], alphabet.repeat(3)),
            ),
            (
                "slice(whole, 2, 52)",
                format!("{}{}", &alphabet[2..], alphabet),
            ),
            ("slice(whole, 0, 104)", alphabet.repeat(4)),
            ("slice(whole, 78, 104)", alphabet.to_string()),
            (
                "tail(tail(slice(whole, 1, 60)))",
                alphabet.repeat(3)[3..60].to_string(),
            ),
        ] {
            let part = session.text(code);

            assert!(part.shares_buffer_with(&whole), "{code}");
            assert_eq!(part.as_str(), expected, "{code}");
        }

        assert!(
            session
                .text("trim(padded)")
                .shares_buffer_with(&session.text("padded"))
        );
        assert_eq!(session.outcome("trim(padded) == whole"), "true");
        assert!(
            session
                .text("split(listed, \",\")[1]")
                .shares_buffer_with(&session.text("listed"))
        );
        assert_eq!(
            session.outcome("split(listed, \",\") == [whole, whole]"),
            "[true, true]"
        );

        // A part that is short has its characters to itself.
        assert!(
            !session
                .text("slice(whole, 2, 24)")
                .shares_buffer_with(&whole)
        );
        assert_eq!(session.outcome("slice(whole, 2, 24)"), alphabet[2..24]);
        assert_eq!(session.outcome("whole"), alphabet.repeat(4));
    }

    #[test]
    fn parts_of_a_string_are_what_they_were_before_strings_shared_their_characters() {
        for (code, expected) in [
            ("head(\"\")", ""),
            ("head(\"abc\")", "a"),
            ("head(\"é\")", ""),
            ("head(\"aé\")", "a"),
            ("tail(\"\")", ""),
            ("tail(\"a\")", ""),
            ("tail(\"abc\")", "bc"),
            ("tail(\"é\")", ""),
            ("tail(\"aé\")", "é"),
            ("slice(\"abcd\", 1, 3)", "bc"),
            ("slice(\"abcd\", 0, 0)", ""),
            ("slice(\"abcd\", 4, 4)", ""),
            ("slice(\"abcd\", 0, 4)", "abcd"),
            (
                "slice(\"abcd\", 2, 1)",
                "in built-in function \"slice\": index out of bounds",
            ),
            (
                "slice(\"abcd\", 0, 5)",
                "in built-in function \"slice\": index out of bounds",
            ),
            (
                "slice(\"abcd\", 5, 5)",
                "in built-in function \"slice\": index out of bounds",
            ),
            ("slice(\"aé\", 1, 3)", "é"),
            (
                "slice(\"aé\", 1, 2)",
                "in built-in function \"slice\": index out of bounds",
            ),
            (
                "slice(\"aé\", 2, 3)",
                "in built-in function \"slice\": index out of bounds",
            ),
            ("slice(\"abcd\", -1, 2)", "ab"),
            ("slice(\"abcd\", 1.9, 2.9)", "b"),
            ("split(\"a,b,,c\", \",\")", "[a, b, , c]"),
            ("split(\"abc\", \"\")", "[, a, b, c, ]"),
            ("split(\"\", \",\")", "[]"),
            ("split(\"\", \"\")", "[, ]"),
            ("split(\"abc\", \"abc\")", "[, ]"),
            ("split(\"aébéc\", \"é\")", "[a, b, c]"),
            ("split(\"abc\", \"x\")", "[abc]"),
            ("trim(\"  ab cd \")", "ab cd"),
            ("trim(\"\")", ""),
            ("trim(\"   \")", ""),
            ("trim(\"ab\")", "ab"),
            ("trim(\"\u{a0}é\u{3000}\")", "é"),
            ("len(\"é\")", "2"),
            ("len(tail(\"aé\"))", "2"),
            ("len(split(\"a,b\", \",\"))", "2"),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }
    }

    #[test]
    fn the_characters_of_a_string_are_found_by_their_position() {
        let session = Session::new();
        session
            .evaluate(&format!(
                "short = \"abc\"\nwide = \"aé😀b\"\nlong = {}\nmixed = long + \"é\" + long",
                letters(2)
            ))
            .unwrap();

        for (code, expected) in [
            ("short[0]", "a"),
            ("short[2]", "c"),
            ("short[3]", "null"),
            ("short[-1]", "c"),
            ("short[-3]", "a"),
            ("short[-4]", "null"),
            ("short[1.9]", "b"),
            ("short[-0.5]", "a"),
            ("short[0/0]", "a"),
            ("short[1e300]", "null"),
            ("short[-1e300]", "null"),
            ("wide[0]", "a"),
            ("wide[1]", "é"),
            ("wide[2]", "😀"),
            ("wide[3]", "b"),
            ("wide[4]", "null"),
            ("wide[-1]", "b"),
            ("wide[-3]", "é"),
            ("wide[-4]", "a"),
            ("wide[-5]", "null"),
            ("\"\"[0]", "null"),
            ("\"\"[-1]", "null"),
            ("long[0]", "a"),
            ("long[27]", "b"),
            ("long[51]", "z"),
            ("long[52]", "null"),
            ("long[-1]", "z"),
            ("long[-52]", "a"),
            ("long[-53]", "null"),
            ("mixed[51]", "z"),
            ("mixed[52]", "é"),
            ("mixed[53]", "a"),
            ("mixed[104]", "z"),
            ("mixed[105]", "null"),
            ("mixed[-1]", "z"),
            ("mixed[-53]", "é"),
            ("mixed[-105]", "a"),
            ("mixed[-106]", "null"),
            ("slice(mixed, 26, 80)[26]", "é"),
            ("slice(mixed, 26, 80)[27]", "a"),
            ("slice(mixed, 54, 106)[51]", "z"),
            ("tail(long)[0]", "b"),
            ("(long + \"!\")[52]", "!"),
            ("typeof(long[3])", "string"),
            ("map(range(3), i => wide[i + 1])", "[é, 😀, b]"),
            ("[...wide]", "[a, é, 😀, b]"),
            ("{...wide}", "{0: a, 1: é, 2: 😀, 3: b}"),
        ] {
            assert_eq!(session.outcome(code), expected, "{code}");
        }

        assert_eq!(
            session.outcome("short[\"a\"]"),
            "expected a number, but got a string"
        );
    }

    #[test]
    fn a_part_that_ends_its_string_is_followed_without_changing_the_string() {
        let session = Session::new();
        let alphabet = "abcdefghijklmnopqrstuvwxyz";

        session
            .evaluate(&format!(
                "whole = reduce(range(4), (acc, i) => acc + \"-\", {})\n\
                 end = slice(whole, 20, 56)\n\
                 longer = end + \"1\"\n\
                 other = whole + \"2\"\n\
                 middle = slice(whole, 10, 40) + \"3\"",
                letters(2)
            ))
            .unwrap();

        let whole = format!("{}----", alphabet.repeat(2));
        assert_eq!(
            session.outcome("[whole, end, longer, other, middle]"),
            format!(
                "[{whole}, {}, {}1, {whole}2, {}3]",
                &whole[20..],
                &whole[20..],
                &whole[10..40]
            )
        );
    }

    #[test]
    fn strings_that_share_their_characters_survive_compaction() {
        let session = Session::new();
        let alphabet = "abcdefghijklmnopqrstuvwxyz";

        session
            .evaluate(&format!(
                "grown = reduce(range(200), (acc, i) => acc + to_string(i % 10), {})\n\
                 part = slice(grown, 100, 160)\n\
                 more = part + \"x\"\n\
                 rest = tail(part)\n\
                 words = split(reduce(range(50), (acc, i) => acc + \"word number \" + to_string(i) + \" of fifty,\", \"\"), \",\")\n\
                 kept = (whole => [slice(whole, 5000, 5040), slice(whole, 5010, 5060) + \"!\"])(reduce(range(10000), (acc, i) => acc + to_string(i % 10), \"\"))",
                letters(1)
            ))
            .unwrap();

        let digits = "0123456789";
        let grown = format!("{alphabet}{}", digits.repeat(20));
        let rendered = |session: &Session| {
            session
                .outcome("[part, more, rest, kept, len(grown), grown[225], len(words), words[49]]")
        };
        let expected = format!(
            "[{}, {}x, {}, [{}, {}!], 226, 9, 51, word number 49 of fifty]",
            &grown[100..160],
            &grown[100..160],
            &grown[101..160],
            digits.repeat(4),
            digits.repeat(5),
        );
        assert_eq!(rendered(&session), expected);

        let slots = session.heap.borrow().len();
        session.compact();
        assert!(session.heap.borrow().len() < slots);
        assert_eq!(rendered(&session), expected);

        // The strings can be followed and taken apart like before.
        session
            .evaluate(
                "again = more + \"y\"\n\
                 further = grown + \"!\"\n\
                 inner = kept[1] + \"?\"",
            )
            .unwrap();
        session.compact();

        assert_eq!(rendered(&session), expected);
        assert_eq!(
            session.outcome("[again, len(further), further[226], grown[226], inner, tail(rest)]"),
            format!(
                "[{}xy, 227, !, null, {}!?, {}]",
                &grown[100..160],
                digits.repeat(5),
                &grown[102..160]
            )
        );
    }

    #[test]
    fn compaction_gives_up_the_characters_that_no_string_reads_any_more() {
        let session = Session::new();

        session
            .evaluate(
                "part = (whole => slice(whole, 5000, 5010))(reduce(range(100000), (acc, i) => acc + to_string(i % 10), \"\"))",
            )
            .unwrap();
        assert!(session.heap.borrow().len() > 100_000);

        session.compact();

        assert!(
            session.heap.borrow().len() < 100,
            "{} slots",
            session.heap.borrow().len()
        );
        assert!(
            session.heap.borrow().approx_bytes() < 10_000,
            "{} bytes",
            session.heap.borrow().approx_bytes()
        );
        assert_eq!(session.outcome("part"), "0123456789");
    }

    #[test]
    fn a_copy_of_a_heap_keeps_its_strings_when_the_other_adds_to_them() {
        let session = Session::new();
        session
            .evaluate(&format!(
                "grown = reduce(range(8), (acc, i) => acc + to_string(i), {})\npart = tail(grown)",
                letters(1)
            ))
            .unwrap();

        let copy = Session {
            heap: Rc::new(RefCell::new(session.heap.borrow().clone())),
            bindings: Rc::clone(&session.bindings),
        };

        let grown = "abcdefghijklmnopqrstuvwxyz01234567";
        assert_eq!(copy.outcome("grown + \"A\""), format!("{grown}A"));
        assert_eq!(session.outcome("grown + \"B\""), format!("{grown}B"));
        assert_eq!(copy.outcome("part + \"C\""), format!("{}C", &grown[1..]));
        assert_eq!(session.outcome("part + \"D\""), format!("{}D", &grown[1..]));
        assert_eq!(
            copy.outcome("[grown, part]"),
            format!("[{grown}, {}]", &grown[1..])
        );
        assert_eq!(
            session.outcome("[grown, part]"),
            format!("[{grown}, {}]", &grown[1..])
        );
    }

    #[test]
    fn strings_are_keys_and_values_whatever_they_were_made_from() {
        let session = Session::new();
        session
            .evaluate(&format!(
                "long = {}\npart = slice(long + \"!\", 0, 52)\nbuilt = reduce(range(52), (acc, i) => acc + long[i], \"\")\nshort = slice(long, 3, 6)",
                letters(2)
            ))
            .unwrap();

        for (code, expected) in [
            (
                "[long == part, part == built, built == long]",
                "[true, true, true]",
            ),
            (
                "[long != part, long < part, long <= built, part >= built]",
                "[false, false, true, true]",
            ),
            ("{[long]: 1}[part]", "1"),
            (
                "{[part]: 1, [built]: 2, [long]: 3}",
                &format!("{{{}: 3}}", "abcdefghijklmnopqrstuvwxyz".repeat(2)),
            ),
            ("len(unique([long, part, built, short, \"def\"]))", "2"),
            ("includes([long], built)", "true"),
            ("includes(long, short)", "true"),
            ("short == \"def\"", "true"),
            ("{def: 1}[short]", "1"),
            (
                "sort([part, \"b\", short, \"a\"])[1]",
                "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz",
            ),
            ("uppercase(short) + lowercase(\"ABC\")", "DEFabc"),
            ("replace(part, long, \"x\")", "x"),
            ("join([short, part], \"-\") == \"def-\" + built", "true"),
            ("to_string(part) == part", "true"),
            ("typeof(part)", "string"),
            ("format(\"{}/{}\", short, 1)", "def/1"),
        ] {
            assert_eq!(session.outcome(code), expected, "{code}");
        }
    }
}

#[cfg(test)]
mod shared_record_tests {
    use crate::environment::Environment;
    use crate::error::RuntimeError;
    use crate::expressions::evaluate_ast;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::{Heap, HeapValue};
    use crate::record::Record;
    use crate::values::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A program that is evaluated a few statements at a time.
    struct Session {
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<Environment>,
    }

    impl Session {
        fn new() -> Self {
            Self {
                heap: Rc::new(RefCell::new(Heap::new())),
                bindings: Rc::new(Environment::new()),
            }
        }

        /// Evaluate the statements of `code`, up to the value of the last one.
        fn evaluate(&self, code: &str) -> Result<Value, RuntimeError> {
            let statements = parse_program(code).expect("a program the fast parser reads");
            let source: Rc<str> = code.into();

            let mut result = Value::Null;
            for statement in statements {
                let (Statement::Expression(expr) | Statement::Output { expr, .. }) = statement
                else {
                    continue;
                };
                result = evaluate_ast(
                    &expr,
                    Rc::clone(&self.heap),
                    Rc::clone(&self.bindings),
                    0,
                    Rc::clone(&source),
                )?;
            }

            Ok(result)
        }

        /// What the statements of `code` come to, rendered: the value, or the error.
        fn outcome(&self, code: &str) -> String {
            match self.evaluate(code) {
                Ok(value) => value.stringify_internal(&self.heap.borrow()),
                Err(error) => error.message,
            }
        }

        /// What `look` makes of the record that `code` comes to and of the heap it is in.
        fn inspect<T>(&self, code: &str, look: impl FnOnce(&Record, &Heap) -> T) -> T {
            let value = self.evaluate(code).unwrap();
            let heap = self.heap.borrow();

            look(value.as_record(&heap).unwrap(), &heap)
        }

        /// Whether the records that `one` and `other` come to read their values from the
        /// same block.
        fn share(&self, one: &str, other: &str) -> bool {
            let one = self.evaluate(one).unwrap();
            let other = self.evaluate(other).unwrap();
            let heap = self.heap.borrow();

            one.as_record(&heap)
                .unwrap()
                .shares_values_with(other.as_record(&heap).unwrap())
        }

        /// Drop everything that the variables of the program do not lead to.
        fn compact(&self) {
            self.heap.borrow_mut().compact(&self.bindings, &mut []);
        }

        /// The number of blocks that the records of at least `len` entries in the heap read
        /// their values from.
        fn blocks(&self, len: usize) -> usize {
            let heap = self.heap.borrow();
            let mut distinct: Vec<&Record> = Vec::new();

            for slot in 0..heap.len() {
                if let Some(HeapValue::Record(record)) = heap.get(slot)
                    && record.len() >= len
                    && !distinct
                        .iter()
                        .any(|known| known.shares_values_with(record))
                {
                    distinct.push(record);
                }
            }

            distinct.len()
        }

        /// The number of records in the heap that read their values where the record that
        /// `code` comes to does.
        fn sharing(&self, code: &str) -> usize {
            self.inspect(code, |record, heap| {
                (0..heap.len())
                    .filter(|slot| match heap.get(*slot) {
                        Some(HeapValue::Record(other)) => other.shares_values_with(record),
                        _ => false,
                    })
                    .count()
            })
        }
    }

    fn outcome(code: &str) -> String {
        Session::new().outcome(code)
    }

    #[test]
    fn a_record_that_starts_with_a_spread_holds_what_it_is_made_of() {
        for (code, expected) in [
            ("{...{a: 1, b: 2}, c: 3}", "{a: 1, b: 2, c: 3}"),
            ("{...{a: 1, b: 2}}", "{a: 1, b: 2}"),
            ("{...{}}", "{}"),
            ("{...{}, a: 1}", "{a: 1}"),
            ("{...{a: 1, b: 2}, a: 10}", "{a: 10, b: 2}"),
            ("{...{a: 1, b: 2}, b: 10, a: 20}", "{a: 20, b: 10}"),
            ("{...{a: 1, b: 2}, c: 3, c: 4, a: 5}", "{a: 5, b: 2, c: 4}"),
            (
                "{...{a: 1, b: 2}, ...{c: 3, a: 0}, d: 4}",
                "{a: 0, b: 2, c: 3, d: 4}",
            ),
            ("{...{a: 1}, ...{b: 2}, ...{c: 3}}", "{a: 1, b: 2, c: 3}"),
            ("{a: 0, ...{a: 1, b: 2}}", "{a: 1, b: 2}"),
            ("{a: 0, ...{a: 1, b: 2}, c: 1}", "{a: 1, b: 2, c: 1}"),
            ("{z: 0, ...{a: 1, b: 2}}", "{z: 0, a: 1, b: 2}"),
            ("{...[7, 8], a: 1}", "{0: 7, 1: 8, a: 1}"),
            ("{...\"hi\", a: 1}", "{0: h, 1: i, a: 1}"),
            ("{...{a: 1}, [\"b\"]: 2}", "{a: 1, b: 2}"),
            ("{...{a: 1}, [\"a\"]: 2}", "{a: 2}"),
            ("r = {a: 1, b: 2}\n{...r, ...r}", "{a: 1, b: 2}"),
            (
                "r = {a: 1, b: 2}\n{...r, r: r}",
                "{a: 1, b: 2, r: {a: 1, b: 2}}",
            ),
            (
                "r = {a: 1, b: 2}\n{...r, x: {...r, y: 1}, z: 2}",
                "{a: 1, b: 2, x: {a: 1, b: 2, y: 1}, z: 2}",
            ),
            (
                "r = {a: 1, b: 2}\n{...r, x: 1, y: {...r, z: 2}}",
                "{a: 1, b: 2, x: 1, y: {a: 1, b: 2, z: 2}}",
            ),
            (
                "r = {...{a: 1}, b: 2}\ns = {...r, c: 3}\n{...s, x: 1, y: {...s, z: 2}, w: {...s, x: 3}}",
                "{a: 1, b: 2, c: 3, x: 1, y: {a: 1, b: 2, c: 3, z: 2}, w: {a: 1, b: 2, c: 3, x: 3}}",
            ),
        ] {
            assert_eq!(outcome(code), expected, "{code}");
        }

        for code in [
            "{...1, a: 2}",
            "{...null}",
            "{a: 1, ...true}",
            "{...(x => x)}",
        ] {
            assert_eq!(
                outcome(code),
                "expected a list, record, or string",
                "{code}"
            );
        }
        assert_eq!(
            outcome("{...{a: 1}, [1]: 2}"),
            "expected a string, but got a number"
        );
    }

    #[test]
    fn records_that_add_to_the_same_record_do_not_show_up_in_each_other() {
        let session = Session::new();

        session
            .evaluate(
                "base = {a: 1, b: 2}\n\
                 c = {...base, c: 3}\n\
                 d = {...base, c: 4}\n\
                 e = {...c, d: 5}\n\
                 f = {...c, d: 6}\n\
                 g = {...e, e: 7}\n\
                 h = {...e, e: 8, f: 9}\n\
                 i = {...e, d: 10}\n\
                 j = {...g, f: 11}\n\
                 k = {...g, g: 12}",
            )
            .unwrap();

        assert_eq!(
            session.outcome("[base, c, d, e, f, g, h, i, j, k]"),
            "[{a: 1, b: 2}, {a: 1, b: 2, c: 3}, {a: 1, b: 2, c: 4}, {a: 1, b: 2, c: 3, d: 5}, \
             {a: 1, b: 2, c: 3, d: 6}, {a: 1, b: 2, c: 3, d: 5, e: 7}, \
             {a: 1, b: 2, c: 3, d: 5, e: 8, f: 9}, {a: 1, b: 2, c: 3, d: 10}, \
             {a: 1, b: 2, c: 3, d: 5, e: 7, f: 11}, {a: 1, b: 2, c: 3, d: 5, e: 7, g: 12}]"
        );

        // A record that is added to once or twice just fits its values, and so do the
        // records that are made from it apart from the first one, which is taken to keep
        // growing: the record that is made from that one first has its entry right behind
        // the values of that one.
        assert_eq!(session.sharing("c"), 0);
        assert_eq!(session.sharing("e"), 1);
        assert_eq!(session.sharing("g"), 2);
        assert!(session.share("j", "g"));
        assert!(!session.share("k", "g"));
        assert!(!session.share("g", "e"));
        assert!(!session.share("h", "e"));
        assert!(!session.share("f", "e"));
        assert!(!session.share("i", "e"));
    }

    #[test]
    fn every_step_of_a_fold_keeps_the_record_it_made() {
        assert_eq!(
            outcome(
                "reduce(range(5), (state, i) => {now: {...state.now, [\"k\" + to_string(i)]: i}, before: [...state.before, state.now]}, {now: {}, before: []}).before"
            ),
            "[{}, {k0: 0}, {k0: 0, k1: 1}, {k0: 0, k1: 1, k2: 2}, {k0: 0, k1: 1, k2: 2, k3: 3}]"
        );

        assert_eq!(
            outcome(
                "reduce(range(4), (state, i) => {now: {...state.now, n: i, [\"k\" + to_string(i)]: i}, before: [...state.before, state.now]}, {now: {n: -1}, before: []}).before"
            ),
            "[{n: -1}, {n: 0, k0: 0}, {n: 1, k0: 0, k1: 1}, {n: 2, k0: 0, k1: 1, k2: 2}]"
        );

        assert_eq!(
            outcome("map(range(3), i => {...{a: 1, b: 2}, [\"k\" + to_string(i)]: i})"),
            "[{a: 1, b: 2, k0: 0}, {a: 1, b: 2, k1: 1}, {a: 1, b: 2, k2: 2}]"
        );

        assert_eq!(
            outcome(
                "grown = reduce(range(6), (acc, i) => {...acc, [\"k\" + to_string(i)]: i}, {})\nmap(range(3), i => {...grown, x: i})"
            ),
            "[{k0: 0, k1: 1, k2: 2, k3: 3, k4: 4, k5: 5, x: 0}, \
             {k0: 0, k1: 1, k2: 2, k3: 3, k4: 4, k5: 5, x: 1}, \
             {k0: 0, k1: 1, k2: 2, k3: 3, k4: 4, k5: 5, x: 2}]"
        );
    }

    #[test]
    fn a_record_that_grows_in_a_fold_is_not_copied_at_every_step() {
        for (code, len, first, last) in [
            (
                "grown = reduce(range(3000), (acc, i) => {...acc, [\"k\" + to_string(i)]: i}, {})",
                3000,
                0.0,
                2999.0,
            ),
            (
                "grown = reduce(range(3000), (acc, i) => {...acc, [\"k\" + to_string(i)]: i, [\"n\" + to_string(i)]: -i}, {})",
                6000,
                0.0,
                -2999.0,
            ),
            (
                "grown = reduce(range(3000), (acc, i) => {...acc, ...{[\"k\" + to_string(i)]: i}}, {})",
                3000,
                0.0,
                2999.0,
            ),
            (
                "grown = reduce(range(3000), (acc, i) => if i % 2 == 0 then {...acc, [\"k\" + to_string(i)]: i} else acc, {})",
                1500,
                0.0,
                2998.0,
            ),
            (
                "grown = reduce(range(3000), (acc, i) => {...acc, [\"k\" + to_string(i)]: i}, {start: -1})",
                3001,
                -1.0,
                2999.0,
            ),
            (
                "grow = (n, acc) => if n == 0 then acc else grow(n - 1, {...acc, [\"k\" + to_string(n)]: n})\ngrown = grow(900, {})",
                900,
                900.0,
                1.0,
            ),
        ] {
            let session = Session::new();
            session.evaluate(code).unwrap();

            let values = session.inspect("grown", |record, _| record.as_slice().to_vec());
            assert_eq!(values.len(), len, "{code}");
            assert_eq!(values[0], Value::Number(first), "{code}");
            assert_eq!(values[len - 1], Value::Number(last), "{code}");

            // A block for every time that the record outgrows the one before, which is twice
            // as large every time, rather than one for each of the steps.
            let blocks = session.blocks(3);
            assert!(blocks <= 16, "{code}: {blocks} blocks");

            let sharing = session.sharing("grown");
            assert!(
                sharing > len / 8,
                "{code}: {sharing} records share the last block"
            );

            // What the heap holds is in proportion to the number of steps.
            let bytes = session.heap.borrow().approx_bytes();
            assert!(bytes < 2_000_000, "{code}: {bytes} bytes");
        }
    }

    #[test]
    fn records_are_what_they_were_before_records_shared_their_values() {
        let session = Session::new();
        session
            .evaluate(
                "r = {...{a: 1, b: 2}, c: 3}\n\
                 grown = reduce(range(20), (acc, i) => {...acc, [\"k\" + to_string(i)]: i}, {})\n\
                 longer = {...grown, more: true}",
            )
            .unwrap();

        for (code, expected) in [
            ("keys(r)", "[a, b, c]"),
            ("values(r)", "[1, 2, 3]"),
            ("entries(r)", "[[a, 1], [b, 2], [c, 3]]"),
            ("r == {c: 3, b: 2, a: 1}", "true"),
            ("r != {c: 3, b: 2, a: 1}", "false"),
            ("[r.c, r[\"c\"], r.d, r[\"d\"]]", "[3, 3, null, null]"),
            ("keys({...{a: 1, b: 2}, a: 3})", "[a, b]"),
            ("typeof(r)", "record"),
            ("to_string(r)", "{a: 1, b: 2, c: 3}"),
            (
                "unique([r, {a: 1, b: 2, c: 3}, {...r}, {...r, c: 4}])",
                "[{a: 1, b: 2, c: 3}, {a: 1, b: 2, c: 4}]",
            ),
            ("r.a + r.b + r.c", "6"),
            ("{...r, a: r.a + 1}.a", "2"),
            (
                "sort_by([{...r, a: 3}, {...r, a: 2}], x => x.a)",
                "[{a: 2, b: 2, c: 3}, {a: 3, b: 2, c: 3}]",
            ),
            ("includes([r], {a: 1, b: 2, c: 3})", "true"),
            ("len(keys(grown))", "20"),
            (
                "[grown.k0, grown.k19, grown.k20, grown.more]",
                "[0, 19, null, null]",
            ),
            ("[longer.k0, longer.k19, longer.more]", "[0, 19, true]"),
            ("grown == longer", "false"),
            ("{...grown, more: true} == longer", "true"),
            ("{...longer} == longer", "true"),
            ("sum(...values(grown))", "190"),
            ("len(entries(longer))", "21"),
            ("keys(longer)[20]", "more"),
            ("{...grown, k3: 30}.k3 + grown.k3", "33"),
        ] {
            assert_eq!(session.outcome(code), expected, "{code}");
        }
    }

    #[test]
    fn records_that_share_their_values_survive_compaction() {
        let session = Session::new();

        session
            .evaluate(
                "grown = reduce(range(100), (acc, i) => {...acc, [\"k\" + to_string(i)]: [i, to_string(i)]}, {})\n\
                 more = {...grown, extra: \"x\"}\n\
                 other = {...grown, other: \"y\"}\n\
                 updated = {...grown, k5: \"five\"}\n\
                 kept = (steps => [steps[10], steps[12]])(reduce(range(50), (state, i) => {now: {...state.now, [\"s\" + to_string(i)]: {n: i}}, all: [...state.all, state.now]}, {now: {}, all: []}).all)",
            )
            .unwrap();

        let rendered = |session: &Session| {
            session.outcome(
                "[len(keys(grown)), grown.k99, grown.k0, grown.extra, more.extra, more.k50, \
                 other.other, other.extra, updated.k5, updated.k6, grown.k5, kept[0].s9, \
                 kept[0].s10, len(keys(kept[0])), len(keys(kept[1])), kept[1].s11, kept[1].s12]",
            )
        };
        let expected = "[100, [99, 99], [0, 0], null, x, [50, 50], y, null, five, [6, 6], [5, 5], \
                        {n: 9}, null, 10, 12, {n: 11}, null]";
        assert_eq!(rendered(&session), expected);

        let slots = session.heap.borrow().len();
        session.compact();
        assert!(session.heap.borrow().len() < slots);
        assert_eq!(rendered(&session), expected);

        // The records can be added to and read like before.
        session
            .evaluate(
                "again = {...more, y: 1}\n\
                 further = {...grown, z: [100, \"100\"]}\n\
                 inner = {...kept[1], t: {n: 0}}",
            )
            .unwrap();
        session.compact();

        assert_eq!(rendered(&session), expected);
        assert_eq!(
            session.outcome(
                "[again.extra, again.y, len(keys(again)), further.z, further.k99, len(keys(further)), \
                 grown.z, inner.t, inner.s11, len(keys(inner)), kept[1].t]"
            ),
            "[x, 1, 102, [100, 100], [99, 99], 101, null, {n: 0}, {n: 11}, 13, null]"
        );
    }

    #[test]
    fn compaction_gives_up_the_values_that_no_record_reads_any_more() {
        let session = Session::new();

        session
            .evaluate(
                "part = (steps => steps[12])(reduce(range(20000), (state, i) => {now: {...state.now, [\"k\" + to_string(i)]: [i]}, all: [...state.all, state.now]}, {now: {}, all: []}).all)",
            )
            .unwrap();
        assert!(session.heap.borrow().len() > 20_000);

        // Other records have their values behind those of the record.
        assert!(session.sharing("part") > 2);
        session.evaluate("aside = {...part, more: [12]}").unwrap();
        assert!(!session.share("aside", "part"));

        session.compact();

        // The lists that the dropped values were are gone with them.
        let slots = session.heap.borrow().len();
        assert!(slots < 100, "{slots} slots");
        let bytes = session.heap.borrow().approx_bytes();
        assert!(bytes < 10_000, "{bytes} bytes");
        assert_eq!(
            session.outcome("part"),
            "{k0: [0], k1: [1], k2: [2], k3: [3], k4: [4], k5: [5], k6: [6], k7: [7], k8: [8], k9: [9], k10: [10], k11: [11]}"
        );
        assert_eq!(
            session.outcome("[aside.more, aside.k11, len(keys(aside))]"),
            "[[12], [11], 13]"
        );

        // The record that is left is added to where its values are.
        session.evaluate("longer = {...part, more: [10]}").unwrap();
        assert_eq!(session.sharing("part"), 2);
        assert_eq!(
            session.outcome("[longer.more, longer.k11, part.more, aside.more]"),
            "[[10], [11], null, [12]]"
        );
    }

    #[test]
    fn a_copy_of_a_heap_has_records_of_its_own() {
        let session = Session::new();
        session
            .evaluate("base = {...{a: 1}, b: 2}\ngrown = {...base, c: 3}")
            .unwrap();

        let copy = Session {
            heap: Rc::new(RefCell::new(session.heap.borrow().clone())),
            bindings: Rc::clone(&session.bindings),
        };

        let shared = session.inspect("grown", |original, _| {
            copy.inspect("grown", |copied, _| {
                assert!(original == copied);
                original.shares_values_with(copied)
            })
        });
        assert!(!shared);

        assert_eq!(copy.outcome("{...grown, d: 4}"), "{a: 1, b: 2, c: 3, d: 4}");
        assert_eq!(
            session.outcome("{...grown, d: 5}"),
            "{a: 1, b: 2, c: 3, d: 5}"
        );
        assert_eq!(
            copy.outcome("[base, grown]"),
            "[{a: 1, b: 2}, {a: 1, b: 2, c: 3}]"
        );
        assert_eq!(
            session.outcome("[base, grown]"),
            "[{a: 1, b: 2}, {a: 1, b: 2, c: 3}]"
        );
    }
}

// A call that names a built-in function is compiled to an instruction that calls that
// function, and for the functions that are called the most to one that does what the function
// does where that takes no more than looking at numbers, or at a list or a string. Nothing
// may tell either from calling the function through its value.
#[cfg(test)]
mod built_in_call_tests {
    use crate::ast::Expr;
    use crate::bytecode::{Instr, Op};
    use crate::compiler::{compile_chunk, compile_lambda};
    use crate::environment::Environment;
    use crate::fast_parser::{Statement, parse_program};
    use crate::heap::Heap;
    use crate::vm::Vm;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// The functions of one argument that have an instruction.
    const OF_ONE: [(&str, Op); 16] = [
        ("sqrt", Op::Sqrt),
        ("sin", Op::Sin),
        ("cos", Op::Cos),
        ("tan", Op::Tan),
        ("asin", Op::Asin),
        ("acos", Op::Acos),
        ("atan", Op::Atan),
        ("log", Op::Log),
        ("log10", Op::Log10),
        ("exp", Op::Exp),
        ("abs", Op::Abs),
        ("floor", Op::Floor),
        ("ceil", Op::Ceil),
        ("round", Op::Round),
        ("trunc", Op::Trunc),
        ("len", Op::Len),
    ];

    /// The functions that have an instruction for two arguments.
    const OF_TWO: [(&str, Op); 2] = [("min", Op::Min), ("max", Op::Max)];

    /// Values of every kind, and the numbers that functions tell apart.
    const VALUES: [&str; 30] = [
        "0",
        "0 * -1",
        "0.5",
        "-0.5",
        "1.5",
        "2.5",
        "-1.5",
        "-2.5",
        "3",
        "-7",
        "1e308",
        "1e-320",
        "0.49999999999999994",
        "4503599627370497",
        "-4503599627370497.5",
        "nan",
        "inf",
        "-inf",
        "\"héllo\"",
        "\"\"",
        "[1, 2, 3]",
        "[]",
        "[4, 9]",
        "[\"a\"]",
        "{a: 1}",
        "null",
        "true",
        "x => x",
        "sqrt",
        "missing",
    ];

    /// A heap and the variables of the statements that were evaluated against it.
    struct Session {
        heap: Rc<RefCell<Heap>>,
        bindings: Rc<Environment>,
    }

    impl Session {
        fn new() -> Self {
            let session = Self {
                heap: Rc::new(RefCell::new(Heap::new())),
                bindings: Rc::new(Environment::new()),
            };
            for definition in ["nan = sqrt(-1)", "inf = 1 / 0"] {
                let bound = session.outcome(definition, true);
                assert!(!bound.contains(" at "), "{definition}: {bound}");
            }

            session
        }

        /// Evaluate the statement `code`, with the calls that name a built-in function
        /// compiled to the instructions they have (`direct`) or to calls of the function's
        /// value, and render what comes of it: the value, or the error with the place it
        /// points at.
        fn outcome(&self, code: &str, direct: bool) -> String {
            let statements = parse_program(code).expect("a program the fast parser reads");
            let [Statement::Expression(expr)] = &statements[..] else {
                panic!("{code} is not a single expression");
            };

            let result = compile_chunk(expr, direct, &self.bindings).and_then(|chunk| {
                Vm::new(Rc::clone(&self.heap), Rc::clone(&self.bindings)).run_chunk(
                    &chunk,
                    code.into(),
                    0,
                )
            });

            match result {
                Ok(value) => value.stringify_internal(&self.heap.borrow()),
                Err(error) => format!("{} at {:?}", error.message, error.span),
            }
        }

        /// Evaluate the statements of `code` one after the other, and render what comes of
        /// the last one.
        fn run(&self, code: &str) -> String {
            let mut last = String::new();
            for statement in code.lines() {
                last = self.outcome(statement, true);
            }

            last
        }
    }

    /// How many instructions with the operation `op` the function `code` compiles to.
    fn count(code: &str, op: Op) -> usize {
        let statements = parse_program(code).expect("a program the fast parser reads");
        let [Statement::Expression(expr)] = &statements[..] else {
            panic!("{code} is not a single expression");
        };
        let Expr::Lambda { args, body, .. } = &expr.node else {
            panic!("{code} is not a function");
        };
        let proto = compile_lambda(&Rc::new(args.clone()), body, true).unwrap();

        let mut count = 0;
        let mut pc = 0;
        while pc < proto.code.len() {
            // SAFETY: `pc` only lands on instruction words, as number operands are skipped.
            let instr = unsafe { Instr::decode(proto.code[pc]) };
            count += usize::from(instr.op == op);
            pc += 1 + instr.operand_words();
        }

        count
    }

    /// How many instructions that call a value the function `code` compiles to.
    fn calls_of_values(code: &str) -> usize {
        count(code, Op::Call) + count(code, Op::CallList)
    }

    #[test]
    fn a_call_that_names_a_function_is_an_instruction() {
        for (function, op) in OF_ONE {
            let code = format!("x => {function}(x)");
            assert_eq!(count(&code, op), 1, "{code}");
            assert_eq!(calls_of_values(&code), 0, "{code}");
        }

        for (function, op) in OF_TWO {
            let code = format!("(a, b) => {function}(a, b)");
            assert_eq!(count(&code, op), 1, "{code}");
            assert_eq!(calls_of_values(&code), 0, "{code}");
        }

        for code in [
            "x => typeof(x)",
            "x => to_string(x)",
            "x => keys(x)",
            "x => round(x, 2)",
            "x => min(x)",
            "(a, b, c) => max(a, b, c)",
            "(a, b) => sum(a, b)",
            "(a, b) => concat(a, b)",
            "(a, b, c) => slice(a, b, c)",
            "(xs, f) => map(xs, f)",
            "(xs, f) => reduce(xs, f, 0)",
            "(xs, f) => sort_by(xs, f)",
        ] {
            assert_eq!(count(code, Op::CallBuiltIn), 1, "{code}");
            assert_eq!(calls_of_values(code), 0, "{code}");
        }
    }

    #[test]
    fn a_call_that_only_shows_what_it_passes_when_it_is_made_calls_the_value() {
        for (code, op) in [
            // A number of arguments that the function does not take.
            ("x => sqrt()", Op::Call),
            ("x => sqrt(x, x)", Op::Call),
            ("x => len(x, x)", Op::Call),
            ("x => min()", Op::Call),
            ("x => slice(x, 1)", Op::Call),
            ("x => typeof()", Op::Call),
            // Arguments that are spread.
            ("x => sqrt(...x)", Op::CallList),
            ("x => min(...x)", Op::CallList),
            ("x => max(1, ...x)", Op::CallList),
            ("x => typeof(...x)", Op::CallList),
        ] {
            assert_eq!(count(code, op), 1, "{code}");
            assert_eq!(count(code, Op::CallBuiltIn), 0, "{code}");
            for (_, instruction) in OF_ONE.into_iter().chain(OF_TWO) {
                assert_eq!(count(code, instruction), 0, "{code}");
            }
        }
    }

    #[test]
    fn an_instruction_comes_to_what_calling_the_function_comes_to() {
        let session = Session::new();
        let mut outcomes = std::collections::HashSet::new();

        let mut check = |code: String| {
            let direct = session.outcome(&code, true);
            assert_eq!(direct, session.outcome(&code, false), "{code}");
            outcomes.insert(direct);
        };

        for (function, _) in OF_ONE {
            for value in VALUES {
                check(format!("{function}({value})"));
                check(format!("1 / {function}({value})"));
                check(format!("[{function}({value}), {function}({value})]"));
            }
        }

        for (function, _) in OF_TWO {
            for first in VALUES {
                for second in VALUES {
                    check(format!("{function}({first}, {second})"));
                    check(format!("1 / {function}({first}, {second})"));
                }
            }
        }

        for call in [
            "typeof(V)",
            "to_string(V)",
            "to_number(V)",
            "to_bool(V)",
            "keys(V)",
            "values(V)",
            "entries(V)",
            "arity(V)",
            "sum(V)",
            "sum(V, 1)",
            "sum(1, V, 2)",
            "min(V)",
            "max(1, 2, V)",
            "avg(V, 3)",
            "range(V)",
            "range(1, V)",
            "head(V)",
            "tail(V)",
            "reverse(V)",
            "sort(V)",
            "unique(V)",
            "concat(V, V)",
            "slice(V, 0, 1)",
            "round(V, 1)",
            "includes(V, 1)",
            "join(V, \"-\")",
            "format(\"{}\", V)",
            "flatten(V)",
            "map(V, sqrt)",
            "map([4, 9], V)",
            "filter([4, 9], V)",
            "reduce([4, 9], V, 0)",
            "sort_by([4, 9], V)",
            "ugt(V, 1)",
        ] {
            for value in VALUES {
                check(call.replace('V', value));
            }
        }

        // The calls come to enough different things to tell the functions and their
        // arguments apart.
        assert!(outcomes.len() > 900, "{} outcomes", outcomes.len());
    }

    #[test]
    fn the_arguments_of_a_call_are_evaluated_before_it_fails() {
        let session = Session::new();
        for (code, expected) in [
            // The first thing that fails is an argument, from left to right.
            ("min(missing, other)", "unknown identifier: missing"),
            ("min(1, other)", "unknown identifier: other"),
            ("sum(1, other, missing)", "unknown identifier: other"),
            ("sqrt(missing)", "unknown identifier: missing"),
            ("len(missing)", "unknown identifier: missing"),
            // What a function cannot take only shows after that.
            ("min(\"a\", missing)", "unknown identifier: missing"),
            ("slice(null, missing, 1)", "unknown identifier: missing"),
        ] {
            for direct in [true, false] {
                let outcome = session.outcome(code, direct);
                assert!(outcome.starts_with(expected), "{code}: {outcome}");
            }
        }
    }

    #[test]
    fn the_smaller_and_the_larger_of_two_numbers_are_what_the_functions_find() {
        let session = Session::new();
        for (code, expected) in [
            (
                "[min(1, 2), min(2, 1), max(1, 2), max(2, 1)]",
                "[1, 1, 2, 2]",
            ),
            ("[min(-inf, inf), max(-inf, inf)]", "[-inf, inf]"),
            ("[min(inf, inf), max(-inf, -inf)]", "[inf, -inf]"),
            ("[min(3, 3), max(-3, -3)]", "[3, -3]"),
            ("[1 / min(0, 0), 1 / max(0 * -1, 0 * -1)]", "[inf, -inf]"),
            // A number that is none is passed over, and so are two of them.
            (
                "[min(nan, 1), min(1, nan), max(nan, 1), max(1, nan)]",
                "[1, 1, 1, 1]",
            ),
            ("[min(nan, nan), max(nan, nan)]", "[inf, -inf]"),
            ("[min(inf, nan), max(-inf, nan)]", "[inf, -inf]"),
        ] {
            for direct in [true, false] {
                assert_eq!(session.outcome(code, direct), expected, "{code}");
            }
        }

        // Which of two zeros that differ in their sign is found is up to the machine, but
        // not to how the function is called.
        for code in [
            "1 / min(0, 0 * -1)",
            "1 / min(0 * -1, 0)",
            "1 / max(0, 0 * -1)",
            "1 / max(0 * -1, 0)",
        ] {
            let found = session.outcome(code, true);
            assert!(found == "inf" || found == "-inf", "{code}: {found}");
            assert_eq!(found, session.outcome(code, false), "{code}");
        }
    }

    #[test]
    fn the_length_of_a_string_is_the_number_of_its_bytes() {
        let session = Session::new();
        for (code, expected) in [
            ("len(\"\")", "0"),
            ("len(\"abc\")", "3"),
            ("len(\"héllo\")", "6"),
            ("len(\"abc\" + \"déf\")", "7"),
            ("len([])", "0"),
            ("len([1, [2, 3]])", "2"),
            ("len(range(1000))", "1000"),
            ("len([...[1, 2], ...[3]])", "3"),
        ] {
            for direct in [true, false] {
                assert_eq!(session.outcome(code, direct), expected, "{code}");
            }
        }
    }

    #[test]
    fn a_function_that_is_called_too_deep_fails_as_its_call_does() {
        let calls = OF_ONE
            .into_iter()
            .map(|(function, _)| match function {
                "len" => "len([4])".to_string(),
                _ => format!("{function}(0.5)"),
            })
            .chain(
                OF_TWO
                    .into_iter()
                    .map(|(function, _)| format!("{function}(4, 3)")),
            )
            .chain(["typeof(4)", "sum(4, 3)", "round(0.25, 1)"].map(String::from));

        for call in calls {
            let function = call.split('(').next().unwrap();

            // The function calls itself `n` times before it makes the call, which puts the
            // call at the last depth to call from and then one deeper.
            let session = Session::new();
            let defined = session.run(&format!(
                "f = n => if n == 0 then {call} else [f(n - 1)][0]"
            ));
            assert!(!defined.contains(" at "), "{call}: {defined}");

            assert_eq!(
                session.outcome("f(999)", true),
                session.outcome(&call, true),
                "{call}"
            );

            let deeper = session.outcome("f(1000)", true);
            let expected =
                format!("in built-in function \"{function}\": maximum call depth of 1000 exceeded");
            assert!(deeper.contains(&expected), "{call}: {deeper}");
        }
    }

    #[test]
    fn a_call_in_a_loop_that_is_compiled_in_place_is_an_instruction_too() {
        let session = Session::new();
        for (code, expected) in [
            ("map([1, 4, 9], x => sqrt(x))", "[1, 2, 3]"),
            ("map([\"a\", \"bcd\", \"\"], s => len(s))", "[1, 3, 0]"),
            ("map([[], [1], [1, 2]], xs => len(xs) * 2)", "[0, 2, 4]"),
            ("reduce(range(10), (acc, i) => max(acc, i * 7 % 5), 0)", "4"),
            ("reduce(range(10), (acc, i) => min(acc, i - 4), 0)", "-4"),
            (
                "filter(range(9), i => floor(i / 2) == i / 2)",
                "[0, 2, 4, 6, 8]",
            ),
            (
                "map([1.5, -1.5], x => [floor(x), ceil(x), round(x), trunc(x), abs(x)])",
                "[[1, 2, 2, 1, 1.5], [-2, -1, -2, -1, 1.5]]",
            ),
            ("map([1, 2], x => typeof(x))", "[number, number]"),
        ] {
            for direct in [true, false] {
                assert_eq!(session.outcome(code, direct), expected, "{code}");
            }
        }

        for (code, expected) in [
            (
                "map([4, \"4\"], x => sqrt(x))",
                "in built-in function \"sqrt\": expected a number, but got a string",
            ),
            (
                "map([[1], 1], x => len(x))",
                "in built-in function \"len\": argument must be a list or string",
            ),
            (
                "map([1, null], x => max(x, 0))",
                "in built-in function \"max\": expected a number, but got a null",
            ),
        ] {
            let direct = session.outcome(code, true);
            assert!(direct.contains(expected), "{code}: {direct}");
            assert_eq!(direct, session.outcome(code, false), "{code}");
        }
    }
}
