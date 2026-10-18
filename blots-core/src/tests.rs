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
            pc += 1 + instr.has_number_operand() as usize;
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
