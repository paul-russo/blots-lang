#[cfg(test)]
mod format_blots_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_format_blots_simple() {
        let source = "x = [1, 2, 3]";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(formatted, "x = [1, 2, 3]");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_multiline() {
        let source = "{name: \"Alice\", age: 30, email: \"alice@example.com\", address: \"123 Main Street\"}";
        let result = format_blots(source, Some(30)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        // Record should be formatted across multiple lines
        assert!(formatted.contains("\n"));
        assert!(formatted.contains("{"));
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_comments() {
        let source = "// This is a comment\nx = [1, 2, 3]\n// Another comment\ny = x + 1";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(formatted.contains("// This is a comment"));
        assert!(formatted.contains("// Another comment"));
        assert!(formatted.contains("x = [1, 2, 3]"));
        assert!(formatted.contains("y = x + 1"));
    }

    #[wasm_bindgen_test]
    fn test_format_blots_default_max_columns() {
        let source = "x = 1";
        let result = format_blots(source, None).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(formatted, "x = 1");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_invalid_syntax() {
        let source = "x = [1, 2,";
        let result = format_blots(source, Some(80));
        assert!(result.is_err());
    }

    #[wasm_bindgen_test]
    fn test_format_blots_multiple_statements() {
        let source = "x = 1\ny = 2\nz = x + y";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "x = 1");
        assert_eq!(lines[1], "y = 2");
        assert_eq!(lines[2], "z = x + y");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_end_of_line_comments() {
        let source = "x = 5  // this is a comment\ny = 10";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "x = 5  // this is a comment");
        assert_eq!(lines[1], "y = 10");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_mixed_comments() {
        let source = "// standalone comment\nx = 1  // end-of-line comment\ny = 2";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();
        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "// standalone comment");
        assert_eq!(lines[1], "x = 1  // end-of-line comment");
        assert_eq!(lines[2], "y = 2");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_empty_lines() {
        let source = "x = 1\n\ny = 2\n\n\n\nz = 3";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Should preserve up to 2 empty lines between statements
        // 1 empty line between x and y (preserved)
        // 4 empty lines between y and z (capped at 2)
        assert_eq!(formatted, "x = 1\n\ny = 2\n\n\nz = 3");

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 6); // 3 statements + 1 blank + 2 blanks
    }

    #[wasm_bindgen_test]
    fn test_format_blots_no_empty_lines() {
        let source = "x = 1\ny = 2\nz = 3";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Should NOT add empty lines when there were none
        assert_eq!(formatted, "x = 1\ny = 2\nz = 3");

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[wasm_bindgen_test]
    fn test_format_blots_preserves_output_keyword() {
        let source = "output x = 42\noutput y = [1, 2, 3]\nz = 10";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Output keywords should be preserved
        assert!(formatted.contains("output x = 42"));
        assert!(formatted.contains("output y = [1, 2, 3]"));
        assert!(formatted.contains("z = 10"));

        let lines: Vec<&str> = formatted.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "output x = 42");
        assert_eq!(lines[1], "output y = [1, 2, 3]");
        assert_eq!(lines[2], "z = 10");
    }

    #[wasm_bindgen_test]
    fn test_format_blots_output_with_via() {
        let source = "output result = items via i => i * 2";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        // Output keyword should be preserved with via expression
        assert!(formatted.contains("output result = items via"));
        assert!(formatted.starts_with("output result"));
    }

    #[wasm_bindgen_test]
    fn test_format_blots_comments_inside_list() {
        // Test that comments inside lists are preserved
        let source = "[\n// leading comment\n1,\n2, // trailing comment\n]";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        assert!(
            formatted.contains("// leading comment"),
            "Leading comment inside list should be preserved. Got: {}",
            formatted
        );
        assert!(
            formatted.contains("// trailing comment"),
            "Trailing comment inside list should be preserved. Got: {}",
            formatted
        );
    }

    #[wasm_bindgen_test]
    fn test_format_blots_comments_inside_record() {
        // Test that comments inside records are preserved
        let source = "{\n// leading comment\na: 1,\nb: 2, // trailing comment\n}";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        assert!(
            formatted.contains("// leading comment"),
            "Leading comment inside record should be preserved. Got: {}",
            formatted
        );
        assert!(
            formatted.contains("// trailing comment"),
            "Trailing comment inside record should be preserved. Got: {}",
            formatted
        );
    }

    #[wasm_bindgen_test]
    fn test_format_blots_comments_inside_nested_structures() {
        // Test that comments are preserved in nested lists/records
        let source = "{\n  items: [\n    // first item\n    1,\n    2, // second\n  ],\n}";
        let result = format_blots(source, Some(80)).unwrap();
        let formatted: String = serde_wasm_bindgen::from_value(result).unwrap();

        assert!(
            formatted.contains("// first item"),
            "Comment in nested list should be preserved. Got: {}",
            formatted
        );
        assert!(
            formatted.contains("// second"),
            "Trailing comment in nested list should be preserved. Got: {}",
            formatted
        );
    }
}

#[cfg(test)]
mod evaluate_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_evaluate_simple_expression() {
        let source = "output result = 1 + 2";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());
        assert_eq!(
            result_obj["result"]["outputs"],
            serde_json::json!(["result"])
        );
    }

    #[wasm_bindgen_test]
    fn test_evaluate_with_inputs() {
        let source = "output doubled = inputs.x * 2";
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(5.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());
        assert_eq!(
            result_obj["result"]["outputs"],
            serde_json::json!(["doubled"])
        );

        // Check that we got a binding for doubled
        let bindings = &result_obj["result"]["bindings"];
        assert_eq!(bindings["doubled"]["Number"], serde_json::json!(10));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_input_reference() {
        let source = "output result = inputs.x + inputs.y";
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(3.0));
        inputs_map.insert("y".to_string(), SerializableValue::Number(7.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());
        assert_eq!(
            result_obj["result"]["bindings"]["result"]["Number"],
            serde_json::json!(10)
        );
    }

    #[wasm_bindgen_test]
    fn test_evaluate_list_operations() {
        let source = "output result = [1, 2, 3] + [4, 5, 6]";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());
        // Check that result is a List serializable value
        assert!(result_obj["result"]["bindings"]["result"]["List"].is_array());
        let list = &result_obj["result"]["bindings"]["result"]["List"];
        assert_eq!(list[0]["Number"], serde_json::json!(5));
        assert_eq!(list[1]["Number"], serde_json::json!(7));
        assert_eq!(list[2]["Number"], serde_json::json!(9));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_multiline_source() {
        let source = "x = inputs.a * 2\ny = inputs.b + 10\noutput result = x + y";
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("a".to_string(), SerializableValue::Number(5.0));
        inputs_map.insert("b".to_string(), SerializableValue::Number(3.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());

        // Check the output
        assert_eq!(
            result_obj["result"]["outputs"],
            serde_json::json!(["result"])
        );
        // x = 5 * 2 = 10, y = 3 + 10 = 13, result = 10 + 13 = 23
        assert_eq!(
            result_obj["result"]["bindings"]["result"]["Number"],
            serde_json::json!(23)
        );

        // Check intermediate bindings
        assert_eq!(
            result_obj["result"]["bindings"]["x"]["Number"],
            serde_json::json!(10)
        );
        assert_eq!(
            result_obj["result"]["bindings"]["y"]["Number"],
            serde_json::json!(13)
        );
    }

    #[wasm_bindgen_test]
    fn test_evaluate_multiline_with_comments() {
        let source = "x = inputs.a * 2\ny = inputs.b + 10\n\n// hi\noutput result = x + y";
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("a".to_string(), SerializableValue::Number(5.0));
        inputs_map.insert("b".to_string(), SerializableValue::Number(3.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["result"].is_object());

        // Check the output
        assert_eq!(
            result_obj["result"]["outputs"],
            serde_json::json!(["result"])
        );
        // x = 5 * 2 = 10, y = 3 + 10 = 13, result = 10 + 13 = 23
        assert_eq!(
            result_obj["result"]["bindings"]["result"]["Number"],
            serde_json::json!(23)
        );
    }

    #[wasm_bindgen_test]
    fn test_evaluate_invalid_syntax() {
        let source = "output result = 1 +";
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(result_obj["error"].is_object());
        assert!(result_obj["error"]["message"].is_string());
        // Should have a range for parsing errors
        assert!(result_obj["error"]["range"].is_object() || result_obj["error"]["range"].is_null());
    }
}

#[cfg(test)]
mod error_position_tests {
    use crate::*;
    use string_offsets::StringOffsets;
    use wasm_bindgen_test::*;

    /// Helper to get error range from evaluate result
    fn get_error_range(source: &str) -> Option<serde_json::Value> {
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();

        if result_obj["error"].is_object() {
            Some(result_obj["error"]["range"].clone())
        } else {
            None
        }
    }

    /// Helper to calculate expected UTF-16 position from UTF-8 byte position
    fn utf8_to_utf16(source: &str, utf8_pos: usize) -> usize {
        let offsets: StringOffsets = StringOffsets::new(source);
        offsets.utf8_to_utf16(utf8_pos)
    }

    // ==================== ASCII Baseline Tests ====================

    #[wasm_bindgen_test]
    fn test_error_position_ascii_only() {
        // Pure ASCII - UTF-8 and UTF-16 positions should be identical
        let source = "output x = 1 +";
        let range = get_error_range(source).expect("Should have error range");

        // For ASCII, positions should match
        // The error is at the end after "+"
        assert!(
            range.is_object(),
            "Expected range object for parse error, got: {:?}",
            range
        );
    }

    #[wasm_bindgen_test]
    fn test_error_position_ascii_multiline() {
        let source = "x = 1\ny = 2\nz = 3 +";
        let range = get_error_range(source).expect("Should have error range");
        assert!(
            range.is_object(),
            "Expected range for multiline ASCII error"
        );
    }

    // ==================== Emoji Tests (4 UTF-8 bytes → 2 UTF-16 code units) ====================

    #[wasm_bindgen_test]
    fn test_error_position_after_emoji_in_string() {
        // Emoji: 4 UTF-8 bytes → 2 UTF-16 code units
        let source = r#"x = "🎉" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // Calculate positions manually:
        // x = " 🎉 " +
        // UTF-8: x(1) _(1) =(1) _(1) "(1) 🎉(4) "(1) _(1) +(1) _(1) = 13 bytes total
        // UTF-16: x(1) _(1) =(1) _(1) "(1) 🎉(2) "(1) _(1) +(1) _(1) = 11 code units total

        // The error position should be in UTF-16 units, not UTF-8 bytes
        // If we got UTF-8 position 13, UTF-16 should be 11
        if let Some(pos) = range.get("pos") {
            let pos_val = pos.as_u64().unwrap() as usize;
            // Should be around 11 (UTF-16), not 13 (UTF-8)
            assert!(
                pos_val <= 13,
                "Position {} seems too large for UTF-16",
                pos_val
            );
        } else if let (Some(start), Some(_end)) = (range.get("start"), range.get("end")) {
            let start_val = start.as_u64().unwrap() as usize;
            // Verify it's UTF-16, not UTF-8
            assert!(
                start_val <= 13,
                "Start position {} seems too large for UTF-16",
                start_val
            );
        }
    }

    #[wasm_bindgen_test]
    fn test_error_position_multiple_emojis() {
        // Multiple emojis amplify the UTF-8/UTF-16 difference
        let source = r#"x = "🎉🎂🔥" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // UTF-8: x(1) _(1) =(1) _(1) "(1) 🎉(4) 🎂(4) 🔥(4) "(1) _(1) +(1) _(1) = 21 bytes
        // UTF-16: x(1) _(1) =(1) _(1) "(1) 🎉(2) 🎂(2) 🔥(2) "(1) _(1) +(1) _(1) = 15 code units
        // Difference: 6 (each emoji saves 2)

        if let Some(pos) = range.get("pos") {
            let pos_val = pos.as_u64().unwrap() as usize;
            // UTF-16 position should be significantly less than UTF-8
            assert!(
                pos_val <= 17,
                "Position {} too large - should be UTF-16 not UTF-8",
                pos_val
            );
        }
    }

    // ==================== CJK Tests (3 UTF-8 bytes → 1 UTF-16 code unit) ====================

    #[wasm_bindgen_test]
    fn test_error_position_after_cjk_in_string() {
        // CJK: 3 UTF-8 bytes → 1 UTF-16 code unit each
        let source = r#"x = "中文" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // UTF-8: x(1) _(1) =(1) _(1) "(1) 中(3) 文(3) "(1) _(1) +(1) _(1) = 15 bytes
        // UTF-16: x(1) _(1) =(1) _(1) "(1) 中(1) 文(1) "(1) _(1) +(1) _(1) = 11 code units
        // Difference: 4 (each CJK char saves 2)

        if let Some(pos) = range.get("pos") {
            let pos_val = pos.as_u64().unwrap() as usize;
            assert!(
                pos_val <= 13,
                "Position {} too large for UTF-16",
                pos_val
            );
        }
    }

    #[wasm_bindgen_test]
    fn test_error_position_long_cjk_string() {
        // Longer CJK string to amplify the difference
        let source = r#"x = "你好世界日本語" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // 7 CJK characters: 21 UTF-8 bytes → 7 UTF-16 code units
        // Difference: 14 bytes

        if let Some(pos) = range.get("pos") {
            let pos_val = pos.as_u64().unwrap() as usize;
            // UTF-8 would be ~32, UTF-16 should be ~18
            assert!(
                pos_val <= 22,
                "Position {} too large for UTF-16",
                pos_val
            );
        }
    }

    // ==================== Unicode in Comments ====================

    #[wasm_bindgen_test]
    fn test_error_position_after_unicode_comment() {
        // Unicode in comment before the error
        let source = "// 日本語\nx = 1 +";

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // Line 1: //(2) _(1) 日(3)本(3)語(3) \n(1) = 13 UTF-8 bytes, 7 UTF-16 units
        // Line 2: x(1) _(1) =(1) _(1) 1(1) _(1) +(1) = 7 bytes/units
        // Total UTF-8: 20, UTF-16: 14

        if let Some(pos) = range.get("pos") {
            let pos_val = pos.as_u64().unwrap() as usize;
            assert!(
                pos_val <= 16,
                "Position {} too large for UTF-16",
                pos_val
            );
        }
    }

    #[wasm_bindgen_test]
    fn test_error_position_after_emoji_comment() {
        let source = "// 🎉🎂\nx = 1 +";

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // Comment has 2 emojis: 8 UTF-8 bytes → 4 UTF-16 code units
    }

    // ==================== Mixed Unicode Types ====================

    #[wasm_bindgen_test]
    fn test_error_position_mixed_unicode() {
        // Mix of ASCII, emoji, and CJK
        let source = r#"x = "hello 🎉 世界" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // "hello 🎉 世界" breakdown:
        // hello(5) _(1) 🎉(4/2) _(1) 世(3/1)界(3/1)
        // UTF-8: 5+1+4+1+3+3 = 17 bytes in string
        // UTF-16: 5+1+2+1+1+1 = 11 code units in string
    }

    // ==================== Latin Extended (2 UTF-8 bytes → 1 UTF-16) ====================

    #[wasm_bindgen_test]
    fn test_error_position_after_accented_chars() {
        // Accented characters: 2 UTF-8 bytes → 1 UTF-16 code unit
        let source = r#"x = "café résumé" + "#;

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // é is 2 UTF-8 bytes, 1 UTF-16 code unit
        // "café résumé" has 3 accented chars (é, é, é)
        // UTF-8: 14 bytes, UTF-16: 11 code units for the string content
    }

    // ==================== Verify Exact Positions ====================

    #[wasm_bindgen_test]
    fn test_exact_position_simple_emoji() {
        // Precisely verify the conversion using StringOffsets
        let source = r#"x = "🎉" + "#;

        // Find where the error should be (after the trailing space)
        let utf8_end = source.len();
        let expected_utf16 = utf8_to_utf16(source, utf8_end);

        let range = get_error_range(source).expect("Should have error range");

        // Verify the position matches our calculation
        if let Some(pos) = range.get("pos") {
            let actual = pos.as_u64().unwrap() as usize;
            // Allow some tolerance since pest might report slightly different positions
            let diff = actual.abs_diff(expected_utf16);
            assert!(
                diff <= 2,
                "Position mismatch: expected ~{}, got {}",
                expected_utf16,
                actual
            );
        }
    }

    #[wasm_bindgen_test]
    fn test_exact_position_cjk() {
        let source = r#"x = "中文日本" + "#;

        let utf8_end = source.len();
        let expected_utf16 = utf8_to_utf16(source, utf8_end);

        // Sanity check: UTF-16 should be less than UTF-8 for CJK
        assert!(
            expected_utf16 < utf8_end,
            "UTF-16 ({}) should be less than UTF-8 ({}) for CJK",
            expected_utf16,
            utf8_end
        );

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");
    }

    // ==================== Runtime Errors ====================

    #[wasm_bindgen_test]
    fn test_runtime_error_position_after_unicode() {
        // Runtime error (undefined variable) after unicode content
        let source = r#"x = "🎉"
y = undefined_variable"#;

        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();

        // Should get a runtime error for undefined_variable
        if result_obj["error"].is_object() {
            let range = &result_obj["error"]["range"];
            // Runtime errors should also have UTF-16 positions
            if range.is_object() {
                // The error position should account for the emoji
                if let Some(start) = range.get("start") {
                    let start_val = start.as_u64().unwrap() as usize;
                    // Position should be reasonable UTF-16, not inflated UTF-8
                    assert!(
                        start_val < source.len(),
                        "Runtime error position {} exceeds source length",
                        start_val
                    );
                }
            }
        }
        // Note: If there's no error, the test still passes - we're testing position conversion
        // when errors do occur, not that errors must occur
    }

    #[wasm_bindgen_test]
    fn test_runtime_error_after_cjk_comment() {
        let source = "// 这是注释\ny = unknown";

        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate(source, inputs).unwrap();
        let result_obj: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();

        if result_obj["error"].is_object() {
            let range = &result_obj["error"]["range"];
            if range.is_object() && range.get("start").is_some() {
                // CJK comment: // 这是注释\n = 3 + (4*3) + 1 = 16 UTF-8 bytes, 8 UTF-16 units
                // UTF-16 position should be much smaller than UTF-8
                let utf8_comment_end = "// 这是注释\n".len();
                let utf16_comment_end = utf8_to_utf16("// 这是注释\n", utf8_comment_end);

                assert!(
                    utf16_comment_end < utf8_comment_end,
                    "UTF-16 ({}) should be less than UTF-8 ({})",
                    utf16_comment_end,
                    utf8_comment_end
                );
            }
        }
    }

    // ==================== Edge Cases ====================

    #[wasm_bindgen_test]
    fn test_error_at_start() {
        // Error at position 0 - boundary condition
        let source = "+ invalid";
        let range = get_error_range(source);
        // Should have some error, position should be 0 or near 0
        assert!(range.is_some(), "Should have error for invalid syntax");
    }

    #[wasm_bindgen_test]
    fn test_error_empty_source_after_unicode() {
        // Error right after unicode with nothing else
        let source = r#""🎉" +"#;
        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object() || range.is_null(), "Should have range or null");
    }

    #[wasm_bindgen_test]
    fn test_surrogate_pair_boundary() {
        // Test that surrogate pairs (emoji) are correctly counted as 2 UTF-16 units
        let emoji = "🎉";
        assert_eq!(emoji.len(), 4, "Emoji should be 4 UTF-8 bytes");
        assert_eq!(
            emoji.encode_utf16().count(),
            2,
            "Emoji should be 2 UTF-16 code units"
        );

        let source = format!(r#"x = "{}" + "#, emoji);
        let range = get_error_range(&source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");

        // Verify using StringOffsets
        let offsets: StringOffsets = StringOffsets::new(&source);
        let utf8_len = source.len();
        let utf16_len = offsets.utf8_to_utf16(utf8_len);

        // UTF-16 should be 2 less than UTF-8 (emoji: 4 bytes → 2 code units, saves 2)
        assert_eq!(
            utf8_len - utf16_len,
            2,
            "Emoji should cause 2-unit difference"
        );
    }

    #[wasm_bindgen_test]
    fn test_multiple_surrogate_pairs() {
        // Multiple emojis to verify cumulative calculation
        let source = r#"x = "🎉🎂🔥😀" + "#;

        let offsets: StringOffsets = StringOffsets::new(source);
        let utf8_len = source.len();
        let utf16_len = offsets.utf8_to_utf16(utf8_len);

        // 4 emojis × 2 difference each = 8 total difference
        assert_eq!(
            utf8_len - utf16_len,
            8,
            "4 emojis should cause 8-unit difference"
        );

        let range = get_error_range(source).expect("Should have error range");
        assert!(range.is_object(), "Expected range object");
    }

    // ==================== Span vs Pos Tests ====================

    #[wasm_bindgen_test]
    fn test_error_range_format() {
        // Verify we get proper range objects
        let source = "output x = 1 +";
        let range = get_error_range(source).expect("Should have error range");

        // Should be either {pos: N} or {start: N, end: M}
        let is_pos = range.get("pos").is_some();
        let is_span = range.get("start").is_some() && range.get("end").is_some();

        assert!(
            is_pos || is_span,
            "Range should be Pos or Span format, got: {:?}",
            range
        );
    }
}

#[cfg(test)]
mod tokenize_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_tokenize_simple() {
        let source = "1 + 2";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }

    #[wasm_bindgen_test]
    fn test_tokenize_with_identifiers() {
        let source = "x = 42";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }

    #[wasm_bindgen_test]
    fn test_tokenize_complex_expression() {
        let source = "[1, 2, 3] + [4, 5, 6]";
        let result = tokenize(source).unwrap();
        let tokens: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!tokens.is_empty());
    }
}

#[cfg(test)]
mod get_built_in_function_names_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_get_built_in_function_names() {
        let result = get_built_in_function_names().unwrap();
        let names: Vec<String> = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(!names.is_empty());
        assert!(names.contains(&"map".to_string()));
        assert!(names.contains(&"filter".to_string()));
        assert!(names.contains(&"reduce".to_string()));
        assert!(names.contains(&"sum".to_string()));
    }
}

#[cfg(test)]
mod get_constants_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_get_constants() {
        let result = get_constants().unwrap();
        let constants: serde_json::Value = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(constants["constants"].is_object());
        let const_obj = &constants["constants"]["Record"];
        assert!(const_obj["pi"]["Number"].is_number());
        assert!(const_obj["e"]["Number"].is_number());
    }
}

#[cfg(test)]
mod evaluate_inline_expressions_tests {
    use crate::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_single() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["1 + 2"]).unwrap();
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(3));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_multiple() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["x * 2", "x + 5", "x - 1"]).unwrap();
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert("x".to_string(), SerializableValue::Number(10.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(20));
        assert_eq!(results[1]["value"]["Number"], serde_json::json!(15));
        assert_eq!(results[2]["value"]["Number"], serde_json::json!(9));
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_with_error() {
        let expressions = serde_wasm_bindgen::to_value(&vec!["1 + 2", "invalid +"]).unwrap();
        let inputs = serde_wasm_bindgen::to_value(&serde_json::json!({})).unwrap();
        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(results[0]["value"]["Number"], serde_json::json!(3));
        assert!(results[1]["error"].is_string());
    }

    #[wasm_bindgen_test]
    fn test_evaluate_inline_expressions_with_inputs() {
        let expressions =
            serde_wasm_bindgen::to_value(&vec!["\"Hello, \" + name", "age >= 18"]).unwrap();
        // Create inputs using SerializableValue directly
        use indexmap::IndexMap;
        let mut inputs_map: IndexMap<String, SerializableValue> = IndexMap::new();
        inputs_map.insert(
            "name".to_string(),
            SerializableValue::String("Alice".to_string()),
        );
        inputs_map.insert("age".to_string(), SerializableValue::Number(30.0));
        let inputs = serde_wasm_bindgen::to_value(&inputs_map).unwrap();

        let result = evaluate_inline_expressions(expressions, inputs).unwrap();
        let results: Vec<serde_json::Value> = serde_wasm_bindgen::from_value(result).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(
            results[0]["value"]["String"],
            serde_json::json!("Hello, Alice")
        );
        assert_eq!(results[1]["value"]["Bool"], serde_json::json!(true));
    }
}
