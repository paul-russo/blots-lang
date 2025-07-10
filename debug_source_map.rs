use blots_core::transpiler::transpile_to_js_with_source_map;

fn main() {
    let code = r#"x = 42
y = true
result = x + y"#;

    match transpile_to_js_with_source_map(code, "test.blot".to_string()) {
        Ok((js_code, source_map)) => {
            println!("JS Code generated: {} bytes", js_code.len());
            println!("Source map mappings: {}", source_map.mappings.len());
            println!("\nMappings:");
            for (i, mapping) in source_map.mappings.iter().enumerate() {
                println!("  {}: Blots {}:{} -> JS {}:{}", 
                    i,
                    mapping.source_range.start.line,
                    mapping.source_range.start.column,
                    mapping.generated_line,
                    mapping.generated_column
                );
            }
        }
        Err(e) => println!("Error: {}", e),
    }
}