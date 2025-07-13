use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceRange {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceMapping {
    pub source_range: SourceRange,
    pub generated_line: usize,
    pub generated_column: usize,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct SourceMap {
    pub version: u32,
    pub sources: Vec<String>,
    pub sources_content: Vec<String>,
    pub mappings: Vec<SourceMapping>,
    pub names: Vec<String>,
}

impl SourceMap {
    pub fn new(source_name: String, source_content: String) -> Self {
        SourceMap {
            version: 3,
            sources: vec![source_name],
            sources_content: vec![source_content],
            mappings: Vec::new(),
            names: Vec::new(),
        }
    }

    pub fn add_mapping(
        &mut self,
        source_line: usize,
        source_column: usize,
        source_end_line: usize,
        source_end_column: usize,
        generated_line: usize,
        generated_column: usize,
    ) {
        self.mappings.push(SourceMapping {
            source_range: SourceRange {
                start: SourcePosition {
                    line: source_line,
                    column: source_column,
                },
                end: SourcePosition {
                    line: source_end_line,
                    column: source_end_column,
                },
            },
            generated_line,
            generated_column,
        });
    }

    pub fn to_comment(&self) -> String {
        match serde_json::to_string(self) {
            Ok(json) => {
                let encoded = base64::encode(json);
                format!("//# sourceMappingURL=data:application/json;base64,{}", encoded)
            }
            Err(_) => String::new(),
        }
    }
}

pub struct SourceMapBuilder {
    source_map: SourceMap,
    current_line: usize,
    current_column: usize,
}

impl SourceMapBuilder {
    pub fn new(source_name: String, source_content: String) -> Self {
        SourceMapBuilder {
            source_map: SourceMap::new(source_name, source_content),
            current_line: 1,
            current_column: 0,
        }
    }

    pub fn track_generated_position(&mut self, generated_code: &str) {
        for ch in generated_code.chars() {
            if ch == '\n' {
                self.current_line += 1;
                self.current_column = 0;
            } else {
                self.current_column += 1;
            }
        }
    }

    pub fn add_mapping(
        &mut self,
        source_start: (usize, usize),
        source_end: (usize, usize),
    ) {
        self.source_map.add_mapping(
            source_start.0,
            source_start.1,
            source_end.0,
            source_end.1,
            self.current_line,
            self.current_column,
        );
    }

    pub fn get_current_position(&self) -> (usize, usize) {
        (self.current_line, self.current_column)
    }

    pub fn build(self) -> SourceMap {
        self.source_map
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorLocation {
    pub line: usize,
    pub column: usize,
    pub source_line: Option<String>,
    pub source_file: String,
}

pub fn map_js_position_to_source(
    js_line: usize,
    js_column: usize,
    source_map: &SourceMap,
) -> Option<ErrorLocation> {
    for mapping in &source_map.mappings {
        if mapping.generated_line == js_line && mapping.generated_column <= js_column {
            let source_content = &source_map.sources_content[0];
            let lines: Vec<&str> = source_content.lines().collect();
            
            let source_line = if mapping.source_range.start.line <= lines.len() {
                Some(lines[mapping.source_range.start.line - 1].to_string())
            } else {
                None
            };

            return Some(ErrorLocation {
                line: mapping.source_range.start.line,
                column: mapping.source_range.start.column,
                source_line,
                source_file: source_map.sources[0].clone(),
            });
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_map_builder_tracks_position() {
        let mut builder = SourceMapBuilder::new("test.blot".to_string(), "x = 1 + 2".to_string());
        
        // Simulate generating some JS code
        builder.track_generated_position("const ");
        assert_eq!(builder.get_current_position(), (1, 6));
        
        builder.track_generated_position("x = ");
        assert_eq!(builder.get_current_position(), (1, 10));
        
        builder.track_generated_position("1 + 2;\n");
        assert_eq!(builder.get_current_position(), (2, 0));
    }

    #[test]
    fn test_source_map_builder_with_newlines() {
        let mut builder = SourceMapBuilder::new("test.blot".to_string(), "x = 1\ny = 2".to_string());
        
        builder.track_generated_position("const x = 1;\n");
        assert_eq!(builder.get_current_position(), (2, 0));
        
        builder.track_generated_position("const y = 2;\n");
        assert_eq!(builder.get_current_position(), (3, 0));
    }

    #[test]
    fn test_source_map_add_mapping() {
        let mut source_map = SourceMap::new("test.blot".to_string(), "x = 1 + 2".to_string());
        
        source_map.add_mapping(1, 1, 1, 9, 1, 10);
        source_map.add_mapping(1, 5, 1, 7, 1, 14);
        
        assert_eq!(source_map.mappings.len(), 2);
        assert_eq!(source_map.mappings[0].source_range.start.line, 1);
        assert_eq!(source_map.mappings[0].source_range.start.column, 1);
        assert_eq!(source_map.mappings[0].generated_line, 1);
        assert_eq!(source_map.mappings[0].generated_column, 10);
    }

    #[test]
    fn test_source_map_to_comment() {
        let source_map = SourceMap::new("test.blot".to_string(), "x = 1".to_string());
        let comment = source_map.to_comment();
        
        assert!(comment.starts_with("//# sourceMappingURL=data:application/json;base64,"));
    }

    #[test]
    fn test_map_js_position_to_source() {
        let mut source_map = SourceMap::new("test.blot".to_string(), "x = 42\ny = true\nresult = x + y".to_string());
        
        // Add some mappings
        source_map.add_mapping(1, 1, 1, 6, 10, 0);   // x = 42
        source_map.add_mapping(2, 1, 2, 8, 11, 0);   // y = true
        source_map.add_mapping(3, 10, 3, 15, 12, 20); // x + y
        
        // Test exact match
        let location = map_js_position_to_source(12, 20, &source_map);
        assert!(location.is_some());
        let loc = location.unwrap();
        assert_eq!(loc.line, 3);
        assert_eq!(loc.column, 10);
        assert_eq!(loc.source_file, "test.blot");
        assert_eq!(loc.source_line, Some("result = x + y".to_string()));
        
        // Test no match
        let no_location = map_js_position_to_source(100, 100, &source_map);
        assert!(no_location.is_none());
    }

    #[test]
    fn test_source_map_builder_with_mappings() {
        let mut builder = SourceMapBuilder::new("test.blot".to_string(), "x = 1\ny = 2".to_string());
        
        // Track some generated code
        builder.track_generated_position("const $$_x = ");
        builder.add_mapping((1, 1), (1, 5));
        
        builder.track_generated_position("1;\n");
        
        builder.track_generated_position("const $$_y = ");
        builder.add_mapping((2, 1), (2, 5));
        
        builder.track_generated_position("2;\n");
        
        let source_map = builder.build();
        assert_eq!(source_map.mappings.len(), 2);
        
        // Check first mapping
        assert_eq!(source_map.mappings[0].source_range.start.line, 1);
        assert_eq!(source_map.mappings[0].generated_line, 1);
        assert_eq!(source_map.mappings[0].generated_column, 13);
        
        // Check second mapping
        assert_eq!(source_map.mappings[1].source_range.start.line, 2);
        assert_eq!(source_map.mappings[1].generated_line, 2);
        assert_eq!(source_map.mappings[1].generated_column, 13);
    }
}