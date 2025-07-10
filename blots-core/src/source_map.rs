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