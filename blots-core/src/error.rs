use crate::ast::Span;
use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use std::{fmt, rc::Rc};

/// Runtime error with source location information for beautiful error reporting
#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub span: Option<Span>,
    pub source: Option<Rc<str>>,
}

impl RuntimeError {
    /// Create a new runtime error without source location
    pub fn new(message: String) -> Self {
        Self {
            message,
            span: None,
            source: None,
        }
    }

    /// Create a runtime error with source location
    pub fn with_span(message: String, span: Span, source: Rc<str>) -> Self {
        Self {
            message,
            span: Some(span),
            source: Some(source),
        }
    }

    /// Add span information to an existing error (useful for wrapping function call errors)
    pub fn with_call_site(self, span: Span, source: Rc<str>) -> Self {
        // If the error already has span info, keep it (it's more specific)
        // Otherwise, add the call site span
        if self.span.is_some() {
            self
        } else {
            Self {
                message: self.message,
                span: Some(span),
                source: Some(source),
            }
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // If we have both span and source, use ariadne for beautiful formatting
        if let (Some(span), Some(source)) = (&self.span, &self.source) {
            let mut output = Vec::new();

            // Disable colors when compiling for WASM to avoid ANSI escape sequences
            // in browser/Node.js environments where they won't be interpreted
            #[cfg(target_arch = "wasm32")]
            let config = Config::default().with_color(false);

            #[cfg(not(target_arch = "wasm32"))]
            let config = Config::default();

            Report::build(ReportKind::Error, (), span.start_byte)
                .with_message(&self.message)
                .with_label(
                    Label::new(span.start_byte..span.end_byte)
                        .with_message(&self.message)
                        .with_color(Color::Red),
                )
                .with_config(config)
                .finish()
                .write(Source::from(&**source), &mut output)
                .map_err(|_| fmt::Error)?;

            let output_str = String::from_utf8(output).map_err(|_| fmt::Error)?;
            write!(f, "{}", output_str)
        } else {
            // Fallback to simple error message
            write!(f, "[evaluation error] {}", self.message)
        }
    }
}

impl std::error::Error for RuntimeError {}

// Allow conversion from anyhow::Error to RuntimeError (for backwards compatibility)
impl From<anyhow::Error> for RuntimeError {
    fn from(err: anyhow::Error) -> Self {
        RuntimeError::new(err.to_string())
    }
}

// Allow conversion from &str for convenience
impl From<&str> for RuntimeError {
    fn from(s: &str) -> Self {
        RuntimeError::new(s.to_string())
    }
}

// Allow conversion from String for convenience
impl From<String> for RuntimeError {
    fn from(s: String) -> Self {
        RuntimeError::new(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_error_with_span() {
        let source = "output x = undefined_variable + 5".to_string();

        // Simulate an error at the location of "undefined_variable"
        let span = Span::new(11, 29, 1, 12); // byte positions 11-29, line 1, col 12

        let error = RuntimeError::with_span(
            "unknown identifier: undefined_variable".to_string(),
            span,
            source.into(),
        );

        let error_msg = format!("{}", error);

        // The error message should contain the source line and point to the error location
        assert!(error_msg.contains("undefined_variable"));
        println!("\n=== DEMO: Improved Error Message ===");
        println!("{}", error);
        println!("=====================================\n");
    }

    #[test]
    fn test_runtime_error_without_span() {
        let error = RuntimeError::new("simple error".to_string());
        let error_msg = format!("{}", error);
        assert_eq!(error_msg, "[evaluation error] simple error");
    }
}
