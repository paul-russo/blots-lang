use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct FunctionCallStats {
    pub name: String,
    pub start: std::time::Instant,
    pub end: std::time::Instant,
    pub start_var_env: Option<std::time::Instant>,
    pub end_var_env: Option<std::time::Instant>,
}

impl Display for FunctionCallStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start_var_env.is_some() && self.end_var_env.is_some() {
            write!(
                f,
                "FunctionCallStats {{ name: {}, duration: {}ms, var_env_duration: {}ms }}",
                self.name,
                (self.end - self.start).as_secs_f64() * 1_000.0,
                (self.end_var_env.unwrap() - self.start_var_env.unwrap()).as_secs_f64() * 1_000.0
            )
        } else {
            write!(
                f,
                "FunctionCallStats {{ name: {}, duration: {}ms, var_env_duration: None (built-in) }}",
                self.name,
                (self.end - self.start).as_secs_f64() * 1_000.0
            )
        }
    }
}
