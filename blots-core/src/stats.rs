use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct FunctionCallStats {
    pub name: String,
    pub start: std::time::Instant,
    pub end: std::time::Instant,
    pub start_var_env: Option<std::time::Instant>,
    pub end_var_env: Option<std::time::Instant>,
}

impl FunctionCallStats {
    pub fn total_duration_ms(&self) -> f64 {
        (self.end - self.start).as_secs_f64() * 1_000.0
    }

    pub fn var_env_duration_ms(&self) -> Option<f64> {
        self.start_var_env
            .and_then(|start| self.end_var_env.map(|end| (end - start).as_secs_f64() * 1_000.0))
    }

    pub fn body_duration_ms(&self) -> f64 {
        let var_env = self.var_env_duration_ms().unwrap_or(0.0);
        self.total_duration_ms() - var_env
    }
}

impl Display for FunctionCallStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start_var_env.is_some() && self.end_var_env.is_some() {
            write!(
                f,
                "FunctionCallStats {{ name: {}, duration: {}ms, var_env_duration: {}ms }}",
                self.name,
                self.total_duration_ms(),
                self.var_env_duration_ms().unwrap()
            )
        } else {
            write!(
                f,
                "FunctionCallStats {{ name: {}, duration: {}ms, var_env_duration: None (built-in) }}",
                self.name,
                self.total_duration_ms()
            )
        }
    }
}

#[derive(Debug, Default)]
pub struct ProfilingSummary {
    pub total_calls: usize,
    pub lambda_calls: usize,
    pub builtin_calls: usize,
    pub total_time_ms: f64,
    pub var_env_time_ms: f64,
    pub body_time_ms: f64,
    pub calls_by_name: HashMap<String, CallStats>,
}

#[derive(Debug, Clone)]
pub struct CallStats {
    pub count: usize,
    pub total_ms: f64,
    pub var_env_ms: f64,
    pub body_ms: f64,
    pub avg_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
}

impl ProfilingSummary {
    pub fn from_stats(stats: &[FunctionCallStats]) -> Self {
        let mut summary = ProfilingSummary::default();
        summary.total_calls = stats.len();

        let mut calls_by_name: HashMap<String, Vec<&FunctionCallStats>> = HashMap::new();
        for stat in stats {
            calls_by_name
                .entry(stat.name.clone())
                .or_insert_with(Vec::new)
                .push(stat);
        }

        for (name, calls) in calls_by_name.iter() {
            let is_lambda = calls.iter().any(|c| c.var_env_duration_ms().is_some());
            if is_lambda {
                summary.lambda_calls += calls.len();
            } else {
                summary.builtin_calls += calls.len();
            }

            let mut total_ms = 0.0_f64;
            let mut var_env_ms = 0.0_f64;
            let mut body_ms = 0.0_f64;
            let mut min_ms = f64::INFINITY;
            let mut max_ms = 0.0_f64;

            for call in calls.iter() {
                let call_total = call.total_duration_ms();
                let call_var_env = call.var_env_duration_ms().unwrap_or(0.0);
                let call_body = call.body_duration_ms();

                total_ms += call_total;
                var_env_ms += call_var_env;
                body_ms += call_body;
                min_ms = min_ms.min(call_total);
                max_ms = max_ms.max(call_total);
            }

            let count = calls.len();
            summary.calls_by_name.insert(
                name.clone(),
                CallStats {
                    count,
                    total_ms,
                    var_env_ms,
                    body_ms,
                    avg_ms: total_ms / count as f64,
                    min_ms,
                    max_ms,
                },
            );
        }

        // Calculate totals
        for stats in summary.calls_by_name.values() {
            summary.total_time_ms += stats.total_ms;
            summary.var_env_time_ms += stats.var_env_ms;
            summary.body_time_ms += stats.body_ms;
        }

        summary
    }

    pub fn print_summary(&self) {
        println!("\n=== Performance Profile ===");
        println!("Total function calls: {}", self.total_calls);
        println!("  Lambda calls: {}", self.lambda_calls);
        println!("  Built-in calls: {}", self.builtin_calls);
        println!();
        println!("Total time: {:.3}ms", self.total_time_ms);
        println!("  Variable environment setup: {:.3}ms ({:.1}%)", 
            self.var_env_time_ms,
            if self.total_time_ms > 0.0 {
                (self.var_env_time_ms / self.total_time_ms) * 100.0
            } else {
                0.0
            }
        );
        println!("  Function body execution: {:.3}ms ({:.1}%)",
            self.body_time_ms,
            if self.total_time_ms > 0.0 {
                (self.body_time_ms / self.total_time_ms) * 100.0
            } else {
                0.0
            }
        );
        println!();

        if !self.calls_by_name.is_empty() {
            println!("Top functions by total time:");
            let mut sorted: Vec<_> = self.calls_by_name.iter().collect();
            sorted.sort_by(|a, b| b.1.total_ms.partial_cmp(&a.1.total_ms).unwrap());
            
            for (name, stats) in sorted.iter().take(10) {
                let var_env_pct = if stats.total_ms > 0.0 {
                    (stats.var_env_ms / stats.total_ms) * 100.0
                } else {
                    0.0
                };
                println!(
                    "  {}: {} calls, {:.3}ms total (avg {:.3}ms, min {:.3}ms, max {:.3}ms)",
                    name, stats.count, stats.total_ms, stats.avg_ms, stats.min_ms, stats.max_ms
                );
                if stats.var_env_ms > 0.0 {
                    println!(
                        "    └─ var_env: {:.3}ms ({:.1}%), body: {:.3}ms ({:.1}%)",
                        stats.var_env_ms, var_env_pct,
                        stats.body_ms, 100.0 - var_env_pct
                    );
                }
            }
        }
    }
}
