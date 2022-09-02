use super::parser::Rule;
use pest::iterators::Pairs;

pub trait PrettyFormat {
    fn format(&self) -> String;
}

impl PrettyFormat for Pairs<'_, Rule> {
    fn format(&self) -> String {
        let mut result = String::new();
        let pairs = self.clone();

        for pair in pairs {
            result = format!("{}{}", result, pair.as_str());
        }

        return result;
    }
}
