use crate::base::*;

#[derive(Clone)]
pub struct LiteralString(Vec<Char>);

impl Stream<Char> for LiteralString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        Box::new(self.0.iter().map(Char::clone).map(Result::Ok))
    }

    fn len(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for LiteralString {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        let mut ret = String::new();
        ret.push('"');
        for ch in &self.0 {
            ret += &format!("{ch:#}");
        }
        ret.push('"');
        ret
    }
}

impl From<&str> for LiteralString {
    fn from(s: &str) -> Self {
        LiteralString(s.chars().map(Char::from).collect())
    }
}

impl From<Vec<Char>> for LiteralString {
    fn from(s: Vec<Char>) -> Self {
        LiteralString(s)
    }
}

impl std::ops::Deref for LiteralString {
    type Target = [Char];

    fn deref(&self) -> &[Char] {
        &self.0
    }
}
