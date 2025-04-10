use crate::base::*;

#[derive(Clone)]
pub struct LiteralString(Vec<Char>);

impl Stream for LiteralString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.0.iter().map(|x| Ok(Item::new_char(x.clone()))))
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for LiteralString {
    fn describe(&self, _: u32) -> String {
        let mut ret = String::new();
        ret.push('"');
        for ch in &self.0 {
            ret += &format!("{ch:#}");
        }
        ret.push('"');
        ret
    }
}

impl From<String> for LiteralString {
    fn from(s: String) -> Self {
        LiteralString(s.chars().map(Char::from).collect())
    }
}
