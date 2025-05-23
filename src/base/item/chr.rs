use std::fmt::{Display, Formatter};

/// A 'character' in Stream may represent a single code point or a multigraph (such as 'dz').
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Char {
    Single(char),
    Multi(String)
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharCase {
    Indeterminate,
    Lower,
    Upper
}

impl Char {
    pub fn case(&self) -> CharCase {
        match self {
            Char::Single(ch) =>
                if ch.is_lowercase() { CharCase::Lower }
                else if ch.is_uppercase() { CharCase::Upper }
                else { CharCase::Indeterminate },
            Char::Multi(ch) =>
                if ch == &ch.to_lowercase() { CharCase::Lower }
                else if ch == &ch.to_uppercase() { CharCase::Upper }
                else { CharCase::Indeterminate }
        }
    }

    pub fn to_lowercase(&self) -> Char {
        match self {
            Char::Single(ch) => {
                if ch.is_lowercase() {
                    Char::Single(*ch)
                } else {
                    let mut lcase = ch.to_lowercase();
                    if lcase.len() == 1 {
                        Char::Single(lcase.next().unwrap())
                    } else {
                        Char::Multi(lcase.to_string())
                    }
                }
            },
            Char::Multi(ch) => Char::Multi(ch.to_lowercase())
        }
    }

    pub fn to_uppercase(&self) -> Char {
        match self {
            Char::Single(ch) => {
                if ch.is_uppercase() {
                    Char::Single(*ch)
                } else {
                    let mut ucase = ch.to_uppercase();
                    if ucase.len() == 1 {
                        Char::Single(ucase.next().unwrap())
                    } else {
                        Char::Multi(ucase.to_string())
                    }
                }
            },
            Char::Multi(ch) => Char::Multi(ch.to_uppercase())
        }
    }
}

impl From<char> for Char {
    fn from(c: char) -> Char {
        Char::Single(c)
    }
}

impl From<String> for Char {
    fn from(s: String) -> Char {
        let mut it = s.chars();
        if let (Some(c), None) = (it.next(), it.next()) {
            Char::Single(c)
        } else {
            Char::Multi(s)
        }
    }
}

impl From<&str> for Char {
    fn from(x: &str) -> Char {
        Char::from(x.to_string())
    }
}

impl Display for Char {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let alt = f.alternate();
        let escape = |c| -> String {
            match c {
                '\n' => "\\n".into(),
                '\r' => "\\r".into(),
                '\t' => "\\t".into(),
                '\\' => "\\\\".into(),
                '\'' => if alt { c.into() } else { "\\'".into() },
                '"' => if alt { "\\\"".into() } else { c.into() },
                _ => c.into()
            }
        };
        if !alt { write!(f, "'")? };
        match self {
            Char::Single(c) => write!(f, "{}", escape(*c))?,
            Char::Multi(s) => {
                for c in s.chars() {
                    write!(f, "{}", escape(c))?;
                }
            }
        }
        if !alt { write!(f, "'")? };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        assert_eq!(Char::from('a'), Char::Single('a'));
        assert_eq!(Char::from("a"), Char::Single('a'));
        assert_eq!(Char::from('❤'), Char::Single('❤')); // multi-byte
        assert_eq!(Char::from("❤"), Char::Single('❤'));
        assert_eq!(Char::from("é"), Char::Multi("é".into())); // combining mark
        assert_eq!(Char::from("as"), Char::Multi("as".into()));
        assert_eq!(Char::from('\n').to_string(), "'\\n'");
    }
}
