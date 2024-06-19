use crate::base::*;
use std::fmt::{Display, Formatter, Debug};
use num::traits::Euclid;

/// A 'character' in Stream may represent a single code point or a multigraph (such as 'dz').
#[derive(Debug, Clone, PartialEq)]
pub enum Char {
    Single(char),
    Multi(String)
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Case {
    /// All lowercase.
    Lower,
    /// Contains some uppercase.
    Upper
}

impl Char {
    fn case(&self) -> Case {
        let lower = match self {
            Char::Single(ch) => ch.is_lowercase(),
            Char::Multi(st) => st == &st.to_lowercase()
        };
        match lower {
            true => Case::Lower,
            false => Case::Upper
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
        match self {
            Char::Single(c) => write!(f, "{}", escape(*c))?,
            Char::Multi(s) => {
                for c in s.chars() {
                    write!(f, "{}", escape(c))?;
                }
            }
        }
        Ok(())
    }
}

#[test]
fn test_char() {
    assert_eq!(Char::from('a'), Char::Single('a'));
    assert_eq!(Char::from("a"), Char::Single('a'));
    assert_eq!(Char::from('❤'), Char::Single('❤')); // multi-byte
    assert_eq!(Char::from("❤"), Char::Single('❤'));
    assert_eq!(Char::from("é"), Char::Multi("é".into())); // combining mark
    assert_eq!(Char::from("as"), Char::Multi("as".into()));
    assert_eq!(Char::from('\n').to_string(), "\\n");
}


/// The notion of alphabet ordering in Stream.
pub enum Alphabet {
    /// Plain English 26-letter alphabet.
    Std26,
    /// An alphabet ordering created by explicitly listing the letters in order.
    _Listed(Vec<Char>)
}

impl Alphabet {
    /// Returns the order of `chr` in the alphabet. Counts between 1 and the size.
    /// The information about the case of the character is preserved using `Case`.
    pub fn ord_case(&self, chr: &Char) -> Result<(isize, Case), StreamError> {
        match self {
            Alphabet::Std26 => {
                let Char::Single(ch) = chr else {
                    return Err(format!("{chr}: not in alphabet").into());
                };
                let case = chr.case();
                match ch.to_ascii_lowercase().try_into() {
                    Ok(ord @ b'a'..=b'z') => Ok(((ord - b'a' + 1).into(), case)),
                    _ => Err(format!("{chr}: not in alphabet").into())
                }
            },
            _ => todo!()
        }
    }

    /// Returns the `ord`-th character in the alphabet in the given `Case`. Wraps around.
    pub fn chr_case(&self, ord: &Number, case: Case) -> Char {
        match self {
            Alphabet::Std26 => {
                let ord: u8 = ord.rem_euclid(&Number::from(26)).try_into().unwrap();
                let c = char::from(b'a' + (ord + 25u8) % 26u8);
                Char::from(match case {
                    Case::Lower => c,
                    Case::Upper => c.to_ascii_uppercase()
                })
            },
            _ => todo!()
        }
    }

    /// The 'char + char' operation. Wraps around within the alphabet. Preserves the case of the 
    /// left-hand side operand.
    #[cfg(test)]
    pub fn c_plus_c(&self, lhs: &Char, rhs: &Char) -> Result<Char, StreamError> {
        let (index1, case) = self.ord_case(lhs)?;
        let (index2, _) = self.ord_case(rhs)?;
        Ok(self.chr_case(&Number::from(index1 + index2), case))
    }
}

#[test]
fn test_std26() {
    let abc = Alphabet::Std26;
    assert_eq!(abc.ord_case(&Char::from('a')), Ok((1isize, Case::Lower)));
    assert_eq!(abc.ord_case(&Char::from('Z')), Ok((26isize, Case::Upper)));
    assert!(abc.ord_case(&Char::from('@')).is_err());
    assert!(abc.ord_case(&Char::from('á')).is_err());
    assert!(abc.ord_case(&Char::Multi("ch".into())).is_err());

    assert_eq!(abc.chr_case(&Number::from(1), Case::Lower), Char::from('a'));
    assert_eq!(abc.chr_case(&Number::from(0), Case::Lower), Char::from('z'));
    assert_eq!(abc.chr_case(&Number::from(26), Case::Upper), Char::from('Z'));
    assert_eq!(abc.chr_case(&Number::from(100), Case::Lower), Char::from('v'));
    assert_eq!(abc.chr_case(&Number::from(-1), Case::Lower), Char::from('y'));
    assert_eq!(abc.chr_case(&Number::from(-100), Case::Lower), Char::from('d'));

    assert_eq!(abc.c_plus_c(&Char::from('C'), &Char::from('e')), Ok(Char::from('H')));
    assert_eq!(abc.c_plus_c(&Char::from('x'), &Char::from('Y')), Ok(Char::from('w')));
    assert!(abc.c_plus_c(&Char::from('a'), &Char::from('á')).is_err());
}
