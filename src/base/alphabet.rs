use crate::base::*;
use std::collections::HashMap;
use num::traits::Euclid;

/// The notion of alphabet ordering in Stream.
#[derive(Clone, Default)]
pub enum Alphabet {
    /// Plain English 26-letter alphabet.
    #[default]
    Std26,
    /// An alphabet ordering created by explicitly listing the letters in order.
    Listed { vec: Vec<Char>, map: HashMap<Char, (isize, CharCase)> }
}

impl Alphabet {
    /// Returns the order of `chr` in the alphabet. Counts between 1 and the size.
    /// The information about the case of the character is preserved using `CharCase`.
    pub fn ord_case(&self, chr: &Char) -> Result<(isize, CharCase), BaseError> {
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
            Alphabet::Listed{map, ..} => {
                if let Some(rec) = map.get(chr) {
                    Ok(*rec)
                } else if let Some((ix, _)) = map.get(&chr.to_lowercase()) {
                    Ok((*ix, CharCase::Indeterminate))
                } else {
                    Err(format!("{chr}: not in alphabet").into())
                }
            }
        }
    }

    /// Returns the `ord`-th character in the alphabet in the given `CharCase`. Wraps around.
    pub fn chr_case(&self, ord: &Number, case: CharCase) -> Char {
        match self {
            Alphabet::Std26 => {
                let ord: u8 = ord.rem_euclid(&Number::from(26))
                    .try_into().unwrap(); // 0..26 well within range of u8
                let c = char::from(b'a' + (ord + 25u8) % 26u8);
                Char::from(match case {
                    CharCase::Upper => c.to_ascii_uppercase(),
                    _ => c
                })
            },
            Alphabet::Listed{vec, ..} => {
                let ix: usize = (ord - 1i32).rem_euclid(&Number::from(vec.len()))
                    .try_into().unwrap(); // rem_euclid(vec.len()) < vec.len() <= usize::MAX
                let c = &vec[ix];
                match case {
                    CharCase::Upper => c.to_uppercase(),
                    CharCase::Lower => c.to_lowercase(),
                    CharCase::Indeterminate => c.to_owned()
                }
            }
        }
    }

    /// Checks whether the alphabet contains `chr`.
    pub fn contains(&self, chr: &Char) -> bool {
        match self {
            Alphabet::Std26 => {
                let Char::Single(ch) = chr else {
                    return false;
                };
                matches!(ch.to_ascii_lowercase().try_into(), Ok(b'a'..=b'z'))
            },
            Alphabet::Listed{..} => self.ord_case(chr).is_ok()
        }
    }

    #[cfg(test)]
    fn c_plus_c(&self, lhs: &Char, rhs: &Char) -> Result<Char, BaseError> {
        let (index1, case) = self.ord_case(lhs)?;
        let (index2, _) = self.ord_case(rhs)?;
        Ok(self.chr_case(&Number::from(index1 + index2), case))
    }

    /// Compares two characters
    pub fn cmp(&self, x: &Char, y: &Char) -> Result<std::cmp::Ordering, BaseError> {
        let ((ox, _), (oy, _)) = (self.ord_case(x)?, self.ord_case(y)?);
        Ok(ox.cmp(&oy))
    }

    pub(crate) fn wrap_describe(&self, call: impl FnOnce(u32) -> String, prec: u32) -> String {
        match self {
            Alphabet::Std26 => call(prec),
            Alphabet::Listed{vec, ..} => format!("alpha({}, {})", Self::format(vec), call(0))
        }
    }

    fn format(vec: &[Char]) -> String {
        let mut iter = vec.iter();
        let first = iter.next().unwrap(); // nonemptiness checked in try_from
        let mut ret = '['.to_string();
        ret += &first.to_string();
        for chr in iter {
            ret += ", ";
            ret += &chr.to_string();
        }
        ret.push(']');
        ret
    }
}

impl TryFrom<Vec<Item>> for Alphabet {
    type Error = BaseError;

    fn try_from(vec: Vec<Item>) -> Result<Alphabet, BaseError> {
        let mut map = HashMap::new();
        if vec.is_empty() {
            return Err("alphabet is empty".into());
        }
        let mut char_vec = Vec::with_capacity(vec.len());
        for (ix, item) in vec.into_iter().enumerate() {
            let chr = match item {
                Item::Char(chr) => chr,
                _ => return Err(format!("expected character, found {item:?}").into())
            };
            let ix = (ix + 1) as isize;
            let lcase = chr.to_lowercase();
            let ucase = chr.to_uppercase();
            let prev = if lcase == ucase {
                map.insert(chr.to_owned(), (ix, CharCase::Indeterminate))
            } else {
                map.insert(lcase, (ix, CharCase::Lower))
                    .or(map.insert(ucase, (ix, CharCase::Upper)))
            };
            if prev.is_some() {
                return Err(format!("duplicate character {chr}").into());
            }
            char_vec.push(chr);
        }
        Ok(Alphabet::Listed{vec: char_vec, map})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_std26() {
        let abc = Alphabet::Std26;
        assert_eq!(abc.ord_case(&Char::from('a')), Ok((1isize, CharCase::Lower)));
        assert_eq!(abc.ord_case(&Char::from('Z')), Ok((26isize, CharCase::Upper)));
        assert!(abc.ord_case(&Char::from('@')).is_err());
        assert!(abc.ord_case(&Char::from('á')).is_err());
        assert!(abc.ord_case(&Char::from("ch")).is_err());

        assert_eq!(abc.chr_case(&Number::from(1), CharCase::Lower), Char::from('a'));
        assert_eq!(abc.chr_case(&Number::from(0), CharCase::Indeterminate), Char::from('z'));
        assert_eq!(abc.chr_case(&Number::from(26), CharCase::Upper), Char::from('Z'));
        assert_eq!(abc.chr_case(&Number::from(100), CharCase::Lower), Char::from('v'));
        assert_eq!(abc.chr_case(&Number::from(-1), CharCase::Lower), Char::from('y'));
        assert_eq!(abc.chr_case(&Number::from(-100), CharCase::Lower), Char::from('d'));

        assert_eq!(abc.c_plus_c(&Char::from('C'), &Char::from('e')), Ok(Char::from('H')));
        assert_eq!(abc.c_plus_c(&Char::from('x'), &Char::from('Y')), Ok(Char::from('w')));
        assert!(abc.c_plus_c(&Char::from('a'), &Char::from('á')).is_err());

        let plus = crate::parser::parse("1+2").unwrap();
        assert_eq!(abc.wrap_describe(|prec| plus.describe_inner(prec, &Default::default()), u32::MAX), "(1+2)");
    }

    #[test]
    fn test_custom_alpha() {
        let abc = Alphabet::try_from(vec![Item::new_char('b'), Item::new_char('á'), Item::new_char("Ch"), Item::new_char('a')]).unwrap();
        assert_eq!(abc.ord_case(&Char::from('a')), Ok((4isize, CharCase::Lower)));
        assert_eq!(abc.ord_case(&Char::from('Á')), Ok((2isize, CharCase::Upper)));
        assert_eq!(abc.ord_case(&Char::from("CH")), Ok((3isize, CharCase::Upper)));
        assert_eq!(abc.ord_case(&Char::from("Ch")), Ok((3isize, CharCase::Indeterminate)));
        assert!(abc.ord_case(&Char::from('c')).is_err());
        assert_eq!(abc.chr_case(&Number::from(3), CharCase::Lower), Char::from("ch"));
        assert_eq!(abc.chr_case(&Number::from(98), CharCase::Upper), Char::from('Á'));
        assert_eq!(abc.chr_case(&Number::from(99), CharCase::Upper), Char::from("CH"));
        assert_eq!(abc.chr_case(&Number::from(99), CharCase::Indeterminate), Char::from("Ch"));

        let abc = Alphabet::try_from(vec![Item::new_char('❤')]).unwrap();
        assert_eq!(abc.ord_case(&Char::from("❤")), Ok((1isize, CharCase::Indeterminate)));

        assert!(Alphabet::try_from(vec![Item::new_char('a'), Item::new_char('A')]).is_err());
        assert!(Alphabet::try_from(vec![Item::new_char('İ'), Item::new_char("i\u{307}")]).is_err());
        assert!(Alphabet::try_from(vec![Item::new_char("ch"), Item::new_char("Ch")]).is_err());
        assert!(Alphabet::try_from(vec![]).is_err());

        let abc = Alphabet::try_from(vec![Item::new_char('b'), Item::new_char('á'), Item::new_char("Ch"), Item::new_char('a')]).unwrap();
        let plus = crate::parser::parse("1+2").unwrap();
        assert_eq!(abc.wrap_describe(|prec| plus.describe_inner(prec, &Default::default()), u32::MAX), "alpha(['b', 'á', 'Ch', 'a'], 1+2)");
    }
}
