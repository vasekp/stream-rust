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
    Listed { vec: Vec<Char>, map: HashMap<Char, isize> }
}

impl Alphabet {
    /// Returns the order of `chr` in the alphabet. Counts between 1 and the size.
    /// The information about the case of the character is preserved using `CharCase`.
    pub fn ord(&self, chr: &Char) -> Result<isize, BaseError> {
        match self {
            Alphabet::Std26 => {
                let Char::Single(ch) = chr else {
                    return Err(format!("{chr}: not in alphabet").into());
                };
                match ch.to_ascii_lowercase().try_into() {
                    Ok(ord @ b'a'..=b'z') => Ok((ord - b'a' + 1).into()),
                    _ => Err(format!("{chr}: not in alphabet").into())
                }
            },
            Alphabet::Listed{map, ..} => {
                map.get(chr)
                    .or_else(|| map.get(&chr.to_lowercase()))
                    .copied()
                    .ok_or_else(|| format!("{chr}: not in alphabet").into())
            }
        }
    }

    /// Returns the `ord`-th character in the alphabet in the given `CharCase`. Wraps around.
    pub fn chr(&self, ord: &Number, case: CharCase) -> Char {
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
                    _ => c.to_owned()
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
            Alphabet::Listed{..} => self.ord(chr).is_ok()
        }
    }

    #[cfg(test)]
    fn c_plus_c(&self, lhs: &Char, rhs: &Char) -> Result<Char, BaseError> {
        let case = lhs.case();
        let index1 = self.ord(lhs)?;
        let index2 = self.ord(rhs)?;
        Ok(self.chr(&Number::from(index1 + index2), case))
    }

    pub(crate) fn iter(&self) -> Box<dyn Iterator<Item = Char> + '_> {
        match self {
            Alphabet::Std26 => Box::new((0..26).map(|i| Char::from((b'a' + i) as char))),
            Alphabet::Listed{vec, ..} => Box::new(vec.iter().cloned())
        }
    }

    /// Compares two characters
    pub fn cmp(&self, x: &Char, y: &Char) -> Result<std::cmp::Ordering, BaseError> {
        let (ox, oy) = (self.ord(x)?, self.ord(y)?);
        Ok(ox.cmp(&oy))
    }

    pub(crate) fn wrap_describe(self: &Rc<Self>, call: impl FnOnce(u32, &Env) -> String, prec: u32, env: &Env) -> String {
        match **self {
            Alphabet::Listed{ref vec, ..} if !Rc::ptr_eq(self, &env.alpha)
                => format!("alpha({}, {})", Self::format(vec),
                    call(0, &Env{
                        alpha: Rc::clone(self),
                        vars: Rc::clone(&env.vars),
                        tracer: Rc::clone(&env.tracer)})),
            _ => call(prec, env)
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

impl TryFrom<Vec<Char>> for Alphabet {
    type Error = BaseError;

    fn try_from(src_vec: Vec<Char>) -> Result<Alphabet, BaseError> {
        let mut map = HashMap::new();
        if src_vec.is_empty() {
            return Err("alphabet is empty".into());
        }
        let mut res_vec = Vec::with_capacity(src_vec.len());
        for (ix, chr) in src_vec.into_iter().enumerate() {
            let ix = (ix + 1) as isize;
            let lcase = chr.to_lowercase();
            let ucase = chr.to_uppercase();
            let prev = if lcase == ucase {
                map.insert(chr.to_owned(), ix)
            } else {
                map.insert(lcase, ix).or(map.insert(ucase, ix))
            };
            if prev.is_some() {
                return Err(format!("duplicate character {chr}").into());
            }
            res_vec.push(chr);
        }
        Ok(Alphabet::Listed{vec: res_vec, map})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_std26() {
        let abc = Rc::new(Alphabet::Std26);
        assert_eq!(abc.ord(&Char::from('a')), Ok(1isize));
        assert_eq!(abc.ord(&Char::from('Z')), Ok(26isize));
        assert!(abc.ord(&Char::from('@')).is_err());
        assert!(abc.ord(&Char::from('á')).is_err());
        assert!(abc.ord(&Char::from("ch")).is_err());

        assert_eq!(abc.chr(&Number::from(1), CharCase::Lower), Char::from('a'));
        assert_eq!(abc.chr(&Number::from(0), CharCase::Indeterminate), Char::from('z'));
        assert_eq!(abc.chr(&Number::from(26), CharCase::Upper), Char::from('Z'));
        assert_eq!(abc.chr(&Number::from(100), CharCase::Lower), Char::from('v'));
        assert_eq!(abc.chr(&Number::from(-1), CharCase::Lower), Char::from('y'));
        assert_eq!(abc.chr(&Number::from(-100), CharCase::Lower), Char::from('d'));

        assert_eq!(abc.c_plus_c(&Char::from('C'), &Char::from('e')), Ok(Char::from('H')));
        assert_eq!(abc.c_plus_c(&Char::from('x'), &Char::from('Y')), Ok(Char::from('w')));
        assert!(abc.c_plus_c(&Char::from('a'), &Char::from('á')).is_err());

        let plus = crate::parser::parse("1+2").unwrap();
        assert_eq!(abc.wrap_describe(|prec, env| plus.describe_inner(prec, env), u32::MAX, &Default::default()), "(1+2)");
    }

    #[test]
    fn test_custom_alpha() {
        let abc = Rc::new(Alphabet::try_from(vec![Char::from('b'), Char::from('á'), Char::from("Ch"), Char::from('a')]).unwrap());
        assert_eq!(abc.ord(&Char::from('a')), Ok(4isize));
        assert_eq!(abc.ord(&Char::from('Á')), Ok(2isize));
        assert_eq!(abc.ord(&Char::from("CH")), Ok(3isize));
        assert_eq!(abc.ord(&Char::from("Ch")), Ok(3isize));
        assert!(abc.ord(&Char::from('c')).is_err());
        assert_eq!(abc.chr(&Number::from(3), CharCase::Lower), Char::from("ch"));
        assert_eq!(abc.chr(&Number::from(98), CharCase::Upper), Char::from('Á'));
        assert_eq!(abc.chr(&Number::from(99), CharCase::Upper), Char::from("CH"));
        assert_eq!(abc.chr(&Number::from(99), CharCase::Indeterminate), Char::from("Ch"));

        let abc = Alphabet::try_from(vec![Char::from('❤')]).unwrap();
        assert_eq!(abc.ord(&Char::from("❤")), Ok(1isize));

        assert!(Alphabet::try_from(vec![Char::from('a'), Char::from('A')]).is_err());
        assert!(Alphabet::try_from(vec![Char::from('İ'), Char::from("i\u{307}")]).is_err());
        assert!(Alphabet::try_from(vec![Char::from("ch"), Char::from("Ch")]).is_err());
        assert!(Alphabet::try_from(vec![]).is_err());

        let abc = Rc::new(Alphabet::try_from(vec![Char::from('b'), Char::from('á'), Char::from("Ch"), Char::from('a')]).unwrap());
        let plus = crate::parser::parse("1+2").unwrap();
        assert_eq!(abc.wrap_describe(|prec, env| plus.describe_inner(prec, env), u32::MAX, &Default::default()), "alpha(['b', 'á', 'Ch', 'a'], 1+2)");
    }
}
