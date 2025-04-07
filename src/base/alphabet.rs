use crate::base::*;
use num::traits::Euclid;

/// The notion of alphabet ordering in Stream.
pub enum Alphabet {
    /// Plain English 26-letter alphabet.
    Std26,
    /// An alphabet ordering created by explicitly listing the letters in order.
    _Listed(Vec<Char>)
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
            _ => todo!()
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
            _ => todo!()
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
            _ => todo!()
        }
    }

    #[cfg(test)]
    fn c_plus_c(&self, lhs: &Char, rhs: &Char) -> Result<Char, BaseError> {
        let (index1, case) = self.ord_case(lhs)?;
        let (index2, _) = self.ord_case(rhs)?;
        Ok(self.chr_case(&Number::from(index1 + index2), case))
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
        assert!(abc.ord_case(&Char::Multi("ch".into())).is_err());

        assert_eq!(abc.chr_case(&Number::from(1), CharCase::Lower), Char::from('a'));
        assert_eq!(abc.chr_case(&Number::from(0), CharCase::Undefined), Char::from('z'));
        assert_eq!(abc.chr_case(&Number::from(26), CharCase::Upper), Char::from('Z'));
        assert_eq!(abc.chr_case(&Number::from(100), CharCase::Lower), Char::from('v'));
        assert_eq!(abc.chr_case(&Number::from(-1), CharCase::Lower), Char::from('y'));
        assert_eq!(abc.chr_case(&Number::from(-100), CharCase::Lower), Char::from('d'));

        assert_eq!(abc.c_plus_c(&Char::from('C'), &Char::from('e')), Ok(Char::from('H')));
        assert_eq!(abc.c_plus_c(&Char::from('x'), &Char::from('Y')), Ok(Char::from('w')));
        assert!(abc.c_plus_c(&Char::from('a'), &Char::from('á')).is_err());
    }
}
