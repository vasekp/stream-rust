mod reorder;
mod perm;
mod factorial;

mod util {
    use crate::base::*;

    pub(crate) fn factorial(x: u32) -> SResult<UNumber> {
        if x < 2 { Ok(UNumber::one()) } else { range_prod(2, x) }
    }

    fn range_prod(from: u32, to: u32) -> SResult<UNumber> {
        check_stop!();
        Ok(match to - from {
            0 => UNumber::from(from),
            1 => UNumber::from(from) * (from + 1),
            _ => {
                let mid = from + (to - from) / 2;
                range_prod(from, mid)? * range_prod(mid + 1, to)?
            },
        })
    }

    /// Returns `x!` if it does not exceed `cap`, `None` otherwise. Stops multiplying as soon as
    /// the cap is exceeded, so this is cheap for small `cap` even when `x` is huge.
    pub(crate) fn factorial_capped(x: u32, cap: &UNumber) -> SResult<Option<UNumber>> {
        let mut prod = UNumber::one();
        if &prod > cap {
            return Ok(None);
        }
        for i in 2..=x {
            check_stop!();
            prod *= i;
            if &prod > cap {
                return Ok(None);
            }
        }
        Ok(Some(prod))
    }

    #[cfg(test)]
    #[test]
    fn test_factorial() {
        let mut r = 1u64;
        assert_eq!(factorial(0).unwrap(), 1u32.into());
        for x in 1..20 {
            r *= x as u64;
            assert_eq!(factorial(x).unwrap(), r.into());
            assert_eq!(factorial_capped(x, &r.into()).unwrap(), Some(r.into()));
            assert_eq!(factorial_capped(x, &(r - 1).into()).unwrap(), None);
        }
        assert_eq!(factorial(100).unwrap(), "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000".parse().unwrap());
        assert_eq!(factorial_capped(u32::MAX, &1000u32.into()).unwrap(), None);
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    reorder::init(symbols);
    perm::init(symbols);
    factorial::init(symbols);
}
