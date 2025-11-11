use crate::base::*;
use crate::parser::parse;

use std::fmt::Debug;

macro_rules! test_eval {
    ($input:literal => err) => { assert!(parse($input).unwrap().eval_default().is_err()); };
    ($input:literal => $output:literal) => { assert_eq!(eval!($input).to_string(), $output); };
    ($input:literal : $N:expr => $output:expr) => { assert_eq!(format!("{:1$}", eval!($input), $N), $output); };
}

macro_rules! test_describe {
    ($input:literal => $output:literal) => { assert_eq!(eval!($input).describe(), $output); };
}

macro_rules! test_len {
    ($input:literal => $len:literal) => { test_len_exact($input, $len); };
    ($input:literal => $len:expr) => {
        let item = eval!($input);
        match &item {
            Item::Stream(stm) => assert_eq!(stm.len(), $len),
            Item::String(stm) => assert_eq!(stm.len(), $len),
            _ => panic!("test_len: expected stream or string, found {:?}", item)
        }
    }
}

pub(crate) use test_eval;
pub(crate) use test_describe;
pub(crate) use test_len;

#[cfg(test)]
#[track_caller]
pub(crate) fn test_len_exact(input: &str, len: usize) {
    match &eval!(input) {
        Item::Stream(stm) => test_len_exact_impl(&**stm, len),
        Item::String(stm) => test_len_exact_impl(&**stm, len),
        item => panic!("test_len_exact: expected stream or string, found {:?}", item)
    }
}

fn test_len_exact_impl<I>(stm: &dyn Stream<I>, len: usize) {
    assert_eq!(stm.iter().map(Result::unwrap).count(), len);
    assert!(Length::possibly_eq(&stm.len(), &Length::Exact(len.into())));
    assert!(Length::possibly_eq(&stm.iter().len_remain(), &Length::Exact(len.into())));
    assert_eq!(len == 0, stm.is_empty());
}

#[cfg(test)]
#[track_caller]
pub(crate) fn test_advance(input: &str) {
    match &eval!(input) {
        Item::Stream(stm) => test_advance_impl(&**stm),
        Item::String(stm) => test_advance_impl(&**stm),
        item => panic!("test_advance: expected stream or string, found {:?}", item)
    }
}

const TEST: u32 = 5;

#[cfg(test)]
#[track_caller]
fn test_advance_impl<I: PartialEq + Debug>(stm: &dyn Stream<I>) {
    assert_eq!(stm.iter().len_remain(), stm.len());

    // advance(0) = no-op on fresh iterator
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.next(), match i2.advance(UNumber::zero()).unwrap() {
        Some(_) => None,
        None => i2.next() // same None, same element or same error
    });

    if !stm.is_empty() {
        assert_ne!(stm.iter().next().transpose().unwrap(), None); // fails if the first item fails

        // advance(1) = next() on fresh iterator
        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        i1.next();
        assert_eq!(i1.next(), match i2.advance(UNumber::one()).unwrap() {
            Some(_) => None,
            None => i2.next()
        });

        assert_eq!(stm.iter().len_remain(), stm.len());

        match stm.len() {
            Length::Exact(len) => test_advance_exact_impl(stm, len, true),
            Length::Infinite => {
                let many = 10000000000u64;

                // advance() following advance()
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.len_remain(), Length::Infinite);
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None);
                assert_eq!(i2.advance(UNumber::from(many * 2)).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // advance(0) = no-op later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.advance(UNumber::zero()).unwrap(), None);
                assert_eq!(i2.advance(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // advance(1) = next() later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.advance(UNumber::one()).unwrap(), None);
                assert_eq!(i2.advance(UNumber::from(many)).unwrap(), None);
                assert_ne!(i2.next().transpose().unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // test actually comparing a few elements after a small skip
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(TEST.into()).unwrap(), None);
                for _ in 0..TEST {
                    i2.next().unwrap().unwrap();
                }
                for _ in 0..TEST {
                    assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
                }
            },
            _ => {
                let mut iter = stm.iter();
                if let Some(rem) = iter.advance(UNumber::from(TEST + 1)).unwrap() {
                    assert!(rem.try_into().unwrap() <= TEST + 1);
                    let len_real = TEST + 1 - rem.try_into().unwrap();
                    test_advance_exact_impl(stm, UNumber::from(len_real), false);
                    return;
                }

                let mut iter = stm.iter();
                for _ in 0..TEST {
                    assert!(matches!(iter.next(), Some(Ok(_))));
                }
                let next = iter.next().unwrap();

                let mut iter = stm.iter();
                assert_eq!(iter.advance(TEST.into()), Ok(None));
                assert_eq!(iter.next().unwrap(), next);

                const HALF: u32 = TEST / 2;
                const REST: u32 = TEST - HALF;
                let mut iter = stm.iter();
                assert_eq!(iter.advance(HALF.into()), Ok(None));
                assert_eq!(iter.advance(UNumber::zero()), Ok(None));
                assert_eq!(iter.advance(REST.into()), Ok(None));
                assert_eq!(iter.next().unwrap(), next);

                const { assert!(REST > 1); }
                let mut iter = stm.iter();
                assert_eq!(iter.advance(HALF.into()), Ok(None));
                assert!(matches!(iter.next(), Some(Ok(_))));
                assert_eq!(iter.advance((REST - 1).into()), Ok(None));
                assert_eq!(iter.next().unwrap(), next);

                let mut iter = stm.iter();
                const { assert!(TEST >= 2); }
                assert!(matches!(iter.next(), Some(Ok(_))));
                assert_eq!(iter.advance((TEST - 2).into()), Ok(None));
                assert!(matches!(iter.next(), Some(Ok(_))));
                assert_eq!(iter.next().unwrap(), next);

                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(TEST.into()), Ok(None));
                assert_eq!(i1.advance(TEST.into()), i2.advance((2*TEST).into()));
            }
        }
    } else {
        assert_eq!(stm.iter().len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(stm.iter().next(), None);
        assert_eq!(stm.iter().advance(UNumber::one()).unwrap(), Some(UNumber::one()));
    }
}

#[cfg(test)]
#[track_caller]
fn test_advance_exact_impl<I: PartialEq + Debug>(stm: &dyn Stream<I>, len: UNumber, test_len_remain: bool) {
    if len.is_zero() {
        return;
    }

    // advance(len - 1) leaves exactly one element
    let mut it = stm.iter();
    assert_eq!(it.advance(&len - 1u32).unwrap(), None);
    if test_len_remain {
        assert_eq!(it.len_remain(), Length::Exact(UNumber::one()));
    }
    assert_ne!(it.next().transpose().unwrap(), None);
    assert_eq!(it.next(), None);

    // advance(len) leaves nothing
    let mut it = stm.iter();
    match it.advance(len.clone()).unwrap() {
        None => {
            if test_len_remain {
                assert_eq!(it.len_remain(), Length::Exact(UNumber::zero()));
            }
            assert_eq!(it.next(), None);
        },
        Some(rem) => assert_eq!(rem, UNumber::zero())
    }

    // advance(len + N) reports N remaining elements
    let mut it = stm.iter();
    assert_eq!(it.advance(&len + 1u32).unwrap(), Some(UNumber::one()));
    let mut it = stm.iter();
    assert_eq!(it.advance(&len + 100u32).unwrap(), Some(100u32.into()));

    // advance() following advance()
    let mut half = (&len - 1u32) / 2u32;
    let mut rest = &len - 1u32 - &half;
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None);
    if test_len_remain {
        assert_eq!(i1.len_remain(), Length::Exact(&len - &half));
    }
    assert_eq!(i1.advance(rest.clone()).unwrap(), None);
    if test_len_remain {
        assert_eq!(i1.len_remain(), Length::Exact(UNumber::one()));
    }
    assert_eq!(i2.advance(&len - 1u32).unwrap(), None);
    if test_len_remain {
        assert_eq!(i2.len_remain(), Length::Exact(UNumber::one()));
    }
    assert_eq!(i1.next(), i2.next());

    // advance(0) = no-op later in stream
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None);
    assert_eq!(i1.advance(UNumber::zero()).unwrap(), None);
    assert_eq!(i2.advance(half.clone()).unwrap(), None);
    if test_len_remain {
        assert_eq!(i1.len_remain(), i2.len_remain());
    }
    assert_eq!(i1.next(), i2.next());

    // advance(1) = next() later in stream
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None);
    assert_eq!(i1.advance(UNumber::one()).unwrap(), None);
    assert_eq!(i2.advance(half.clone()).unwrap(), None);
    assert_ne!(i2.next().transpose().unwrap(), None);
    if test_len_remain {
        assert_eq!(i1.len_remain(), i2.len_remain());
    }
    assert_eq!(i1.next(), i2.next());

    // advance() from within past end
    half.inc();
    rest.inc(); // now half + rest = len + 1
    let mut it = stm.iter();
    assert_eq!(it.advance(half.clone()).unwrap(), None);
    assert_eq!(it.advance(rest.clone()).unwrap(), Some(UNumber::one()));

    // test actually comparing a few elements after a small skip
    if len > (2 * TEST).into() {
        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        assert_eq!(i1.advance(TEST.into()).unwrap(), None);
        for _ in 0..TEST {
            i2.next().unwrap().unwrap();
        }
        for _ in 1..TEST {
            assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
        }
    }
}
