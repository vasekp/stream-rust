use crate::base::*;

use std::fmt::Debug;

macro_rules! test_eval {
    ($input:literal => err) => { assert!(crate::parser::parse($input).unwrap().eval_default().is_err()); };
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
    assert_eq!(stm.iter().len_remain(), stm.len(), "len_remain on fresh iterator == len");

    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.next(), match i2.advance(UNumber::zero()).unwrap() {
        Some(_) => None,
        None => i2.next() // same None, same element or same error
    }, "advance(0) = no-op on fresh iterator");

    if !stm.is_empty() {
        assert_ne!(stm.iter().next().transpose().unwrap(), None,
            "first next() if !is_empty()"); // panics if the first item is Err

        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        i1.next();
        assert_eq!(i1.next(), match i2.advance(UNumber::one()).unwrap() {
            Some(_) => None,
            None => i2.next()
        }, "advance(1) = next() on fresh iterator");

        match stm.len() {
            Length::Exact(len) => test_advance_exact_impl(stm, len, true),
            Length::Infinite => {
                let many = 10000000000u64;

                // advance() following advance()
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many)");
                assert_eq!(i1.len_remain(), Length::Infinite,
                    "len_remain after reads for infinite");
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many) after advance(many)");
                assert_eq!(i2.advance(UNumber::from(many * 2)).unwrap(), None,
                    "advance(2*many)");
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap(),
                    "advance(many) + advance(many) = advance(2*many)");

                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many)");
                assert_eq!(i1.advance(UNumber::zero()).unwrap(), None,
                    "advance(0)");
                assert_eq!(i2.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many)");
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap(),
                    "advance(0) = no-op later in stream");

                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many)");
                assert_eq!(i1.advance(UNumber::one()).unwrap(), None,
                    "advance(1)");
                assert_eq!(i2.advance(UNumber::from(many)).unwrap(), None,
                    "advance(many)");
                assert_ne!(i2.next().transpose().unwrap(), None,
                    "advance(many) + next() for infinite");
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap(),
                    "advance(many) + advance(1) = advance(many) + next()");

                // test actually comparing a few elements after a small skip
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(TEST.into()).unwrap(), None,
                    "advance({TEST})");
                for _ in 0..TEST {
                    i2.next().unwrap().unwrap();
                }
                for i in 0..TEST {
                    assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap(),
                        "{i}-th next() after advance({TEST})");
                }
            },
            _ => {
                let mut iter = stm.iter();
                if let Some(rem) = iter.advance(UNumber::from(TEST + 1)).unwrap() {
                    assert!(rem.to_u32().unwrap() <= TEST + 1,
                        "remain after advance(n) <= n");
                    let len_real = TEST + 1 - rem.to_u32().unwrap();
                    test_advance_exact_impl(stm, UNumber::from(len_real), false);
                    return;
                }

                let mut iter = stm.iter();
                for i in 0..TEST {
                    assert!(matches!(iter.next(), Some(Ok(_))),
                        "{i}=th next() if advance({TEST}) returned None");
                }
                let next = iter.next().unwrap();

                let mut iter = stm.iter();
                assert_eq!(iter.advance(TEST.into()), Ok(None), "advance({TEST})");
                assert_eq!(iter.next().unwrap(), next, "advance({TEST} + next()");

                const HALF: u32 = TEST / 2;
                const REST: u32 = TEST - HALF;
                let mut iter = stm.iter();
                assert_eq!(iter.advance(HALF.into()), Ok(None), "advance({TEST}/2)");
                assert_eq!(iter.advance(UNumber::zero()), Ok(None), "advance(0)");
                assert_eq!(iter.advance(REST.into()), Ok(None), "advance({TEST}-{TEST}/2)");
                assert_eq!(iter.next().unwrap(), next,
                    "advance({TEST}/2) + advance(0) + advance(rest) + next()");

                const { assert!(REST > 1); }
                let mut iter = stm.iter();
                assert_eq!(iter.advance(HALF.into()), Ok(None), "advance({TEST}/2)");
                assert!(matches!(iter.next(), Some(Ok(_))), "advance({TEST}/2) + next()");
                assert_eq!(iter.advance((REST - 1).into()), Ok(None), "advance(rest)");
                assert_eq!(iter.next().unwrap(), next,
                    "advance({TEST}/2) + next() + advance(rest) + next()");

                let mut iter = stm.iter();
                const { assert!(TEST >= 2); }
                assert!(matches!(iter.next(), Some(Ok(_))),
                    "next() on fresh nonempty at-most iterator");
                assert_eq!(iter.advance((TEST - 2).into()), Ok(None), "advance({TEST} - 2");
                assert!(matches!(iter.next(), Some(Ok(_))),
                    "next() after next() + advance({TEST} - 2)");
                assert_eq!(iter.next().unwrap(), next,
                    "next() + advance({TEST} - 2) + next() + next()");

                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.advance(TEST.into()), Ok(None), "advance({TEST})");
                assert_eq!(i1.advance(TEST.into()), i2.advance((2*TEST).into()),
                    "advance({TEST}) + advance({TEST}) = advance(2*{TEST})");
            }
        }
    } else {
        assert_eq!(stm.iter().len_remain(), Length::Exact(UNumber::zero()),
            "len_remain = 0 for is_empty()");
        assert_eq!(stm.iter().next(), None, "next = None for is_empty()");
        assert_eq!(stm.iter().advance(UNumber::one()).unwrap(), Some(UNumber::one()),
            "advance(1) on empty stream");
    }
}

#[cfg(test)]
#[track_caller]
fn test_advance_exact_impl<I: PartialEq + Debug>(stm: &dyn Stream<I>, len: UNumber, test_len_remain: bool) {
    if len.is_zero() {
        assert_eq!(stm.iter().advance(100u32.into()).unwrap(), Some(100u32.into()),
            "advance() on an empty iter");
        return;
    }

    // advance(len) leaves nothing
    let mut it = stm.iter();
    match it.advance(len.clone()).unwrap() {
        None => {
            if test_len_remain {
                assert_eq!(it.len_remain(), Length::Exact(UNumber::zero()),
                    "len_remain() after advance(len)");
            }
            assert_eq!(it.next(), None, "next() after advance(len)");
        },
        Some(rem) => assert_eq!(rem, UNumber::zero(), "advance(len)")
    }

    // advance(len + N) reports N remaining elements
    let mut it = stm.iter();
    assert_eq!(it.advance(&len + 1u32).unwrap(), Some(UNumber::one()), "advance(len+x)");
    let mut it = stm.iter();
    assert_eq!(it.advance(&len + 100u32).unwrap(), Some(100u32.into()), "advance(len+x)");

    // advance() following advance()
    let mut half = (&len - 1u32) / 2u32;
    let mut rest = &len - 1u32 - &half;
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None, "advance(len/2)");
    if test_len_remain {
        assert_eq!(i1.len_remain(), Length::Exact(&len - &half),
            "len_remain() after advance(len/2)");
    }
    assert_eq!(i1.advance(rest.clone()).unwrap(), None, "composed advance(len-1)");
    if test_len_remain {
        assert_eq!(i1.len_remain(), Length::Exact(UNumber::one()),
            "len_remain() after composed advance(len-1)");
    }
    assert_eq!(i2.advance(&len - 1u32).unwrap(), None, "advance(len-1)");
    if test_len_remain {
        assert_eq!(i2.len_remain(), Length::Exact(UNumber::one()),
            "len_remain() after advance(len-1)");
    }
    let n1 = i1.next().transpose().unwrap();
    let n2 = i2.next().transpose().unwrap();
    assert!(n1.is_some(), "next() after advance(len-1)");
    assert_eq!(n1, n2, "next() after advance(len-1) direct vs. composed");
    assert_eq!(i1.next(), None, "advance(len-1) + next() + next()");

    // advance(0) = no-op
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(UNumber::zero()).unwrap(), None, "advance(0)");
    if test_len_remain {
        assert_eq!(i1.len_remain(), i2.len_remain(), "len_remain() after advance(0)");
    }
    assert_eq!(i1.next(), i2.next(), "next() after advance(0)");

    // advance(0) = no-op later in stream
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None, "advance(len/2)");
    assert_eq!(i1.advance(UNumber::zero()).unwrap(), None, "advance(0) after advance(len/2)");
    assert_eq!(i2.advance(half.clone()).unwrap(), None, "advance(len/2)");
    if test_len_remain {
        assert_eq!(i1.len_remain(), i2.len_remain(),
            "len_remain() after advance(len/2) direct vs. after advance(0)");
    }
    assert_eq!(i1.next(), i2.next(), "advance(len/2) direct vs. after advance(0)");

    // advance(1) = next() later in stream
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.advance(half.clone()).unwrap(), None, "advance(len/2)");
    assert_eq!(i1.advance(UNumber::one()).unwrap(), None, "advance(len/2) + advance(1)");
    assert_eq!(i2.advance(half.clone()).unwrap(), None, "advance(len/2)");
    assert_ne!(i2.next().transpose().unwrap(), None, "advance(len/2) + next()");
    if test_len_remain {
        assert_eq!(i1.len_remain(), i2.len_remain(),
            "len_remain() after advance(len/2) + [advance(1) | next()]");
    }
    assert_eq!(i1.next(), i2.next(), "next() after advance(len/2) + [advance(1) | next()]");

    // advance() from within past end
    half.inc();
    rest.inc(); // now half + rest = len + 1
    let mut it = stm.iter();
    assert_eq!(it.advance(half.clone()).unwrap(), None, "advance(len/2)");
    assert_eq!(it.advance(rest.clone()).unwrap(), Some(UNumber::one()),
        "advance(len/2) + advance(rest + 1)");

    // test actually comparing a few elements after a small skip
    if len > (2 * TEST).into() {
        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        assert_eq!(i1.advance(TEST.into()).unwrap(), None, "advance({TEST})");
        for _ in 0..TEST {
            i2.next().unwrap().unwrap();
        }
        for i in 1..TEST {
            assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap(),
                "{i}-th next() after advance({TEST})");
        }
    }
}
