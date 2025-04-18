use crate::base::*;

#[cfg(test)]
#[track_caller]
pub(crate) fn test_len_exact(item: &Item, len: usize) {
    let stm = item.as_stream().unwrap();
    assert_eq!(stm.iter().map(Result::unwrap).count(), len);
    assert!(Length::possibly_eq(&stm.length(), &Length::Exact(len.into())));
    assert!(Length::possibly_eq(&stm.iter().len_remain(), &Length::Exact(len.into())));
    assert_eq!(len == 0, stm.is_empty());
}

#[cfg(test)]
#[track_caller]
pub(crate) fn test_skip_n(item: &Item) {
    let stm = item.as_stream().unwrap();
    const TEST: u32 = 5;

    assert_eq!(stm.iter().len_remain(), stm.length());

    // skip(0) = no-op on fresh iterator
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.next(), match i2.skip_n(UNumber::zero()).unwrap() {
        Some(_) => None,
        None => i2.next() // same None, same element or same error
    });

    if !stm.is_empty() {
        assert_ne!(stm.iter().next().transpose().unwrap(), None); // fails if the first item fails

        // skip(1) = next() on fresh iterator
        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        i1.next();
        assert_eq!(i1.next(), match i2.skip_n(UNumber::one()).unwrap() {
            Some(_) => None,
            None => i2.next()
        });

        assert_eq!(stm.iter().len_remain(), stm.length());

        match stm.length() {
            Length::Exact(len) => {
                assert!(!len.is_zero());

                // skip(len - 1) leaves exactly one element
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&len - 1u32).unwrap(), None);
                assert_eq!(it.len_remain(), Length::Exact(UNumber::one()));
                assert_ne!(it.next().transpose().unwrap(), None);
                assert_eq!(it.next(), None);

                // skip(len) leaves nothing
                let mut it = stm.iter();
                match it.skip_n(len.clone()).unwrap() {
                    None => {
                        assert_eq!(it.len_remain(), Length::Exact(UNumber::zero()));
                        assert_eq!(it.next(), None);
                    },
                    Some(rem) => assert_eq!(rem, UNumber::zero())
                }

                // skip(len + N) reports N remaining elements
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&len + 1u32).unwrap(), Some(UNumber::one()));
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&len + 100u32).unwrap(), Some(100u32.into()));

                // skip() following skip()
                let mut half = (&len - 1u32) / 2u32;
                let mut rest = &len - 1u32 - &half;
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(half.clone()).unwrap(), None);
                assert_eq!(i1.len_remain(), Length::Exact(&len - &half));
                assert_eq!(i1.skip_n(rest.clone()).unwrap(), None);
                assert_eq!(i1.len_remain(), Length::Exact(UNumber::one()));
                assert_eq!(i2.skip_n(&len - 1u32).unwrap(), None);
                assert_eq!(i2.len_remain(), Length::Exact(UNumber::one()));
                assert_eq!(i1.next(), i2.next());

                // skip(0) = no-op later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(half.clone()).unwrap(), None);
                assert_eq!(i1.skip_n(UNumber::zero()).unwrap(), None);
                assert_eq!(i2.skip_n(half.clone()).unwrap(), None);
                assert_eq!(i1.len_remain(), i2.len_remain());
                assert_eq!(i1.next(), i2.next());

                // skip(1) = next() later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(half.clone()).unwrap(), None);
                assert_eq!(i1.skip_n(UNumber::one()).unwrap(), None);
                assert_eq!(i2.skip_n(half.clone()).unwrap(), None);
                assert_ne!(i2.next().transpose().unwrap(), None);
                assert_eq!(i1.len_remain(), i2.len_remain());
                assert_eq!(i1.next(), i2.next());

                // skip() from within past end
                half.inc();
                rest.inc(); // now half + rest = len + 1
                let mut it = stm.iter();
                assert_eq!(it.skip_n(half.clone()).unwrap(), None);
                assert_eq!(it.skip_n(rest.clone()).unwrap(), Some(UNumber::one()));

                // test actually comparing a few elements after a small skip
                if len > (2 * TEST).into() {
                    let (mut i1, mut i2) = (stm.iter(), stm.iter());
                    assert_eq!(i1.skip_n(TEST.into()).unwrap(), None);
                    for _ in 0..TEST {
                        i2.next().unwrap().unwrap();
                    }
                    for _ in 1..TEST {
                        assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
                    }
                }
            },
            Length::Infinite => {
                let many = 10000000000u64;

                // skip() following skip()
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.len_remain(), Length::Infinite);
                assert_eq!(i1.skip_n(UNumber::from(many)).unwrap(), None);
                assert_eq!(i2.skip_n(UNumber::from(many * 2)).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // skip(0) = no-op later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.skip_n(UNumber::zero()).unwrap(), None);
                assert_eq!(i2.skip_n(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // skip(1) = next() later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(UNumber::from(many)).unwrap(), None);
                assert_eq!(i1.skip_n(UNumber::one()).unwrap(), None);
                assert_eq!(i2.skip_n(UNumber::from(many)).unwrap(), None);
                assert_ne!(i2.next().transpose().unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // test actually comparing a few elements after a small skip
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(TEST.into()).unwrap(), None);
                for _ in 0..TEST {
                    i2.next().unwrap().unwrap();
                }
                for _ in 0..TEST {
                    assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
                }
            },
            _ => ()
        }
    } else {
        assert_eq!(stm.iter().len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(stm.iter().next(), None);
        assert_eq!(stm.iter().skip_n(UNumber::one()).unwrap(), Some(UNumber::one()));
    }
}
