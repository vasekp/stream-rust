use crate::base::*;
use std::hash::Hasher;
use siphasher::sip::SipHasher13;
use rand_xoshiro::Xoshiro256PlusPlus;
use rand_xoshiro::rand_core::{Rng, SeedableRng};

// The sequence for a given seed must not depend on the platform (the native and the WebAssembly
// builds in particular) or on the toolchain version. This is why the hash function and the
// generator are named explicitly: `std::hash::DefaultHasher` makes no stability guarantee and
// `rand::rngs::SmallRng` picks a different algorithm on 32-bit targets. Also, numbers must not be
// hashed through their `Hash` implementation, which goes through platform-sized words.

/// Feeds a number into the hasher in a platform-independent form: a sign byte, the length of the
/// magnitude in bytes, and the magnitude itself, little-endian.
fn write_number(hasher: &mut SipHasher13, negative: bool, magnitude: &UNumber) {
    let bytes = magnitude.to_le_bytes();
    hasher.write(&[negative as u8]);
    hasher.write(&(bytes.len() as u64).to_le_bytes());
    hasher.write(&bytes);
}

fn eval_rnd(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    let seed = node.only_arg_checked()?.as_num()?;
    let len = stm.try_count()?;
    if len.is_zero() {
        return Err("stream is empty".into());
    }
    let mut hasher = SipHasher13::new();
    write_number(&mut hasher, seed.is_negative(), &seed.unsigned_abs());
    let digits = len.to_be_bytes();
    let top_digit = digits[0];
    let max_quot = if digits.len() == 1 {
        u8::MAX / top_digit
    } else if top_digit == u8::MAX {
        1
    } else {
        u8::MAX / (top_digit + 1)
    };
    Ok(Item::new_stream(RndStream {
        source: stm,
        head: node.head.clone(),
        seed: seed.clone(),
        hasher,
        num_digits: digits.len(),
        cutoff: max_quot * &len,
        len,
    }))
}

struct RndStream {
    source: Rc<dyn Stream>,
    head: Head,
    seed: Number,
    /// The hasher with the seed fed in, so that each sample only needs to add its position.
    hasher: SipHasher13,
    len: UNumber,
    num_digits: usize,
    cutoff: UNumber,
}

impl Describe for RndStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.seed)
            .finish(prec)
    }
}

impl Stream for RndStream {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        RndIter::new(self).wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

struct RndIter {
    node: Rc<RndStream>,
    pos: UNumber,
}

impl RndIter {
    fn new(node: Rc<RndStream>) -> Self {
        RndIter { node, pos: UNumber::zero() }
    }
}

impl PreIterator for RndIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let mut hasher = self.node.hasher;
        write_number(&mut hasher, false, &self.pos);
        let mut rng = Xoshiro256PlusPlus::seed_from_u64(hasher.finish());
        let mut digits = vec![0u8; self.node.num_digits];
        let rnd = loop {
            rng.fill_bytes(&mut digits);
            let rnd = UNumber::from_be_bytes(&digits);
            if rnd < self.node.cutoff {
                break rnd;
            }
        };
        let rem = rnd % &self.node.len;
        let mut iter = self.node.source.iter();
        iter.advance(&rem)?;
        self.pos += 1;
        iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.pos += n;
        Ok(None)
    }

    fn origin(&self) -> &Rc<RndStream> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("rnd", eval_rnd, r#"
An infinite stream of uniformly random samples from the input `stream`.
The generator is deterministic: a `seed` (any integer) must be given, and the same seed always yields the same stream.
* For a different seed in each invocation, you may use `$#`, which increases by one in each successfully evaluated input.
* Every sample is read from `stream` anew.
= stream.?(seed)
> (1..6).?(0) : 10 => [5, 3, 1, 1, 3, 3, 6, 1, 3, 4, ...] ; always the same for seed == 0
> ?seq.?(0) => !stream is infinite
> [].?(0) => !stream is empty
"#);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rnd() {
        use super::*;
        test_eval!("(1..6).rnd(0)" => "[5, 3, 1, 1, 3, ...]");
        test_eval!("(1..6).rnd(-1)" => "[1, 4, 6, 3, 3, ...]");
        // The values are pinned: they must be the same on every platform and toolchain.
        test_eval!("(1..6).rnd(2^100)" => "[5, 1, 4, 6, 6, ...]");
        test_eval!("(1..6).rnd(-2^100)" => "[3, 3, 4, 2, 5, ...]");
        test_eval!("(1..6).rnd(0).skip(2^70).first(3)" => "[4, 6, 4]");
        test_eval!("range(10^30).rnd(1).first(1)" => "[46998348545369961395328856068]");
        test_eval!("[].rnd(1)" => err);
        test_eval!("\"abc\".rnd(1)" => err);
        test_eval!("\"abc\".chars.rnd(1)" => "['c', 'c', 'b', 'a', 'a', ...]");
        test_eval!("seq.rnd(1)" => err);
        // test fairness of rejection sampling
        test_eval!("(range(2^64*3/4).rnd(0)/2^62).first(1000).counts(0,1,2)" => "[319, 354, 327]");
        test_advance("range(10^10).rnd(1)");
        test_describe!("(1..6).rnd(0)" => "(1..6).rnd(0)");
    }
}
