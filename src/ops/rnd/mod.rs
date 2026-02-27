use crate::base::*;
use std::hash::{DefaultHasher, Hash, Hasher};
use rand::{RngCore, SeedableRng, rngs::SmallRng};

fn eval_rnd(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve_source()?;
    match node {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Item::Number(seed)), head } => {
            let len = stm.try_count()?;
            if len.is_zero() {
                return Err(StreamError::new("stream is empty", Item::Stream(stm)));
            }
            let mut hasher = DefaultHasher::default();
            Hash::hash(&seed, &mut hasher);
            Hash::hash(&b':', &mut hasher);
            let digits = len.iter_u32_digits().collect::<Vec<_>>();
            let top_digit = digits.last().expect("digits should be nonempty");
            let max_quot = if digits.len() == 1 { u32::MAX / top_digit }
                else if top_digit == &u32::MAX { 1 }
                else { u32::MAX / (top_digit + 1) };
            Ok(Item::new_stream(RndStream {
                source: stm,
                head, seed, hasher,
                num_digits: digits.len(),
                cutoff: max_quot * &len,
                len
            }))
        },
        _ => Err(StreamError::new("expected: stream.rnd(seed)", node))
    }
}

#[derive(Clone)]
struct RndStream {
    source: Rc<dyn Stream>,
    head: Head,
    seed: Number,
    hasher: DefaultHasher,
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
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(RndIter::new(self))
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

struct RndIter<'node> {
    parent: &'node RndStream,
    pos: UNumber,
}

impl<'node> RndIter<'node> {
    fn new(parent: &'node RndStream) -> Self {
        RndIter { parent, pos: UNumber::zero() }
    }
}

impl Iterator for RndIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut hasher = self.parent.hasher.clone();
        Hash::hash(&self.pos, &mut hasher);
        let seed = hasher.finish();
        let mut rng = SmallRng::seed_from_u64(seed);
        let rnd = loop {
            let mut digits = Vec::with_capacity(self.parent.num_digits);
            for _ in 0..self.parent.num_digits {
                digits.push(rng.next_u32());
            }
            let rnd = UNumber::from_slice(&digits[..]);
            if rnd < self.parent.cutoff {
                break rnd;
            } else {
                //eprintln!("drop");
            }
        };
        let rem = rnd % &self.parent.len;
        let mut iter = self.parent.source.iter();
        match iter.advance(rem) {
            Ok(None) => (),
            Ok(Some(_)) => unreachable!("iterator ended before its length"),
            Err(err) => return Some(Err(err))
        };
        self.pos.inc();
        iter.next()
    }
}

impl SIterator for RndIter<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.pos += n;
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rnd() {
        use super::*;
        test_eval!("(1..6).rnd(0)" => "[3, 5, 4, 4, 2, ...]");
        test_eval!("(1..6).rnd(-1)" => "[6, 6, 1, 2, 4, ...]");
        test_eval!("[].rnd(1)" => err);
        test_eval!("\"abc\".rnd(1)" => err);
        test_eval!("seq.rnd(1)" => err);
        // test fairness of rejection sampling
        test_eval!("(range(2^64*3/4).rnd(0)/2^62).first(1000).counts(0,1,2)" => "[324, 336, 340]");
        test_advance("range(10^10).rnd(1)");
        test_describe!("(1..6).rnd(0)" => "(1..6).rnd(0)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("rnd", eval_rnd, r#"
An infinite stream of uniformly random samples from the input `stream`.
This is designed to be a reproducible pseudorandom generator. For this reason, a `seed` (number) needs to be provided. The same seed leads to the same pseudorandom stream.
* For a different seed in each invocation, you may use `$#`, which increases by one in each successfully evaluated input.
= stream.?(seed)
> (1..6).?(0) : 10 => [3, 5, 4, 4, 2, 2, 2, 5, 4, 1, ...] ; always the same for seed == 0
> ?seq.?(0) => !stream is infinite
> [].?(0) => !stream is empty
"#);
}
