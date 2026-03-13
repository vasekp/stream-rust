use crate::base::*;
use std::hash::{DefaultHasher, Hash, Hasher};
use rand::{Rng, SeedableRng, rngs::SmallRng};

fn eval_rnd(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    let [Item::Number(seed)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    let len = stm.try_count()?;
    if len.is_zero() {
        return Err("stream is empty".into());
    }
    let mut hasher = DefaultHasher::default();
    Hash::hash(seed, &mut hasher);
    Hash::hash(&b':', &mut hasher);
    let digits = len.to_be_bytes();
    let top_digit = digits[0];
    let max_quot = if digits.len() == 1 { u8::MAX / top_digit }
        else if top_digit == u8::MAX { 1 }
        else { u8::MAX / (top_digit + 1) };
    Ok(Item::new_stream(RndStream {
        source: stm,
        head: node.head.clone(),
        seed: seed.clone(),
        hasher,
        num_digits: digits.len(),
        cutoff: max_quot * &len,
        len
    }))
}

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
        let mut hasher = self.node.hasher.clone();
        Hash::hash(&self.pos, &mut hasher);
        let seed = hasher.finish();
        let mut rng = SmallRng::seed_from_u64(seed);
        let rnd = loop {
            let mut digits = Vec::with_capacity(self.node.num_digits);
            for _ in 0..self.node.num_digits {
                digits.push(rng.random());
            }
            let rnd = UNumber::from_be_bytes(&digits[..]);
            if rnd < self.node.cutoff {
                break rnd;
            } else {
                //eprintln!("drop");
            }
        };
        let rem = rnd % &self.node.len;
        let mut iter = self.node.source.iter();
        iter.advance(rem)?;
        self.pos += 1;
        iter.next()
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.pos += n;
        Ok(None)
    }

    fn origin(&self) -> &Rc<RndStream> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rnd() {
        use super::*;
        test_eval!("(1..6).rnd(0)" => "[4, 5, 1, 1, 2, ...]");
        test_eval!("(1..6).rnd(-1)" => "[1, 1, 3, 3, 2, ...]");
        test_eval!("[].rnd(1)" => err);
        test_eval!("\"abc\".rnd(1)" => err);
        test_eval!("seq.rnd(1)" => err);
        // test fairness of rejection sampling
        test_eval!("(range(2^64*3/4).rnd(0)/2^62).first(1000).counts(0,1,2)" => "[336, 337, 327]");
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
> (1..6).?(0) : 10 => [4, 5, 1, 1, 2, 6, 3, 6, 4, 5, ...] ; always the same for seed == 0
> ?seq.?(0) => !stream is infinite
> [].?(0) => !stream is empty
"#);
}
