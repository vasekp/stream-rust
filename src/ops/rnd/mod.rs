use crate::base::*;
use std::hash::{DefaultHasher, Hash, Hasher};
use rand::{Rng, SeedableRng, rngs::SmallRng};

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
            Ok(Item::new_stream(RndStream { source: stm.into(), head, seed, len, hasher }))
        },
        _ => Err(StreamError::new("expected: stream.rnd(seed)", node))
    }
}

#[derive(Clone)]
struct RndStream {
    source: BoxedStream,
    head: Head,
    seed: Number,
    len: UNumber,
    hasher: DefaultHasher
}

impl Describe for RndStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source),
            [ProxyItem::Number(&self.seed)],
            prec, env)
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
    source: &'node dyn Stream,
    len: &'node UNumber,
    hasher: &'node DefaultHasher,
    num_digits: usize,
    cutoff: UNumber,
    pos: UNumber,
}

impl<'node> RndIter<'node> {
    fn new(parent: &'node RndStream) -> Self {
        let digits = parent.len.iter_u64_digits().collect::<Vec<_>>();
        let last = digits.last().expect("digits should be nonempty");
        let max_quot = if last == &u64::MAX { 1 } else { u64::MAX / (last + 1) };
        RndIter {
            source: &*parent.source,
            len: &parent.len,
            hasher: &parent.hasher,
            num_digits: digits.len(),
            cutoff: max_quot * &parent.len,
            pos: UNumber::zero()
        }
    }
}

impl Iterator for RndIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut hasher = self.hasher.clone();
        Hash::hash(&self.pos, &mut hasher);
        let seed = hasher.finish();
        let mut rng = SmallRng::seed_from_u64(seed);
        let mut digits = Vec::with_capacity(self.num_digits);
        for _ in 0..self.num_digits {
            digits.push(rng.random());
        }
        let rnd = UNumber::from_slice(&digits[..]);
        if rnd >= self.cutoff {
            println!("drop");
        }
        let rem = rnd % self.len;
        let mut iter = self.source.iter();
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
        test_advance("range(10^10).rnd(1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("rnd", eval_rnd);
}
