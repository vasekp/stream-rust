use crate::base::*;

use std::collections::VecDeque;

fn eval_weave(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    node.check_args_nonempty()?;
    let streams = node.args.iter().map(Item::to_stream).collect::<SResult<Vec<_>>>()?;
    Ok(Item::new_stream(Weave { head: node.head.clone(), streams }))
}

struct Weave {
    head: Head,
    streams: Vec<Rc<dyn Stream>>,
}

impl Describe for Weave {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .push_args(&self.streams)
            .finish(prec)
    }
}

impl Stream for Weave {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iters = self.streams.iter().map(|stm| stm.iter()).collect();
        WeaveIter { iters, node: self }.wrap()
    }

    fn len(&self) -> Length {
        let num = self.streams.len();
        self.streams
            .iter()
            .enumerate()
            .map(|(ix, stm)| stm.len().map(|len| len * num + ix))
            .reduce(Length::intersection)
            .unwrap()
    }
}

struct WeaveIter {
    node: Rc<Weave>,
    iters: VecDeque<Box<dyn SIterator>>,
}

impl PreIterator for WeaveIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let res = iter_try!(
            self.iters
                .front_mut()
                .unwrap() // nonempty checked at eval
                .next()
        );
        self.iters.rotate_left(1);
        Ok(Some(res))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let (quot, mut rem) = n.div_rem(&self.iters.len());
        let num = self.iters.len();
        // If some stream ends during the advance, `avail` is the number of items the weave could
        // produce in total (full rounds, then one item from each stream before the ended one).
        let mut avail: Option<UNumber> = None;
        for (ix, iter) in self.iters.iter_mut().enumerate() {
            if let Some(r) = iter.advance(&quot)? {
                // `r` may be zero: the stream has ended exactly, but must not be read again.
                *iter = EmptyStream::iter();
                let this = (&quot - r) * num + ix;
                avail = Some(avail.map_or(this.clone(), |a| std::cmp::min(a, this)));
            }
        }
        if let Some(avail) = avail
            && &avail < n
        {
            return Ok(Some(n - avail));
        }
        while rem != 0 {
            if self.next()?.is_none() {
                return Ok(Some(rem.into()));
            }
            rem -= 1;
        }
        Ok(None)
    }

    fn origin(&self) -> &Rc<Weave> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["weave", "alternate"], eval_weave, r#"
Interleaves the values of given streams until one of them finishes.
= ?(stream, stream, ...)
> ?(?seq, ?alpha) => [1, 'a', 2, 'b', 3, ...]
: zip
: riffle
: cat
: unweave
"#);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_weave() {
        test_eval!("weave(seq, seq + 3)" => "[1, 4, 2, 5, 3, ...]");
        test_eval!("weave(seq, alpha, alpha:ucase)" : 10 => "[1, 'a', 'A', 2, 'b', 'B', 3, 'c', 'C', 4, ...]");
        test_eval!("weave(1..3, 'a'..'a')" => "[1, 'a', 2]");
        test_eval!("weave(1..3, 'a'..'b')" => "[1, 'a', 2, 'b', 3]");
        test_eval!("weave(1..3, 'a'..'c')" : 6 => "[1, 'a', 2, 'b', 3, 'c']");
        test_eval!("weave(1..3, 'a'..'c', 'A'..'C')" : 10 => "[1, 'a', 'A', 2, 'b', 'B', 3, 'c', 'C']");
        test_eval!("weave(1..3, 'a'..'c', 'A'..'B')" : 10 => "[1, 'a', 'A', 2, 'b', 'B', 3, 'c']");
        test_eval!("weave(1..3, 'a'..'b', 'A'..'C')" : 10 => "[1, 'a', 'A', 2, 'b', 'B', 3]");
        test_eval!("weave(1..2, 'a'..'c', 'A'..'C')" : 10 => "[1, 'a', 'A', 2, 'b', 'B']");
        test_eval!("weave([], seq)" => "[]");
        test_eval!("weave(seq, [])" => "[1]");
        test_len!("weave(1..5, 1..6, 1..7)" => 15);
        test_len!("weave(1..7, 1..6, 1..5)" => 17);
        test_advance("weave(1..5, 1..10)");
        // Inner streams whose advance() reports an exact end (Some(0))
        test_eval!("weave(seq, (1..10).unweave(2)).skip(4)" => "[3]");
        test_eval!("weave(seq, (1..10).unweave(2))[5]" => "3");
        test_eval!("weave(seq, (1..10).unweave(2)).skip(5)" => "[]");
        test_eval!("weave(seq, (1..10).unweave(2)).skip(6)" => "[]");
        test_eval!("weave((1..10).unweave(2), seq).skip(3)" => "[2]");
        test_eval!("weave((1..10).unweave(2), seq).skip(4)" => "[]");
        test_eval!("(weave(seq, (1..10).unweave(2))~[0]).skip(5)" => "[0]");
        test_eval!("(weave(seq, (1..10).unweave(2))~[0]).skip(6)" => "[]");
        test_advance("weave(seq, (1..10).unweave(2))");
        test_advance("weave((1..10).unweave(2), seq)");
        test_advance("weave((1..10).unweave(2), (1..10).unweave(3))");
        test_advance("weave(1..3, 1..5, 1..4)");
        test_advance("weave(1..5, 1..4, 1..3)");
        test_advance("weave(1..10, 1..5)");
        test_advance("weave(seq, 1..10^5)");
        test_advance("weave(1..10^5, seq)");
        test_advance("weave(seq, seq)");
        test_advance("weave([0], 1..10)");
        test_advance("weave(1..10, [0])");
        test_advance("weave(1..1000, 1..100, 1..10)");
    }
}
