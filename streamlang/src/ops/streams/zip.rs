use crate::base::*;

fn eval_zip(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    let streams = node.source.iter().chain(&node.args)
        .map(Item::to_stream)
        .collect::<SResult<_>>()?;
    Ok(Item::new_stream(Zip{head: node.head, streams}))
}

struct Zip {
    head: Head,
    streams: Vec<Rc<dyn Stream>>,
}

impl Describe for Zip {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .push_args(&self.streams)
            .finish(prec)
    }
}

impl Stream for Zip {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iters = self.streams.iter()
            .map(|stm| stm.iter())
            .collect();
        ZipIter{iters, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.streams.iter()
            .map(|stm| stm.len())
            .reduce(Length::intersection)
            .unwrap() // at least 1 arg
    }
}

struct ZipIter {
    node: Rc<Zip>,
    iters: Vec<Box<dyn SIterator>>,
}

impl PreIterator for ZipIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let mut vec = Vec::with_capacity(self.iters.len());
        for iter in &mut self.iters {
            vec.push(iter_try!(iter.next()));
        }
        Ok(Some(vec.into()))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut remain = UNumber::zero();
        for iter in &mut self.iters {
            if let Some(r) = iter.advance(n)? {
                remain = std::cmp::max(remain, r);
            }
        }
        if remain.is_zero() { Ok(None) }
        else { Ok(Some(remain)) }
    }

    fn len_remain(&self) -> Length {
        self.iters.iter()
            .map(|iter| iter.len_remain())
            .reduce(Length::intersection)
            .unwrap()
    }

    fn origin(&self) -> &Rc<Zip> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_riffle() {
        use super::*;
        test_eval!("zip(seq,seq)" : 10 => "[[1, 1], [2, 2], [3, 3], [...], ...]");
        test_eval!("zip(1..3,'a'..'c',\"xyz\".chars)" : 12 => "[[1, 'a', 'x'], [2, 'b', 'y'], [3, 'c', 'z']]");
        test_eval!("seq.zip(1..2)" : 6 => "[[1, 1], [2, 2]]");
        test_eval!("\"ab\".zip(1..2)" => err);
        test_eval!("zip(seq)" => "[[1], [2], [...], ...]");
        test_len!("seq.zip([])" => 0);
        test_len!("seq.zip(1..10)" => 10);
        test_advance("seq.zip(seq)");
        test_advance("seq.zip(1..(10^20))");
        test_advance("seq.zip([])");
        test_describe!("seq.zip(seq)" => "zip(seq, seq)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("zip", eval_zip, r#"
A stream of arrays whose `n`-th element comes from the `n`-th input `streamN`.
The input which finishes first determines the length of the zip.
= ?(stream1, ..., streamM)
= stream1.?(stream2, ..., streamM)
> "abc".?chars.?(?seq) : 10 => [['a', 1], ['b', 2], ['c', 3]]
> ?(?range(1, 3), ?range(5, 10)) : 10 => [[1, 5], [2, 6], [3, 7]]
: riffle
: enum
: transpose
"#);
}
