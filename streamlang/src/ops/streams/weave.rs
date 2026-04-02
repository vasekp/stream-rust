use crate::base::*;

fn eval_weave(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    node.check_args_nonempty()?;
    let streams = node.args.iter()
        .map(Item::to_stream)
        .collect::<SResult<Vec<_>>>()?;
    Ok(Item::new_stream(Weave{head: node.head.clone(), streams}))
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
        let iters = self.streams.iter()
            .map(|stm| stm.iter())
            .collect();
        WeaveIter{iters, index: 0, node: self}.wrap()
    }

    fn len(&self) -> Length {
        let num = self.streams.len();
        self.streams.iter().enumerate()
            .map(|(ix, stm)| stm.len().map(|len| len * num + ix))
            .reduce(Length::intersection).unwrap()
    }
}

struct WeaveIter {
    node: Rc<Weave>,
    iters: Vec<Box<dyn SIterator>>,
    index: usize,
}

impl PreIterator for WeaveIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let res = iter_try!(self.iters[self.index].next());
        self.index += 1;
        if self.index >= self.iters.len() {
            self.index = 0;
        }
        Ok(Some(res))
    }

    fn advance(&mut self, mut n: UNumber) -> SResult<Option<UNumber>> {
        let common = self.iters.iter()
            .map(|it| it.len_remain())
            .reduce(Length::intersection).unwrap();
        let num = self.iters.len();
        let skip = match Length::intersection(common, Length::Exact(&n / num)) {
            Length::Exact(len) => len,
            _ => UNumber::zero()
        };
        for iter in self.iters.iter_mut() {
            iter.advance(skip.clone())?;
            n -= &skip;
        }
        while !n.is_zero() {
            if self.next()?.is_none() {
                return Ok(Some(n));
            }
            n -= 1;
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        let num = self.iters.len();
        self.iters.iter().enumerate()
            .map(|(ix, stm)| stm.len_remain().map(|len| len * num + ((ix + num - self.index) % num)))
            .reduce(Length::intersection).unwrap()
    }

    fn origin(&self) -> &Rc<Weave> {
        &self.node
    }
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
        test_advance("weave(1..10, 1..5)");
        test_advance("weave(seq, 1..10^5)");
        test_advance("weave(1..10^5, seq)");
        test_advance("weave(seq, seq)");
        test_advance("weave([0], 1..10)");
        test_advance("weave(1..10, [0])");
        test_advance("weave(1..1000, 1..100, 1..10)");
    }
}

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
