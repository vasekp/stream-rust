use crate::base::*;

fn eval_zip(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    for arg in node.source.iter().chain(node.args.iter()) {
        if !arg.is_stream() {
            return Err(StreamError::new(format!("expected stream, found {:?}", arg), node));
        }
    }
    let streams = node.source.into_iter().chain(node.args)
        .map(|item| match item {
            Item::Stream(stm) => stm.into(),
            _ => unreachable!()
        })
        .collect();
    Ok(Item::new_stream(Zip{head: node.head, streams}))
}

#[derive(Clone)]
struct Zip {
    head: Head,
    streams: Vec<BoxedStream>,
}


impl Describe for Zip {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, None::<&Item>, &self.streams, prec, env)
    }
}

impl Stream for Zip {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let iters = self.streams.iter()
            .map(|stm| stm.iter())
            .collect();
        Box::new(ZipIter{iters})
    }

    fn length(&self) -> Length {
        self.streams.iter()
            .map(|stm| stm.length())
            .reduce(Length::intersection)
            .unwrap() // at least 1 arg
    }
}

struct ZipIter<'node> {
    iters: Vec<Box<dyn SIterator + 'node>>
}

impl Iterator for ZipIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut vec = Vec::with_capacity(self.iters.len());
        for iter in &mut self.iters {
            vec.push(iter_try_expr!(iter.next()?));
        }
        Some(Ok(vec.into()))
    }
}

impl SIterator for ZipIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let mut remain = UNumber::zero();
        for iter in &mut self.iters {
            if let Some(r) = iter.skip_n(n.clone())? {
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
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_riffle() {
        use super::*;
        use crate::parser::parse;
        test_eval!("zip(seq,seq)" => "[[1, 1], [2, ...], ...]");
        test_eval!("zip(1..3,'a'..'c',\"xyz\".chars)" => "[[1, 'a', 'x'], [...], ...]");
        test_eval!("zip(1..3,'a'..'c',\"xyz\".chars)[3]" => "[3, 'c', 'z']");
        test_eval!("seq.zip(1..2)" => "[[1, 1], [2, ...]]");
        test_eval!("\"ab\".zip(1..2)" => err);
        test_eval!("zip(seq)" => "[[1], [2], [...], ...]");
        test_len!("seq.zip([])" => 0);
        test_len!("seq.zip(1..10)" => 10);
        test_skip_n("seq.zip(seq)");
        test_skip_n("seq.zip(1..(10^20))");
        test_skip_n("seq.zip([])");
        //test_describe!("seq.zip(seq)" => "seq.zip(seq)"); // TODO
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("zip", eval_zip);
}
