use crate::base::*;

fn eval_transpose(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let source = node.source_checked()?;
    let streams = source.to_stream()?
        .listout()?
        .into_iter()
        .map(Item::into_stream)
        .collect::<SResult<Vec<_>>>()?;
    if streams.is_empty() {
        Ok(Item::empty_stream())
    } else {
        Ok(Item::new_stream(Transpose{
            head: node.head.clone(),
            source: source.clone(),
            streams,
        }))
    }
}

struct Transpose {
    head: Head,
    source: Item,
    streams: Vec<Rc<dyn Stream>>,
}

impl Describe for Transpose {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Transpose {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iters = self.streams
            .iter()
            .map(<dyn Stream>::iter)
            .collect();
        TransposeIter{iters, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.streams.iter()
            .map(|s| s.len())
            .reduce(Length::intersection)
            .unwrap_or(Length::empty())
    }
}

struct TransposeIter {
    node: Rc<Transpose>,
    iters: Vec<Box<dyn SIterator>>,
}

impl PreIterator for TransposeIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let vec = self.iters.iter_mut()
            .map(|iter| iter.next())
            .collect::<SResult<Option<Vec<_>>>>()?;
        Ok(vec.map(Item::from))
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        let mut ret = None;
        for iter in &mut self.iters {
            if let Some(this_rem) = iter.advance(n.clone())? {
                if let Some(rem) = ret {
                    ret = Some(std::cmp::max(rem, this_rem));
                } else {
                    ret = Some(this_rem);
                }
            }
        }
        Ok(ret)
    }

    fn len_remain(&self) -> Length {
        self.iters.iter()
            .map(|s| s.len_remain())
            .reduce(Length::intersection)
            .unwrap_or(Length::empty())
    }

    fn origin(&self) -> &Rc<Transpose> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_transpose() {
        use super::*;
        test_eval!("[[1, 2, 3], [4, 5]].trans" : 10 => "[[1, 4], [2, 5]]");
        test_eval!("[].trans" => "[]");
        test_eval!("[[1,2,3], seq].trans" : 10 => "[[1, 1], [2, 2], [3, 3]]");
        test_eval!("[[1,2,3], [], seq].trans" => "[]");
        test_len!("[[1,2,3], [], seq].trans" => 0);
        test_len!("[[1,2,3], [5], seq].trans" => 1);
        test_len!("[[1,2,3], [5,6,7,8], seq].trans" => 3);
        test_len!("[seq, seq].trans" => Length::Infinite);
        test_len!("[[1,2,3].$lenUF, [], seq].trans" => 0);
        test_len!("[[1,2,3].$lenUF, [1], seq].trans" => Length::AtMost(1u32.into()));
        test_advance("[[1,2,3], [], seq].trans");
        test_advance("[[1,2,3], seq].trans");
        test_advance("[[1,2,3].$lenUU, seq].trans");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["transpose", "trans"], eval_transpose, r#"
Takes a stream of streams and returns another, transposed.
The shortest stream determines the length.
= stream.?
> [?seq, [4, 5], "text".?chars].? : 10 => [[1, 4, 't'], [2, 5, 'e']]
: zip
"#);
}
