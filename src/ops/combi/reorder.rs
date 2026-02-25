use crate::base::*;

fn eval_reorder(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let mut indices = Vec::with_capacity(node.args.len());
    for index in &node.args {
        let Item::Number(ref index) = index else {
            return Err(StreamError::new(format!("expected number, found {:?}", index), node));
        };
        if !index.is_positive() {
            return Err(StreamError::new("indices must be positive", node));
        }
        let index = crate::utils::unsign(index.to_owned());
        if indices.contains(&index) {
            return Err(StreamError::new(format!("index {} repeats", index), node));
        }
        indices.push(index);
    }
    let Some(Item::Stream(stm)) = node.source else {
        return Err(StreamError::new("expected: stream.reorder(index...)", node));
    };
    let max_index = indices.iter().max().cloned().unwrap_or_default();
    if let Length::Exact(len) | Length::AtMost(len) = stm.len() {
        if max_index > len {
            let node = ENode { source: Some(Item::Stream(stm)), ..node };
            return Err(StreamError::new("requested index exceeds length of source", node));
        }
    }
    Ok(Item::new_stream(ReorderStream { source: stm.into(), head: node.head, indices, max_index }))
}

#[derive(Clone)]
struct ReorderStream {
    source: BoxedStream,
    head: Head,
    indices: Vec<UNumber>,
    max_index: UNumber,
}

impl Describe for ReorderStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.indices)
            .finish(prec)
    }
}

impl Stream for ReorderStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(ReorderIter {
            parent: self,
            iter: RandomAccess::new(&*self.source),
            state: ReorderState::Args { vec_iter: self.indices.iter() },
            pos: UNumber::zero()
        })
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct ReorderIter<'node> {
    parent: &'node ReorderStream,
    iter: RandomAccess<'node, Item>,
    state: ReorderState<'node>,
    pos: UNumber,
}

enum ReorderState<'node> {
    Args { vec_iter: std::slice::Iter<'node, UNumber> },
    Missed { index: UNumber },
    Rest
}

impl Iterator for ReorderIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            match &mut self.state {
                ReorderState::Args{vec_iter} => {
                    if let Some(next) = vec_iter.next() {
                        match self.iter.nth_from_start(next - 1u32) {
                            Some(Ok(item)) => {
                                self.pos.inc();
                                return Some(Ok(item))
                            },
                            Some(Err(err)) => return Some(Err(err)),
                            None => {
                                return Some(Err(StreamError::new(format!("index past end ({next})"),
                                    Node::new("[part]", 
                                        Some(self.parent.source.clone_item().into()),
                                        vec![Expr::new_number(next.to_owned())]))));
                            }
                        }
                    }
                    if self.parent.max_index.to_usize()
                            .is_some_and(|max| max == self.parent.indices.len()) {
                        iter_try_expr!(self.iter.move_to(self.parent.max_index.to_owned()));
                        self.state = ReorderState::Rest;
                    } else {
                        self.state = ReorderState::Missed{index: UNumber::one()};
                    }
                },
                ReorderState::Missed{index} => {
                    if *index > self.parent.max_index {
                        iter_try_expr!(self.iter.move_to(&*index - 1u32));
                        self.state = ReorderState::Rest;
                        continue;
                    }
                    if self.parent.indices.contains(index) {
                        index.inc();
                        continue;
                    } else {
                        let ret = self.iter.nth_from_start(&*index - 1u32);
                        index.inc();
                        self.pos.inc();
                        return ret;
                    }
                },
                ReorderState::Rest => {
                    self.pos.inc();
                    return self.iter.next();
                }
            }
        }
    }
}

impl SIterator for ReorderIter<'_> {
    fn len_remain(&self) -> Length {
        self.parent.source.len().map(|len| if len >= &self.pos { len - &self.pos }
            else { UNumber::zero() })
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            match &mut self.state {
                ReorderState::Args{vec_iter} => {
                    while !n.is_zero() && vec_iter.next().is_some() {
                        n.dec();
                        self.pos.inc();
                    }
                    if n.is_zero() {
                        return Ok(None);
                    }
                    self.state = ReorderState::Missed{index: UNumber::one()};
                },
                ReorderState::Missed{index} => {
                    let new_index = &*index + &n;
                    let skipped = self.parent.indices.iter()
                        .filter(|ix| (&*index..&new_index).contains(ix))
                        .count();
                    self.pos += &n - skipped;
                    match self.iter.advance(n) {
                        Ok(None) => {
                            *index = new_index;
                            n = skipped.into()
                        },
                        Ok(Some(remain)) => return Ok(Some(remain + skipped)),
                        Err(err) => return Err(err)
                    }
                    if self.pos > self.parent.max_index {
                        self.state = ReorderState::Rest;
                    }
                },
                ReorderState::Rest => {
                    self.pos += &n;
                    return self.iter.advance(n)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reorder() {
        use super::*;
        test_eval!("('a'..'e').reorder(1)" => "['a', 'b', 'c', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3)" => "['c', 'a', 'b', 'd', 'e']");
        test_eval!("('a'..'e').reorder(5)" => "['e', 'a', 'b', 'c', 'd']");
        test_eval!("('a'..'e').reorder(0)" => err);
        test_eval!("('a'..'e').reorder(6)" => err);
        test_eval!("('a'..'e').reorder()" => "['a', 'b', 'c', 'd', 'e']");
        test_eval!("('a'..'e').$lenAM.reorder(6)" => err);
        test_eval!("('a'..'e').$lenUF.reorder(6)" => "[<!>");
        test_eval!("('a'..'e').reorder(3,2)" => "['c', 'b', 'a', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,4)" => "['c', 'd', 'a', 'b', 'e']");
        test_eval!("('a'..'e').reorder(2,4)" => "['b', 'd', 'a', 'c', 'e']");
        test_eval!("('a'..'e').reorder(3,1)" => "['c', 'a', 'b', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,2,1)" => "['c', 'b', 'a', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,3)" => err);
        test_eval!("('a'..'e').reorder(3,3)" => err);
        test_eval!("range(10^100).reorder(10^10)" => "[10000000000, 1, 2, 3, 4, ...]");
        test_advance("range(10).reorder(5)");
        test_advance("range(10).reorder(5,2)");
        test_advance("range(10).reorder(5,6)");
        test_advance("range(10).reorder(5,7)");
        test_advance("range(10).reorder");
        test_advance("range(10).reorder(3,1,2,4,5)");
        test_advance("range(10).reorder(1,2,3,4,5,6,7,8,9,10)");
        test_advance("range(10).reorder(10,9,8,7,6,5,4,3,2,1)");
        test_describe!("range(10).reorder(5,2)" => "range(10).reorder(5, 2)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("reorder", eval_reorder, r#"
A stream applying a given permutation on the input `stream`: returning `stream[i1]`, `stream[i2]` etc., followed by all the remaining items.
= stream.?(i1, ..., iK)
> ('a'..'e').?(3, 2, 1) => ['c', 'b', 'a', 'd', 'e']
> (1..5).?(3) => [3, 1, 2, 4, 5]
> ?seq.?(10, 10) => !indices can not repeat
: perm
"#);
}
