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
    let max_index = indices.iter().max().unwrap_or(&UNumber::zero()).to_owned();
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
        Node::describe_helper(&self.head, Some(&self.source), &self.indices, prec, env)
    }
}

impl Stream for ReorderStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(ReorderIter {
            parent: self,
            iter: self.source.iter(),
            state: ReorderState::Args { vec_iter: self.indices.iter(), iter_pos: UNumber::zero() },
            pos: UNumber::zero()
        })
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct ReorderIter<'node> {
    parent: &'node ReorderStream,
    iter: Box<dyn SIterator + 'node>,
    state: ReorderState<'node>,
    pos: UNumber,
}

enum ReorderState<'node> {
    Args { vec_iter: std::slice::Iter<'node, UNumber>, iter_pos: UNumber },
    Missed { iter_pos: UNumber },
    Rest
}

impl Iterator for ReorderIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            match &mut self.state {
                ReorderState::Args{vec_iter, iter_pos} => {
                    if let Some(next) = vec_iter.next() {
                        let skip_res = if next > iter_pos {
                            iter_try_expr!(self.iter.advance(next - &*iter_pos - 1u32))
                        } else {
                            self.iter = self.parent.source.iter();
                            iter_try_expr!(self.iter.advance(next - 1u32))
                        };
                        if skip_res.is_some() {
                            return Some(Err(StreamError::new(format!("index past end ({next})"),
                                Node::new("*part", 
                                    Some(Item::Stream((*self.parent.source).clone_box()).into()),
                                    vec![Expr::new_number(next.to_owned())]))));
                        }
                        *iter_pos = next.to_owned();
                        match self.iter.next() {
                            Some(Ok(item)) => {
                                self.pos.inc();
                                return Some(Ok(item))
                            },
                            Some(Err(err)) => return Some(Err(err)),
                            None => {
                                return Some(Err(StreamError::new(format!("index past end ({next})"),
                                    Node::new("*part", 
                                        Some(Item::Stream((*self.parent.source).clone_box()).into()),
                                        vec![Expr::new_number(next.to_owned())]))));
                            }
                        }
                    }
                    if self.parent.max_index.to_usize()
                            .is_some_and(|max| max == self.parent.indices.len()) {
                        iter_try_expr!(self.iter.advance(&self.parent.max_index - &*iter_pos));
                        self.state = ReorderState::Rest;
                    } else {
                        self.iter = self.parent.source.iter();
                        self.state = ReorderState::Missed{iter_pos: UNumber::zero()};
                    }
                },
                ReorderState::Missed{iter_pos} => {
                    if *iter_pos >= self.parent.max_index {
                        self.state = ReorderState::Rest;
                        continue;
                    }
                    let next = self.iter.next();
                    iter_pos.inc();
                    if self.parent.indices.contains(iter_pos) {
                        continue;
                    } else {
                        self.pos.inc();
                        return next;
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
                ReorderState::Args{vec_iter, iter_pos} => {
                    while !n.is_zero() && vec_iter.next().is_some() {
                        n.dec();
                        self.pos.inc();
                    }
                    if n.is_zero() {
                        return Ok(None);
                    }
                    self.iter = self.parent.source.iter();
                    self.state = ReorderState::Missed{iter_pos: UNumber::zero()};
                },
                ReorderState::Missed{iter_pos} => {
                    let new_pos = &*iter_pos + &n;
                    let skipped = self.parent.indices.iter()
                        .filter(|ix| (&*iter_pos..&new_pos).contains(&&(*ix - 1u32)))
                        .count();
                    self.pos += &n - skipped;
                    match self.iter.advance(n) {
                        Ok(None) => {
                            *iter_pos = new_pos;
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
        test_eval!("('a'..'e').lenAM.reorder(6)" => err);
        test_eval!("('a'..'e').lenUF.reorder(6)" => "[<!>");
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
        test_advance("range(10).reorder(3,1,2,4,5)");
        test_advance("range(10).reorder(1,2,3,4,5,6,7,8,9,10)");
        test_advance("range(10).reorder(10,9,8,7,6,5,4,3,2,1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("reorder", eval_reorder);
}
