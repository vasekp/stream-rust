use crate::base::*;

#[derive(Clone)]
struct Flatten {
    source: BoxedStream,
    depth: Option<UNumber>,
    head: Head
}

impl Flatten {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        match node.eval_all(env)?.resolve_source()? {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero } => {
                Ok(Item::new_stream(Flatten { source: stm.into(), head, depth: None }))
            },
            RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Item::Number(depth)) } if !depth.is_negative() => {
                Ok(Item::new_stream(Flatten { source: stm.into(), head, depth: Some(crate::utils::unsign(depth)) }))
            },
            node => Err(StreamError::new("expected: stream.flatten or stream.flatten(depth)", node))
        }
    }
}

impl Describe for Flatten {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), self.depth.as_ref(), prec, env)
    }
}

impl Stream for Flatten {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(FlattenIter {
            outer: self.source.iter(),
            iters: vec![],
            depth: self.depth.as_ref().and_then(UNumber::to_usize),
        })
    }

    fn len(&self) -> Length {
        if self.source.is_empty() {
            Length::Exact(UNumber::zero())
        } else {
            Length::Unknown
        }
    }
}

struct FlattenIter<'node> {
    outer: Box<dyn SIterator + 'node>,
    iters: Vec<OwnedStreamIter>,
    depth: Option<usize>,
}

impl Iterator for FlattenIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            let res = match self.iters.last_mut() {
                Some(iter) => iter.next(),
                None => self.outer.next()
            };
            match res {
                Some(Ok(Item::Stream(stm))) => {
                    if self.depth.is_some_and(|d| self.iters.len() == d) {
                        return Some(Ok(Item::Stream(stm)));
                    } else {
                        self.iters.push(stm.into_iter());
                    }
                },
                Some(res) => return Some(res),
                None => {
                    if self.iters.is_empty() {
                        return None;
                    } else {
                        self.iters.pop();
                    }
                }
            }
        }
    }
}

impl SIterator for FlattenIter<'_> {
    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            if self.depth.is_some_and(|d| self.iters.len() == d) {
                let Some(iter) = self.iters.last_mut() else { unreachable!() };
                let Some(m) = iter.advance(n)? else { return Ok(None); };
                self.iters.pop();
                n = m;
            } else {
                let res = match self.iters.last_mut() {
                    Some(iter) => iter.next(),
                    None => self.outer.next()
                };
                match res {
                    Some(Ok(Item::Stream(stm))) => {
                        self.iters.push(stm.into_iter());
                    },
                    Some(Ok(_)) => n.dec(),
                    Some(Err(err)) => return Err(err),
                    None => {
                        if self.iters.is_empty() {
                            return Ok(Some(n));
                        } else {
                            self.iters.pop();
                        }
                    }
                }
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flatten() {
        use crate::parser::parse;

        test_eval!("[1, [2, [3]]].flatten" => "[1, 2, 3]");
        test_eval!("[1, [2, [3]]].flatten(1)" => "[1, 2, [3]]");
        test_eval!("[0].nest{[#]}.flatten" => "[0, 0, 0, 0, 0, ...]");
        test_eval!("[0].nest{[#]}.flatten(3)" => "[0, 0, [0], [...], ...]");
        test_eval!("[0].nest{[#]}.flatten(10^10)" => "[0, 0, 0, 0, 0, ...]");
        test_eval!("[\"ab\",\"cd\"].flatten" => "[\"ab\", \"cd\"]");
        test_advance("[1,range(3)].repeat(10).flatten");
        test_advance("[1,[2,[3]]].repeat(10).flatten");
        test_advance("[1,[2,[3]]].repeat(10).flatten(1)");
        test_advance("[1,[2,[3]]].repeat(10).flatten(2)");
        test_advance("[1,[2,[3]]].repeat(10).flatten(10)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("flatten", Flatten::eval);
}
