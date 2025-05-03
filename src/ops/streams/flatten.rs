use crate::base::*;

#[derive(Clone)]
struct Flatten {
    source: BoxedStream,
    head: Head
}

impl Flatten {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        match node.eval_all(env)?.resolve_source()? {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero } => {
                Ok(Item::new_stream(Flatten { source: stm.into(), head }))
            },
            node => Err(StreamError::new("expected: stream.flatten", node))
        }
    }
}

impl Describe for Flatten {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec)
    }
}

impl Stream for Flatten {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(FlattenIter{outer: self.source.iter(), inner: None})
    }

    fn length(&self) -> Length {
        if self.is_empty() {
            Length::Exact(UNumber::zero())
        } else {
            Length::Unknown
        }
    }

    fn is_empty(&self) -> bool {
        self.source.is_empty()
    }
}

struct FlattenIter<'node> {
    outer: Box<dyn SIterator + 'node>,
    inner: Option<OwnedStreamIter<'node>>
}

impl Iterator for FlattenIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(inner) = &mut self.inner {
                let next = inner.next();
                if next.is_some() {
                    return next;
                } else {
                    self.inner = None;
                }
            }
            match self.outer.next()? {
                Ok(Item::Stream(stm)) => {
                    self.inner = Some(stm.into());
                    continue;
                },
                res => return Some(res)
            }
        }
    }
}

impl SIterator for FlattenIter<'_> {
    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            println!("{n:?}");
            if let Some(inner) = &mut self.inner {
                let Some(m) = inner.skip_n(n)?
                    else { return Ok(None); };
                self.inner = None;
                n = m;
            }
            if n.is_zero() {
                return Ok(None);
            }
            match self.outer.next() {
                Some(Ok(Item::Stream(stm))) => {
                    self.inner = Some(stm.into());
                },
                Some(Ok(_)) => {
                    n.dec();
                    if n.is_zero() {
                        return Ok(None);
                    }
                },
                Some(Err(err)) => return Err(err),
                None => return Ok(Some(n))
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

        assert_eq!(parse("[1, [2, [3]]].flatten").unwrap().eval().unwrap().to_string(), "[1, 2, [3]]");
        test_skip_n(&parse("[1,range(3)].repeat(10).flatten").unwrap().eval().unwrap());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("flatten", Flatten::eval);
}
