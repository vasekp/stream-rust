use crate::base::*;

#[derive(Clone)]
pub struct Rev {
    head: Head,
    source: BoxedStream,
    length: UNumber
}

impl Rev {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let enode = node.eval_all(env)?;
        try_with!(enode, enode.check_no_args()?);
        let rnode = enode.resolve_source()?;
        let is_string = rnode.source.is_string();
        let (Item::Stream(source) | Item::String(source)) = &rnode.source else {
            return Err(StreamError::new("expected stream or string", rnode));
        };
        match source.length() {
            Length::Infinite
                => Err(StreamError::new("input is infinite", rnode)),
            Length::Exact(len) if len.to_usize().is_some_and(|len| len > CACHE_LEN) => {
                let (Item::Stream(source) | Item::String(source)) = rnode.source else { unreachable!() };
                Ok(Item::new_stream_or_string(Rev{head: rnode.head, source: source.into(), length: len}, is_string))
            },
            _ => {
                let mut vec = source.listout()?;
                vec.reverse();
                Ok(Item::new_stream_or_string(List::from(vec), is_string))
            }
        }
    }
}

impl Stream for Rev {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(RevIter {
            source: &*self.source,
            start: self.length.clone(),
            cached: Vec::new()
        })
    }

    fn length(&self) -> Length {
        Length::Exact(self.length.clone())
    }
}

impl Describe for Rev {
    fn describe_inner(&self, prec: u32, env: &Rc<Env>) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

struct RevIter<'node> {
    source: &'node (dyn Stream + 'static),
    start: UNumber,
    cached: Vec<Item>
}

impl Iterator for RevIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cached.pop() {
            Some(item) => Some(Ok(item)),
            None => {
                if self.start.is_zero() {
                    None
                } else {
                    let size_n = CACHE_LEN.into();
                    let (new_start, diff) = match self.start.checked_sub(&size_n) {
                        Some(res) => (res, CACHE_LEN),
                        None => (UNumber::zero(), self.start.to_usize().expect("start < CACHE_LEN should fit into usize"))
                    };
                    let mut iter = self.source.iter();
                    match iter.skip_n(new_start.clone()) {
                        Err(err) => return Some(Err(err)),
                        Ok(Some(_)) => unreachable!(),
                        Ok(None) => ()
                    }
                    self.start = new_start;
                    self.cached = match iter.take(diff).collect() {
                        Ok(vec) => vec,
                        Err(err) => return Some(Err(err))
                    };
                    Ok(self.cached.pop()).transpose()
                }
            }
        }
    }
}

impl SIterator for RevIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let len = self.cached.len();
        match n.to_usize() {
            Some(n) if n <= len => {
                self.cached.truncate(len - n);
                Ok(None)
            },
            _ => {
                self.cached.clear();
                let rem = UNumber::from(n - len);
                if rem > self.start {
                    Ok(Some(rem - &self.start))
                } else {
                    self.start -= rem;
                    Ok(None)
                }
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::Exact(&self.start + self.cached.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rev() {
        use crate::parser::parse;

        assert_eq!(parse("range(3).rev").unwrap().eval_default().unwrap().to_string(), "[3, 2, 1]");
        assert_eq!(parse("\"abc\".rev").unwrap().eval_default().unwrap().to_string(), "\"cba\"");
        assert_eq!(parse("\"abc\".repeat(10^8).rev").unwrap().eval_default().unwrap().to_string(), "\"cbacbacbacbacbacbacb...");
        assert_eq!(parse("[].rev").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("\"\".rev").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert!(parse("seq.rev").unwrap().eval_default().is_err());
        test_skip_n(&parse("range(1000).rev").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^10).rev").unwrap().eval_default().unwrap());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("rev", Rev::eval);
}
