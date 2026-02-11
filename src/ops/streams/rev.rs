use crate::base::*;

fn eval_rev(node: Node, env: &Env) -> Result<Item, StreamError> {
    let enode = node.eval_all(env)?;
    try_with!(enode, enode.check_no_args()?);
    let rnode = enode.resolve_source()?;
    match rnode.source {
        Item::Stream(stm) => eval_rev_impl(rnode.head, stm),
        Item::String(stm) => eval_rev_impl(rnode.head, stm),
        _ => Err(StreamError::new("expected stream or string", rnode))
    }
}

fn eval_rev_impl<I: ItemType>(head: Head, source: Box<dyn Stream<I>>) -> Result<Item, StreamError> {
    match source.len() {
        Length::Infinite
            => Err(StreamError::new("input is infinite", Item::from(source))),
        Length::Exact(len) if len.to_usize().is_some_and(|len| len > CACHE_LEN) =>
            Ok(Item::from(Box::new(Rev{head, source: source.into(), length: len})
                    as Box<dyn Stream<I>>)),
        _ => {
            let mut vec = source.listout()?;
            vec.reverse();
            Ok(vec.into())
        }
    }
}

#[derive(Clone)]
pub struct Rev<I: ItemType> {
    head: Head,
    source: BoxedStream<I>,
    length: UNumber
}

impl<I: ItemType> Stream<I> for Rev<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(RevIter {
            source: &*self.source,
            start: self.length.clone(),
            cached: Vec::new()
        })
    }

    fn len(&self) -> Length {
        Length::Exact(self.length.clone())
    }
}

impl<I: ItemType> Describe for Rev<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

struct RevIter<'node, I: ItemType> {
    source: &'node (dyn Stream<I> + 'static),
    start: UNumber,
    cached: Vec<I>
}

impl<I: ItemType> Iterator for RevIter<'_, I> {
    type Item = Result<I, StreamError>;

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
                    iter_try_expr!(iter.advance(new_start.clone()));
                    self.start = new_start;
                    self.cached = iter_try_expr!(iter.take(diff).collect());
                    Ok(self.cached.pop()).transpose()
                }
            }
        }
    }
}

impl<I: ItemType> SIterator<I> for RevIter<'_, I> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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
        test_eval!("range(3).rev" => "[3, 2, 1]");
        test_eval!("\"abc\".rev" => "\"cba\"");
        test_eval!("\"abc\".repeat(10^8).rev" => "\"cbacbacbacbacbacbacb...");
        test_eval!("[].rev" => "[]");
        test_eval!("\"\".rev" => "\"\"");
        test_eval!("seq.rev" => err);
        test_advance("range(1000).rev");
        test_advance("range(10^10).rev");
        //test_describe!("\"abc\".rev" => "\"abc\".rev"); // TODO caching
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("rev", eval_rev);
}
