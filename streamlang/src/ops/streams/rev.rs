use crate::base::*;

fn eval_rev(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    match node.source_checked()? {
        Item::Stream(stm) => eval_rev_impl(&node.head, stm),
        Item::String(stm) => eval_rev_impl(&node.head, stm),
        _ => Err(StreamError::usage(&node.head))
    }
}

fn eval_rev_impl<I: ItemType>(head: &Head, source: &Rc<dyn Stream<I>>) -> SResult<Item> {
    match source.len() {
        Length::Infinite => Err("input is infinite".into()),
        Length::Exact(len) if usize::try_from(&len).is_ok_and(|len| len > CACHE_LEN) =>
            Ok(Item::from(Rc::new(Rev{head: head.clone(), source: Rc::clone(source), length: len})
                    as Rc<dyn Stream<I>>)),
        _ => {
            let mut vec = source.listout()?;
            vec.reverse();
            Ok(vec.into())
        }
    }
}

pub struct Rev<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    length: UNumber
}

impl<I: ItemType> Stream<I> for Rev<I> {
    fn into_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        RevIter {
            start: self.length.clone(),
            cached: Vec::new(),
            node: self,
        }.wrap()
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

struct RevIter<I: ItemType> {
    node: Rc<Rev<I>>,
    start: UNumber,
    cached: Vec<I>
}

impl<I: ItemType> PreIterator<I> for RevIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        match self.cached.pop() {
            Some(item) => Ok(Some(item)),
            None => {
                if self.start.is_zero() {
                    Ok(None)
                } else {
                    let size_n = UNumber::from(CACHE_LEN);
                    let (new_start, diff) = if size_n <= self.start {
                        (&self.start - size_n, CACHE_LEN)
                    } else {
                        (UNumber::zero(), usize::try_from(&self.start)
                            .expect("start < CACHE_LEN should fit into usize"))
                    };
                    let mut iter = self.node.source.iter();
                    iter.advance(new_start.clone())?;
                    self.start = new_start;
                    self.cached = iter.transposed().take(diff).collect::<SResult<_>>()?;
                    Ok(self.cached.pop())
                }
            }
        }
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        let len = self.cached.len();
        match usize::try_from(&n) {
            Ok(n) if n <= len => {
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

    fn origin(&self) -> &Rc<Rev<I>> {
        &self.node
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("rev", eval_rev, r#"
The input `stream` or `string`, reversed.
! May need to evaluate the entire contents of the input.
= stream.?
= string.?
> ?range(5).? => [5, 4, 3, 2, 1]
> "Hello".? => "olleH"
> ?seq.? => !stream is infinite
"#);
}
