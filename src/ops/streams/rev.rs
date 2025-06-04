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

fn eval_rev_impl<ItemType: ItemTypeT>(head: Head, source: Box<dyn Stream<ItemType>>) -> Result<Item, StreamError> {
    match source.length() {
        Length::Infinite
            => Err(StreamError::new("input is infinite", ItemType::from_box(source))),
        Length::Exact(len) if len.to_usize().is_some_and(|len| len > CACHE_LEN) =>
            Ok(ItemType::from_box(Box::new(Rev{head, source: source.into(), length: len}))),
        _ => {
            let mut vec = ItemType::listout(&*source)?;
            vec.reverse();
            Ok(ItemType::from_vec(vec))
        }
    }
}

#[derive(Clone)]
pub struct Rev<ItemType: ItemTypeT> {
    head: Head,
    source: BoxedStream<ItemType>,
    length: UNumber
}

impl<ItemType: ItemTypeT> Stream<ItemType> for Rev<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<ItemType> + 'node> {
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

impl<ItemType: ItemTypeT> Describe for Rev<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

struct RevIter<'node, ItemType: ItemTypeT> {
    source: &'node (dyn Stream<ItemType> + 'static),
    start: UNumber,
    cached: Vec<ItemType>
}

impl<ItemType: ItemTypeT> Iterator for RevIter<'_, ItemType> {
    type Item = Result<ItemType, StreamError>;

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
                    iter_try_expr!(iter.skip_n(new_start.clone()));
                    self.start = new_start;
                    self.cached = iter_try_expr!(iter.take(diff).collect());
                    Ok(self.cached.pop()).transpose()
                }
            }
        }
    }
}

impl<ItemType: ItemTypeT> SIterator<ItemType> for RevIter<'_, ItemType> {
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

        test_eval!("range(3).rev" => "[3, 2, 1]");
        test_eval!("\"abc\".rev" => "\"cba\"");
        test_eval!("\"abc\".repeat(10^8).rev" => "\"cbacbacbacbacbacbacb...");
        test_eval!("[].rev" => "[]");
        test_eval!("\"\".rev" => "\"\"");
        test_eval!("seq.rev" => err);
        test_skip_n("range(1000).rev");
        test_skip_n("range(10^10).rev");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("rev", eval_rev);
}
