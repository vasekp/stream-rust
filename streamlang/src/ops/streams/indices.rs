use crate::base::*;

fn eval_indices(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    match &node.source {
        Some(Item::Stream(stm)) => {
            let queries = node.args.clone();
            Ok(Item::new_stream(Indices{
                head: node.head.clone(),
                source: Rc::clone(stm),
                queries
            }))
        },
        Some(Item::String(stm)) => {
            let queries = node.args.iter()
                .map(Item::to_char)
                .collect::<SResult<Vec<_>>>()?;
            Ok(Item::new_stream(Indices{
                head: node.head.clone(),
                source: Rc::clone(stm),
                queries
            }))
        },
        _ => Err(StreamError::usage(&node.head))
    }
}

struct Indices<I: ItemType> {
    source: Rc<dyn Stream<I>>,
    queries: Vec<I>,
    head: Head
}

impl<I: ItemType> Stream for Indices<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        IndicesIter{iter: self.source.iter(), index: 0u32.into(), node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl<I: ItemType> Describe for Indices<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.queries)
            .finish(prec)
    }
}

struct IndicesIter<I: ItemType> {
    iter: Box<dyn SIterator<I>>,
    index: UNumber,
    node: Rc<Indices<I>>,
}

impl<I: ItemType> PreIterator for IndicesIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        while let Some(item) = self.iter.next()? {
            check_stop!();
            self.index += 1;
            for query in &self.node.queries {
                if item.try_eq(&query)? {
                    return Ok(Some(Item::new_number(&self.index)));
                }
            }
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }

    fn origin(&self) -> &Rc<Indices<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_indices() {
        use super::*;
        test_eval!("\"hello\".indices('l')" => "[3, 4]");
        test_eval!("\"hello\".indices('l', 'l', 'e')" => "[2, 3, 4]");
        test_eval!("[1, 2, [], 2, []].indices(2, [])" => "[2, 3, 4, 5]");
        test_eval!("[1, 2, [], 2, []].indices([], [])" => "[3, 5]");
        test_eval!("[1, 2, [], 2, []].indices(3)" => "[]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["indices", "positions"], eval_indices, r#"
Returns all indices in which an `item` appears in `stream`, or in which `char` appears in `string`.
Multiple items can be specified, in that case, an index is returned if it matches any of them.
! Does not accept substrings in contrast to `?index` or `?contains`.
= stream.?(item, ...)
= string.?(char, ...)
> [1, 0, 1, 0, 2].?(0) => [2, 4]
> "hello".?('l') => [3, 4]
> "This is a test".?('a', 'e', 'i', 'o', 'u') => [3, 6, 9, 12]
: index
: contains
"#);
}
