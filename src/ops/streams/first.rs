use crate::base::*;

fn eval_first(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let count = match &node.args[..] {
        [] => None,
        [Item::Number(count)] => Some(count.try_into().map_err(|_| StreamError::new0("count can't be negative"))?),
        _ => return Err(StreamError::new0("expected: source.first or source.first(count)"))
    };
    match (node.source_checked()?, count) {
        (Item::Stream(stm), None) => first_item_impl(&**stm),
        (Item::Stream(stm), Some(count)) => Ok(Item::new_stream(First{head: node.head.clone(), source: Rc::clone(stm), count })),
        (Item::String(stm), None) => first_item_impl(&**stm).map(Item::Char),
        (Item::String(stm), Some(count)) => Ok(Item::new_string(First{head: node.head.clone(), source: Rc::clone(stm), count })),
        _ => Err(StreamError::new0("expected: source.first or source.first(count)"))
    }
}

fn first_item_impl<I: ItemType>(stm: &dyn Stream<I>) -> Result<I, StreamError> {
    match stm.iter().next() {
        Some(result) => Ok(result?),
        None => Err(StreamError::new0("stream is empty"))
    }
}

struct First<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    count: UNumber
}

struct FirstIter<'node, I: ItemType> {
    source: Box<dyn SIterator<I> + 'node>,
    count_rem: UNumber
}

impl<I: ItemType> Stream<I> for First<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(FirstIter { source: self.source.iter(), count_rem: self.count.clone() })
    }

    fn len(&self) -> Length {
        Length::intersection(self.source.len(), Length::Exact(self.count.to_owned()))
    }
}

impl<I: ItemType> Describe for First<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.count)
            .finish(prec)
    }
}

impl<I: ItemType> Iterator for FirstIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem -= 1;
            self.source.next()
        } else {
            None
        }
    }
}

impl<I: ItemType> SIterator<I> for FirstIter<'_, I> {
    fn len_remain(&self) -> Length {
        Length::intersection(self.source.len_remain(), Length::Exact(self.count_rem.to_owned()))
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if n > self.count_rem {
            n -= &self.count_rem;
            self.count_rem = UNumber::zero();
            Ok(Some(n))
        } else {
            self.count_rem -= &n;
            self.source.advance(n)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_first() {
        test_eval!("(1..3).first" => "1");
        test_eval!("(1..3).first(0)" => "[]");
        test_eval!("(1..3).first(1)" => "[1]");
        test_eval!("(1..3).first(2)" => "[1, 2]");
        test_eval!("(1..3).first(3)" => "[1, 2, 3]");
        test_eval!("(1..3).first(4)" => "[1, 2, 3]");
        test_eval!("\"abc\".first" => "'a'");
        test_eval!("\"abc\".first(0)" => "\"\"");
        test_eval!("\"abc\".first(1)" => "\"a\"");
        test_eval!("\"abc\".first(4)" => "\"abc\"");
        test_eval!("seq.first(10^10).len" => "10000000000");
        test_eval!("range(10^9).first(10^10).len" => "1000000000");
        test_eval!("range(10^11).first(10^10).len" => "10000000000");
        test_len!("(1..3).first(0)" => 0);
        test_len!("(1..3).first(1)" => 1);
        test_len!("(1..3).first(2)" => 2);
        test_len!("(1..3).first(3)" => 3);
        test_len!("(1..3).first(4)" => 3);
        test_advance("seq.first(10^10)");
        test_advance("range(10^9).first(10^10)");
        test_advance("range(10^11).first(10^10)");
        test_describe!("(1..3).first" => "1");
        test_describe!("(1..3).first(4)" => "(1..3).first(4)");
        test_describe!("(1..3).take(4)" => "(1..3).take(4)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["first", "take"], eval_first, r#"
Up to the first `count` items of `stream`, or up to the first `count` characters of `string`.
If `count` is not given, only gives the first item or the first character (no stream / string).
= stream.?
= stream.?(count)
= string.?
= string.?(count)
> ?seq.? => 1
> ?seq.?(2) => [1, 2]
> [1, 2, 3].?(10) => [1, 2, 3]
> "abc".? => 'a'
> "abc".?(2) => "ab"
: last
"#);
}
