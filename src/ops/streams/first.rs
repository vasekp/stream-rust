use crate::base::*;
use crate::utils::unsign;

fn eval_first(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, .. }
            => first_item_impl(&**stm).map_err(|err| StreamError::new(err, rnode)),
        RNodeS { source: Item::String(ref stm), args: RArgs::Zero, .. }
            => first_item_impl(&**stm).map(Item::Char).map_err(|err| StreamError::new(err, rnode)),
        RNodeS { head, source: Item::Stream(s), args: RArgs::One(Item::Number(count)) }
                if !count.is_negative()
            => Ok(Item::new_stream(First{head, source: s.into(), count: unsign(count)})),
        RNodeS { head, source: Item::String(s), args: RArgs::One(Item::Number(count)) }
                if !count.is_negative()
            => Ok(Item::new_string_stream(First{head, source: s.into(), count: unsign(count)})),
        _ => Err(StreamError::new("expected: source.first or source.first(count)", rnode))
    }
}

fn first_item_impl<ItemType: ItemTypeT>(stm: &dyn Stream<ItemType>) -> Result<ItemType, BaseError> {
    match stm.iter().next() {
        Some(result) => Ok(result?),
        None => Err("stream is empty".into())
    }
}

#[derive(Clone)]
struct First<ItemType: ItemTypeT> {
    head: Head,
    source: BoxedStream<ItemType>,
    count: UNumber
}

struct FirstIter<'node, ItemType: ItemTypeT> {
    source: Box<dyn SIterator<ItemType> + 'node>,
    count_rem: UNumber
}

impl<ItemType: ItemTypeT> Stream<ItemType> for First<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<ItemType> + 'node> {
        Box::new(FirstIter { source: self.source.iter(), count_rem: self.count.clone() })
    }

    fn length(&self) -> Length {
        Length::intersection(self.source.length(), Length::Exact(self.count.to_owned()))
    }
}

impl<ItemType: ItemTypeT> Describe for First<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec, env)
    }
}

impl<ItemType: ItemTypeT> Iterator for FirstIter<'_, ItemType> {
    type Item = Result<ItemType, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem.dec();
            self.source.next()
        } else {
            None
        }
    }
}

impl<ItemType: ItemTypeT> SIterator<ItemType> for FirstIter<'_, ItemType> {
    fn len_remain(&self) -> Length {
        Length::intersection(self.source.len_remain(), Length::Exact(self.count_rem.to_owned()))
    }

    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if n > self.count_rem {
            n -= &self.count_rem;
            self.count_rem = UNumber::zero();
            Ok(Some(n))
        } else {
            self.count_rem -= &n;
            self.source.skip_n(n)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_first() {
        use crate::parser::parse;

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
        test_skip_n("seq.first(10^10)");
        test_skip_n("range(10^9).first(10^10)");
        test_skip_n("range(10^11).first(10^10)");
        test_describe!("(1..3).first" => "1");
        test_describe!("(1..3).first(4)" => "(1..3).first(4)");
        test_describe!("(1..3).take(4)" => "(1..3).take(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("first", eval_first);
    keywords.insert("take", eval_first);
}
