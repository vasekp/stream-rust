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
            => Ok(Item::new_string(First{head, source: s.into(), count: unsign(count)})),
        _ => Err(StreamError::new("expected: source.first or source.first(count)", rnode))
    }
}

fn first_item_impl<I: ItemType>(stm: &dyn Stream<I>) -> Result<I, BaseError> {
    match stm.iter().next() {
        Some(result) => Ok(result?),
        None => Err("stream is empty".into())
    }
}

#[derive(Clone)]
struct First<I: ItemType> {
    head: Head,
    source: BoxedStream<I>,
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
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec, env)
    }
}

impl<I: ItemType> Iterator for FirstIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem.dec();
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
        test_advance("seq.first(10^10)");
        test_advance("range(10^9).first(10^10)");
        test_advance("range(10^11).first(10^10)");
        test_describe!("(1..3).first" => "1");
        test_describe!("(1..3).first(4)" => "(1..3).first(4)");
        test_describe!("(1..3).take(4)" => "(1..3).take(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("first", eval_first);
    keywords.insert("take", eval_first);
}
