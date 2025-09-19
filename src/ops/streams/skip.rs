use crate::base::*;
use crate::utils::unsign;

fn eval_skip(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero }
            => Ok(Item::new_stream(Skip{head, source: stm.into(), count: None })),
        RNodeS { head, source: Item::String(stm), args: RArgs::Zero }
            => Ok(Item::new_string(Skip{head, source: stm.into(), count: None })),
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Item::Number(count)) }
                if !count.is_negative()
            => Ok(Item::new_stream(Skip{head, source: stm.into(), count: Some(unsign(count))})),
        RNodeS { head, source: Item::String(stm), args: RArgs::One(Item::Number(count)) }
                if !count.is_negative()
            => Ok(Item::new_string(Skip{head, source: stm.into(), count: Some(unsign(count))})),
        _ => Err(StreamError::new("expected: source.skip or source.skip(count)", rnode))
    }
}

#[derive(Clone)]
struct Skip<I: ItemType> {
    head: Head,
    source: BoxedStream<I>,
    count: Option<UNumber>
}

impl<I: ItemType> Stream<I> for Skip<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        let mut iter = self.source.iter();
        match iter.advance(self.count.as_ref().cloned().unwrap_or_else(UNumber::one)) {
            Ok(None) => iter,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => Box::new(std::iter::once(Err(err)))
        }
    }

    fn len(&self) -> Length {
        self.source.len()
            .map(|x| match &self.count {
                Some(count) => x.checked_sub(count).unwrap_or_default(),
                None => if x.is_zero() { x.to_owned() } else { x - 1u32 }
            })
    }
}

impl<I: ItemType> Describe for Skip<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), &self.count, prec, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip() {
        test_eval!("(1..3).skip(0)" => "[1, 2, 3]");
        test_eval!("(1..3).skip(1)" => "[2, 3]");
        test_eval!("(1..3).skip(2)" => "[3]");
        test_eval!("(1..3).skip(3)" => "[]");
        test_eval!("(1..3).skip(4)" => "[]");
        test_eval!("(1..3).skip(5)" => "[]");
        test_eval!("(1..3).skip" => "[2, 3]");
        test_eval!("\"abc\".skip" => "\"bc\"");
        test_eval!("\"abc\".skip(3)" => "\"\"");
        test_eval!("(1..3).skip(-1)" => err);
        test_eval!("seq.skip(10^10)" => "[10000000001, 10000000002, 10000000003, 10000000004, 10000000005, ...]");
        test_eval!("seq.skip(10^10).skip(10^10)" => "[20000000001, 20000000002, 20000000003, 20000000004, 20000000005, ...]");
        test_len!("(1..3).skip(0)" => 3);
        test_len!("(1..3).skip(1)" => 2);
        test_len!("(1..3).skip(2)" => 1);
        test_len!("(1..3).skip(3)" => 0);
        test_len!("(1..3).skip(4)" => 0);
        test_len!("(1..3).skip(5)" => 0);
        test_advance("seq.skip(100)");
        test_advance("(1..10^10).skip(10^9)");
        test_describe!("(1..3).skip(0)" => "(1..3).skip(0)");
        test_describe!("(1..3).skip(4)" => "(1..3).skip(4)");
        test_describe!("\"abc\".skip" => "\"abc\".skip");
        test_describe!("\"abc\".skip(4)" => "\"abc\".skip(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("skip", eval_skip);
}
