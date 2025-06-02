use crate::base::*;
use crate::utils::unsign;

#[derive(Clone)]
struct Skip {
    head: Head,
    source: BoxedStream,
    count: Option<UNumber>
}

impl Skip {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        let is_string = rnode.source.is_string();
        match rnode {
            RNodeS { head, source: Item::Stream(s) | Item::String(s), args: RArgs::Zero }
                => Ok(Item::new_stream_or_string(Skip {
                    head,
                    source: s.into(),
                    count: None
                }, is_string)),
            RNodeS { head, source: Item::Stream(s) | Item::String(s), args: RArgs::One(Item::Number(count)) }
                    if !count.is_negative()
                => Ok(Item::new_stream_or_string(Skip {
                    head,
                    source: s.into(),
                    count: Some(unsign(count))
                }, is_string)),
            _ => Err(StreamError::new("expected: source.skip or source.skip(count)", rnode))
        }
    }
}

impl Stream for Skip {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let mut iter = self.source.iter();
        match iter.skip_n(self.count.as_ref().cloned().unwrap_or_else(UNumber::one)) {
            Ok(None) => iter,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => Box::new(std::iter::once(Err(err)))
        }
    }

    fn length(&self) -> Length {
        self.source.length()
            .map(|x| match &self.count {
                Some(count) => x.checked_sub(count).unwrap_or_default(),
                None => if x.is_zero() { x.to_owned() } else { x - 1u32 }
            })
    }
}

impl Describe for Skip {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), &self.count, prec, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip() {
        use crate::parser::parse;

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
        test_skip_n("seq.skip(100)");
        test_skip_n("(1..10^10).skip(10^9)");
        test_describe!("(1..3).skip(0)" => "(1..3).skip(0)");
        test_describe!("(1..3).skip(4)" => "(1..3).skip(4)");
        test_describe!("\"abc\".skip" => "\"abc\".skip");
        test_describe!("\"abc\".skip(4)" => "\"abc\".skip(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("skip", Skip::eval);
}
