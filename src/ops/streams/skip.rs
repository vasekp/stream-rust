use crate::base::*;
use crate::utils::{TriState, unsign};

#[derive(Clone)]
struct Skip {
    head: Head,
    source: BoxedStream,
    count: Option<UNumber>
}

impl Skip {
    fn eval(node: Node, env: &std::rc::Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        let count = match &mut node.args[..] {
            [Item::Number(ref mut count)] => {
                if count.is_negative() {
                    return Err(StreamError::new("expected nonnegative count", node));
                }
                Some(count)
            },
            [] => None,
            _ => return Err(StreamError::new("expected: source.skip() or source.skip(count)", node))
        };
        match node.source {
            Some(Item::Stream(s)) =>
                Ok(Item::Stream(Box::new(Skip {
                    head: node.head,
                    source: s.into(),
                    count: count.map(|c| unsign(std::mem::take(c)))
                }))),
            _ => Err(StreamError::new("expected: source.skip(count)", node))
        }
    }
}

impl Stream for Skip {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let mut iter = self.source.iter();
        match iter.skip_n(self.count.as_ref().cloned().unwrap_or(UNumber::one())) {
            Ok(None) => iter,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => Box::new(std::iter::once(Err(err)))
        }
    }

    fn length(&self) -> Length {
        self.source.length()
            .map(|x| x.checked_sub(self.count.as_ref()
                    .unwrap_or(&UNumber::one()))
                .unwrap_or_default())
    }

    fn is_string(&self) -> TriState {
        self.source.is_string()
    }
}

impl Describe for Skip {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, Some(&self.source), &self.count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip() {
        use crate::parser::parse;

        assert_eq!(parse("(1..3).skip(0)").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("(1..3).skip(1)").unwrap().eval().unwrap().to_string(), "[2, 3]");
        assert_eq!(parse("(1..3).skip(2)").unwrap().eval().unwrap().to_string(), "[3]");
        assert_eq!(parse("(1..3).skip(3)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).skip(4)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).skip(5)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).skip").unwrap().eval().unwrap().to_string(), "[2, 3]");
        assert_eq!(parse("\"abc\".skip").unwrap().eval().unwrap().to_string(), "\"bc\"");
        assert_eq!(parse("\"abc\".skip(3)").unwrap().eval().unwrap().to_string(), "\"\"");
        assert!(parse("(1..3).skip(-1)").unwrap().eval().is_err());
        assert_eq!(parse("seq.skip(10^10)").unwrap().eval().unwrap().to_string(), "[10000000001, 10000000002, 10000000003, 10000000004, 10000000005, ...]");
        assert_eq!(parse("seq.skip(10^10).skip(10^10)").unwrap().eval().unwrap().to_string(), "[20000000001, 20000000002, 20000000003, 20000000004, 20000000005, ...]");
        test_len_exact(&parse("(1..3).skip(0)").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("(1..3).skip(1)").unwrap().eval().unwrap(), 2);
        test_len_exact(&parse("(1..3).skip(2)").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("(1..3).skip(3)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("(1..3).skip(4)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("(1..3).skip(5)").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("seq.skip(100)").unwrap().eval().unwrap());
        test_skip_n(&parse("(1..10^10).skip(10^9)").unwrap().eval().unwrap());
        assert_eq!(parse("(1..3).skip(0)").unwrap().eval().unwrap().describe(), "(1..3).skip(0)");
        assert_eq!(parse("(1..3).skip(4)").unwrap().eval().unwrap().describe(), "(1..3).skip(4)");
        assert_eq!(parse("\"abc\".skip").unwrap().eval().unwrap().describe(), "\"abc\".skip");
        assert_eq!(parse("\"abc\".skip(4)").unwrap().eval().unwrap().describe(), "\"abc\".skip(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("skip", Skip::eval);
}
