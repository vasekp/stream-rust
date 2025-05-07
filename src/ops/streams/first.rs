use crate::base::*;
use crate::utils::unsign;

#[derive(Clone)]
struct First {
    head: Head,
    source: BoxedStream,
    count: UNumber
}

struct FirstIter<'node> {
    source: Box<dyn SIterator + 'node>,
    count_rem: UNumber
}

impl First {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode {
            RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, .. } => {
                let mut it = stm.iter();
                match it.next() {
                    Some(result) => result,
                    None => {
                        drop(it);
                        Err(StreamError::new("stream is empty", rnode))
                    }
                }
            },
            RNodeS { head, source: Item::Stream(s), args: RArgs::One(Item::Number(count)) }
                    if !count.is_negative()
                => Ok(Item::Stream(Box::new(First {
                    head,
                    source: s.into(),
                    count: unsign(count)
                }))),
            _ => Err(StreamError::new("expected: source.first or source.first(count)", rnode))
        }
    }
}

impl Stream for First {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(FirstIter { source: self.source.iter(), count_rem: self.count.clone() })
    }

    fn length(&self) -> Length {
        Length::intersection(self.source.length(), Length::Exact(self.count.to_owned()))
    }

    fn is_string(&self) -> TriState {
        self.source.is_string()
    }
}

impl Describe for First {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec)
    }
}

impl Iterator for FirstIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem.dec();
            self.source.next()
        } else {
            None
        }
    }
}

impl SIterator for FirstIter<'_> {
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

        assert_eq!(parse("(1..3).first").unwrap().eval_default().unwrap().to_string(), "1");
        assert_eq!(parse("(1..3).first(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).first(1)").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse("(1..3).first(2)").unwrap().eval_default().unwrap().to_string(), "[1, 2]");
        assert_eq!(parse("(1..3).first(3)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("(1..3).first(4)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("\"abc\".first").unwrap().eval_default().unwrap().to_string(), "'a'");
        assert_eq!(parse("\"abc\".first(0)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"abc\".first(1)").unwrap().eval_default().unwrap().to_string(), "\"a\"");
        assert_eq!(parse("\"abc\".first(4)").unwrap().eval_default().unwrap().to_string(), "\"abc\"");
        assert_eq!(parse("seq.first(10^10).len").unwrap().eval_default().unwrap().to_string(), "10000000000");
        assert_eq!(parse("range(10^9).first(10^10).len").unwrap().eval_default().unwrap().to_string(), "1000000000");
        assert_eq!(parse("range(10^11).first(10^10).len").unwrap().eval_default().unwrap().to_string(), "10000000000");
        test_len_exact(&parse("(1..3).first(0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("(1..3).first(1)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("(1..3).first(2)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("(1..3).first(3)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("(1..3).first(4)").unwrap().eval_default().unwrap(), 3);
        test_skip_n(&parse("seq.first(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^9).first(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^11).first(10^10)").unwrap().eval_default().unwrap());
        assert_eq!(parse("(1..3).first").unwrap().eval_default().unwrap().describe(), "1");
        assert_eq!(parse("(1..3).first(4)").unwrap().eval_default().unwrap().describe(), "(1..3).first(4)");
        assert_eq!(parse("(1..3).take(4)").unwrap().eval_default().unwrap().describe(), "(1..3).take(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("first", First::eval);
    keywords.insert("take", First::eval);
}
