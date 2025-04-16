use crate::base::*;
use crate::utils::unsign;

use std::collections::VecDeque;

#[derive(Clone)]
struct Last {
    head: Head,
    source: BoxedStream,
    count: UNumber
}

impl Last {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode {
            RNodeS { source: Item::Stream(ref stm), .. }
                    if stm.length() == Length::Infinite
                => Err(StreamError::new("stream is infinite", rnode)),
            RNodeS { source: Item::Stream(ref stm), args: RArgs::Zero, .. } => {
                match stm.length() {
                    Length::Exact(len) if !len.is_zero() => {
                        let mut it = stm.iter();
                        it.skip_n(len - 1u32)?;
                        it.next().expect("1 item should remain after skip(len - 1)")
                    },
                    Length::Infinite => Err(StreamError::new("stream is infinite", rnode)),
                    _ => stm.iter()
                            .last()
                            .unwrap_or_else(|| Err(StreamError::new("stream is empty", rnode)))
                }
            },
            RNodeS { source: Item::Stream(ref stm), args: RArgs::One(Item::Number(ref count)), .. } if count.is_zero()
                => Ok(Item::Stream(Box::new(EmptyStream::cond_string(stm.is_string())))),
            RNodeS { head, source: Item::Stream(s), args: RArgs::One(Item::Number(count)) }
                    if !count.is_negative()
                => Ok(Item::Stream(Box::new(Last {
                    head,
                    source: s.into(),
                    count: unsign(count)
                }))),
            _ => Err(StreamError::new("expected: source.last or source.last(count)", rnode))
        }
    }
}

impl Last {
    fn aux_node(&self) -> ENode {
        ENode {
            head: self.head.clone(),
            source: Some(Item::Stream(self.source.clone().into())),
            args: vec![Item::Number(self.count.clone().into())]
        }
    }
}

impl Stream for Last {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match self.source.length() {
            Length::Exact(len) => {
                let mut it = self.source.iter();
                match it.skip_n(len.checked_sub(&self.count).unwrap_or_default()) {
                    Ok(None) => it,
                    Ok(Some(_)) => Box::new(std::iter::empty()),
                    Err(err) => Box::new(std::iter::once(Err(err)))
                }
            },
            _ => {
                let size = match self.count.to_usize() {
                    Some(size) => size,
                    None => return Box::new(std::iter::once(Err(StreamError::new("length too large", self.aux_node()))))
                };
                let mut vec = VecDeque::with_capacity(size);
                for res in self.source.iter() {
                    let item = match res {
                        Ok(item) => item,
                        Err(err) => return Box::new(std::iter::once(Err(err)))
                    };
                    if vec.len() == size {
                        vec.pop_front();
                    }
                    vec.push_back(item);
                }
                Box::new(vec.into_iter().map(Result::Ok))
            }
        }
    }

    fn length(&self) -> Length {
        Length::intersection(self.source.length(), Length::Exact(self.count.to_owned()))
    }

    fn is_string(&self) -> TriState {
        self.source.is_string()
    }
}

impl Describe for Last {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_last() {
        use crate::parser::parse;

        assert_eq!(parse("(1..3).last").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("(1..3).last(0)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).last(1)").unwrap().eval().unwrap().to_string(), "[3]");
        assert_eq!(parse("(1..3).last(2)").unwrap().eval().unwrap().to_string(), "[2, 3]");
        assert_eq!(parse("(1..3).last(3)").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("(1..3).last(4)").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("\"abc\".last").unwrap().eval().unwrap().to_string(), "'c'");
        assert_eq!(parse("\"abc\".last(0)").unwrap().eval().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"abc\".last(1)").unwrap().eval().unwrap().to_string(), "\"c\"");
        assert_eq!(parse("\"abc\".last(4)").unwrap().eval().unwrap().to_string(), "\"abc\"");
        assert!(parse("[].lenUF.last").unwrap().eval().is_err());
        assert_eq!(parse("(1..10).lenUF.last").unwrap().eval().unwrap().to_string(), "10");
        assert_eq!(parse("[].lenUF.last(3)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("[1,2].lenUF.last(3)").unwrap().eval().unwrap().to_string(), "[1, 2]");
        assert_eq!(parse("(1..10).lenUF.last(3)").unwrap().eval().unwrap().to_string(), "[8, 9, 10]");
        assert_eq!(parse("\"ab\".lenUF.last(3)").unwrap().eval().unwrap().to_string(), "\"ab\"");
        assert_eq!(parse("\"abcde\".lenUF.last(3)").unwrap().eval().unwrap().to_string(), "\"cde\"");
        assert_eq!(parse("\"abcde\".lenUF.last(1)").unwrap().eval().unwrap().to_string(), "\"e\"");
        assert_eq!(parse("\"abcde\".lenUF.last(0)").unwrap().eval().unwrap().to_string(), "\"\"");
        assert!(parse("seq.last(10^10)").unwrap().eval().is_err());
        assert_eq!(parse("range(10^9).last(10^10).len").unwrap().eval().unwrap().to_string(), "1000000000");
        assert_eq!(parse("range(10^11).last(10^10).len").unwrap().eval().unwrap().to_string(), "10000000000");
        assert_eq!(parse("range(10^10).last(10^9).first").unwrap().eval().unwrap().to_string(), "9000000001");
        test_len_exact(&parse("(1..3).last(0)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("(1..3).last(1)").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("(1..3).last(2)").unwrap().eval().unwrap(), 2);
        test_len_exact(&parse("(1..3).last(3)").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("(1..3).last(4)").unwrap().eval().unwrap(), 3);
        test_skip_n(&parse("range(10^9).last(10^10)").unwrap().eval().unwrap());
        test_skip_n(&parse("range(10^11).last(10^10)").unwrap().eval().unwrap());
        assert_eq!(parse("(1..3).last").unwrap().eval().unwrap().describe(), "3");
        assert_eq!(parse("(1..3).last(4)").unwrap().eval().unwrap().describe(), "(1..3).last(4)");
        assert_eq!(parse("(1..3).take(4)").unwrap().eval().unwrap().describe(), "(1..3).take(4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("last", Last::eval);
}
