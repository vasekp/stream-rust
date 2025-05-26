use crate::base::*;
use crate::utils::unsign;

use std::collections::VecDeque;

#[derive(Clone)]
struct Last {
    head: Head,
    source: BoxedStream,
    count: UNumber,
    skip: UNumber
}

impl Last {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        match &rnode {
            RNodeS { source: Item::Stream(stm) | Item::String(stm), .. }
                    if stm.length() == Length::Infinite
                => Err(StreamError::new(format!("{} is infinite", rnode.source.type_str()), rnode)),
            RNodeS { source: Item::Stream(stm) | Item::String(stm), args: RArgs::Zero, .. } => {
                match stm.length() {
                    Length::Exact(len) if !len.is_zero() => {
                        let mut it = stm.iter();
                        it.skip_n(len - 1u32)?;
                        it.next().expect("1 item should remain after skip(len - 1)")
                    },
                    Length::Infinite => Err(StreamError::new(format!("{} is infinite", rnode.source.type_str()), rnode)),
                    _ => {
                        let mut iter = stm.iter();
                        let mut last = match iter.next() {
                            Some(Ok(item)) => item,
                            Some(Err(err)) => return Err(err),
                            None => {
                                drop(iter);
                                return Err(StreamError::new(format!("{} is empty", rnode.source.type_str()), rnode))
                            }
                        };
                        for res in iter {
                            check_stop!();
                            last = res?;
                        }
                        Ok(last)
                    }
                }
            },
            RNodeS { source: Item::Stream(_) | Item::String(_), args: RArgs::One(Item::Number(count)), .. } if count.is_zero()
                => Ok(Item::empty_stream_or_string(rnode.source.is_string())),
            RNodeS { source: Item::Stream(stm) | Item::String(stm), args: RArgs::One(Item::Number(count)), .. }
                    if !count.is_negative()
                => {
                    let count = unsign(count.to_owned());
                    match stm.length() {
                        Length::Exact(len) if len < count => Ok(rnode.source),
                        Length::Exact(len) => {
                            let (stm, is_string) = match rnode.source {
                                Item::Stream(stm) => (stm, false),
                                Item::String(stm) => (stm, true),
                                _ => unreachable!()
                            };
                            Ok(Item::new_stream_or_string(Last {
                                head: rnode.head,
                                source: stm.into(),
                                skip: len - &count,
                                count
                            }, is_string))
                        },
                        Length::Infinite => Err(StreamError::new("stream is infinite", rnode)),
                        _ => {
                            let size = match count.to_usize() {
                                Some(size) => size,
                                None => return Err(StreamError::new("length too large", rnode))
                            };
                            let mut vec = VecDeque::with_capacity(size);
                            for res in stm.iter() {
                                check_stop!();
                                let item = res?;
                                if vec.len() == size {
                                    vec.pop_front();
                                }
                                vec.push_back(item);
                            }
                            let is_string = rnode.source.is_string();
                            Ok(Item::new_stream_or_string(List::from(Vec::from(vec)), is_string))
                        }
                    }
                },
            _ => Err(StreamError::new("expected: source.last or source.last(count)", rnode))
        }
    }
}

impl Stream for Last {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let mut it = self.source.iter();
        match it.skip_n(self.skip.to_owned()) {
            Ok(None) => it,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => Box::new(std::iter::once(Err(err)))
        }
    }

    fn length(&self) -> Length {
        Length::Exact(self.count.to_owned())
    }
}

impl Describe for Last {
    fn describe_inner(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_last() {
        use crate::parser::parse;

        assert_eq!(parse("(1..3).last").unwrap().eval_default().unwrap().to_string(), "3");
        assert_eq!(parse("(1..3).last(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("(1..3).last(1)").unwrap().eval_default().unwrap().to_string(), "[3]");
        assert_eq!(parse("(1..3).last(2)").unwrap().eval_default().unwrap().to_string(), "[2, 3]");
        assert_eq!(parse("(1..3).last(3)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("(1..3).last(4)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("\"abc\".last").unwrap().eval_default().unwrap().to_string(), "'c'");
        assert_eq!(parse("\"abc\".last(0)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"abc\".last(1)").unwrap().eval_default().unwrap().to_string(), "\"c\"");
        assert_eq!(parse("\"abc\".last(4)").unwrap().eval_default().unwrap().to_string(), "\"abc\"");
        assert!(parse("[].lenUF.last").unwrap().eval_default().is_err());
        assert_eq!(parse("(1..10).lenUF.last").unwrap().eval_default().unwrap().to_string(), "10");
        assert_eq!(parse("[].lenUF.last(3)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("[1,2].lenUF.last(3)").unwrap().eval_default().unwrap().to_string(), "[1, 2]");
        assert_eq!(parse("(1..10).lenUF.last(3)").unwrap().eval_default().unwrap().to_string(), "[8, 9, 10]");
        assert_eq!(parse("\"ab\".lenUF.last(3)").unwrap().eval_default().unwrap().to_string(), "\"ab\"");
        assert_eq!(parse("\"abcde\".lenUF.last(3)").unwrap().eval_default().unwrap().to_string(), "\"cde\"");
        assert_eq!(parse("\"abcde\".lenUF.last(1)").unwrap().eval_default().unwrap().to_string(), "\"e\"");
        assert_eq!(parse("\"abcde\".lenUF.last(0)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert!(parse("seq.last(10^10)").unwrap().eval_default().is_err());
        assert_eq!(parse("range(10^9).last(10^10).len").unwrap().eval_default().unwrap().to_string(), "1000000000");
        assert_eq!(parse("range(10^11).last(10^10).len").unwrap().eval_default().unwrap().to_string(), "10000000000");
        assert_eq!(parse("range(10^10).last(10^9).first").unwrap().eval_default().unwrap().to_string(), "9000000001");
        test_len_exact(&parse("(1..3).last(0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("(1..3).last(1)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("(1..3).last(2)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("(1..3).last(3)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("(1..3).last(4)").unwrap().eval_default().unwrap(), 3);
        test_skip_n(&parse("range(10^9).last(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^11).last(10^10)").unwrap().eval_default().unwrap());
        assert_eq!(parse("(1..3).last").unwrap().eval_default().unwrap().describe(), "3");
        assert_eq!(parse("(1..3).last(4)").unwrap().eval_default().unwrap().describe(), "1..3");
        assert_eq!(parse("(1..3).last(2)").unwrap().eval_default().unwrap().describe(), "(1..3).last(2)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("last", Last::eval);
}
