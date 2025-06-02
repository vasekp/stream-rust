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
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
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
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_last() {
        use crate::parser::parse;

        test_eval!("(1..3).last" => "3");
        test_eval!("(1..3).last(0)" => "[]");
        test_eval!("(1..3).last(1)" => "[3]");
        test_eval!("(1..3).last(2)" => "[2, 3]");
        test_eval!("(1..3).last(3)" => "[1, 2, 3]");
        test_eval!("(1..3).last(4)" => "[1, 2, 3]");
        test_eval!("\"abc\".last" => "'c'");
        test_eval!("\"abc\".last(0)" => "\"\"");
        test_eval!("\"abc\".last(1)" => "\"c\"");
        test_eval!("\"abc\".last(4)" => "\"abc\"");
        test_eval!("[].lenUF.last" => err);
        test_eval!("(1..10).lenUF.last" => "10");
        test_eval!("[].lenUF.last(3)" => "[]");
        test_eval!("[1,2].lenUF.last(3)" => "[1, 2]");
        test_eval!("(1..10).lenUF.last(3)" => "[8, 9, 10]");
        test_eval!("\"ab\".lenUF.last(3)" => "\"ab\"");
        test_eval!("\"abcde\".lenUF.last(3)" => "\"cde\"");
        test_eval!("\"abcde\".lenUF.last(1)" => "\"e\"");
        test_eval!("\"abcde\".lenUF.last(0)" => "\"\"");
        test_eval!("seq.last(10^10)" => err);
        test_eval!("range(10^9).last(10^10).len" => "1000000000");
        test_eval!("range(10^11).last(10^10).len" => "10000000000");
        test_eval!("range(10^10).last(10^9).first" => "9000000001");
        test_len!("(1..3).last(0)" => 0);
        test_len!("(1..3).last(1)" => 1);
        test_len!("(1..3).last(2)" => 2);
        test_len!("(1..3).last(3)" => 3);
        test_len!("(1..3).last(4)" => 3);
        test_skip_n("range(10^9).last(10^10)");
        test_skip_n("range(10^11).last(10^10)");
        test_describe!("(1..3).last" => "3");
        test_describe!("(1..3).last(4)" => "1..3");
        test_describe!("(1..3).last(2)" => "(1..3).last(2)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("last", Last::eval);
}
