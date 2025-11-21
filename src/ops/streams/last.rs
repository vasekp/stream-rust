use crate::base::*;
use crate::utils::unsign;

use std::collections::VecDeque;

fn eval_last(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::Zero, .. }
            => eval_last_item(&**stm).map_err(|err| StreamError::new(err, rnode)),
        RNodeS { source: Item::String(stm), args: RArgs::Zero, .. }
            => eval_last_item(&**stm).map(Item::Char).map_err(|err| StreamError::new(err, rnode)),
        RNodeS { source: Item::Stream(_), args: RArgs::One(Item::Number(count)), .. } if count.is_zero()
            => Ok(Item::empty_stream()),
        RNodeS { source: Item::String(_), args: RArgs::One(Item::Number(count)), .. } if count.is_zero()
            => Ok(Item::empty_string()),
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Item::Number(count)), .. }
                if !count.is_negative()
            => {
                let count = unsign(count.to_owned());
                match eval_last_count(&**stm, &count) {
                    Ok(RetType::Listed(vec)) => Ok(Item::new_stream(List::from(vec))),
                    Ok(RetType::PassThrough) => Ok(rnode.source),
                    Ok(RetType::Skip(skip)) => {
                        let Item::Stream(stm) = rnode.source else { unreachable!() };
                        Ok(Item::new_stream(Last {
                            head: rnode.head,
                            source: stm.into(),
                            skip,
                            count
                        }))
                    },
                    Err(err) => Err(StreamError::new(err, rnode))
                }
            },
        RNodeS { source: Item::String(stm), args: RArgs::One(Item::Number(count)), .. }
                if !count.is_negative()
            => {
                let count = unsign(count.to_owned());
                match eval_last_count(&**stm, &count) {
                    Ok(RetType::Listed(vec)) => Ok(Item::new_string(LiteralString::from(vec))),
                    Ok(RetType::PassThrough) => Ok(rnode.source),
                    Ok(RetType::Skip(skip)) => {
                        let Item::String(stm) = rnode.source else { unreachable!() };
                        Ok(Item::new_string(Last {
                            head: rnode.head,
                            source: stm.into(),
                            skip,
                            count
                        }))
                    },
                    Err(err) => Err(StreamError::new(err, rnode))
                }
            },
        _ => Err(StreamError::new("expected: source.last or source.last(count)", rnode))
    }
}

fn eval_last_item<I: ItemType>(stm: &dyn Stream<I>) -> Result<I, BaseError> {
    match stm.len() {
        Length::Exact(len) if !len.is_zero() => {
            let mut it = stm.iter();
            it.advance(len - 1u32)?;
            it.next()
                .expect("1 item should remain after skip(len - 1)")
                .map_err(BaseError::from)
        },
        Length::Infinite => Err("stream is infinite".into()),
        _ => {
            let mut iter = stm.iter();
            let mut last = match iter.next() {
                Some(Ok(item)) => item,
                Some(Err(err)) => return Err(err.into()),
                None => return Err("stream is empty".into())
            };
            for res in iter {
                check_stop!();
                last = res?;
            }
            Ok(last)
        }
    }
}

fn eval_last_count<I: ItemType>(stm: &dyn Stream<I>, count: &UNumber) -> Result<RetType<I>, BaseError> {
    match stm.len() {
        Length::Exact(len) if &len < count => Ok(RetType::PassThrough),
        Length::Exact(len) => Ok(RetType::Skip(len - count)),
        Length::Infinite => Err("stream is infinite".into()),
        _ => {
            let size = match count.to_usize() {
                Some(size) => size,
                None => return Err("length too large".into())
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
            Ok(RetType::Listed(Vec::from(vec)))
        }
    }
}

enum RetType<I> {
    Listed(Vec<I>),
    PassThrough,
    Skip(UNumber)
}

#[derive(Clone)]
struct Last<I: ItemType> {
    head: Head,
    source: BoxedStream<I>,
    count: UNumber,
    skip: UNumber
}

impl<I: ItemType> Stream<I> for Last<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        let mut it = self.source.iter();
        match it.advance(self.skip.to_owned()) {
            Ok(None) => it,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => Box::new(std::iter::once(Err(err)))
        }
    }

    fn len(&self) -> Length {
        Length::Exact(self.count.to_owned())
    }
}

impl<I: ItemType> Describe for Last<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.count], prec, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_last() {
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
        test_advance("range(10^9).last(10^10)");
        test_advance("range(10^11).last(10^10)");
        test_describe!("(1..3).last" => "3");
        test_describe!("(1..3).last(4)" => "1..3");
        test_describe!("(1..3).last(2)" => "(1..3).last(2)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("last", eval_last);
}
