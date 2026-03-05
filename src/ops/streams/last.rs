use crate::base::*;

use std::collections::VecDeque;

fn eval_last(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let count: Option<UNumber> = match &node.args[..] {
        [] => None,
        [Item::Number(count)] => Some(count.try_into().map_err(|_| StreamError::new0("count can't be negative"))?),
        _ => return Err(StreamError::new0("expected: source.last or source.last(count)"))
    };
    match (node.source_checked()?, count) {
        (Item::Stream(stm), None) => eval_last_item(&**stm),
        (Item::Stream(stm), Some(count)) => eval_last_count(&node.head, stm, count),
        (Item::String(stm), None) => eval_last_item(&**stm).map(Item::Char),
        (Item::String(stm), Some(count)) => eval_last_count(&node.head, stm, count),
        _ => Err(StreamError::new0("expected: source.first or source.last(count)"))
    }
}

fn eval_last_item<I: ItemType>(stm: &dyn Stream<I>) -> Result<I, StreamError> {
    match stm.len() {
        Length::Exact(len) if !len.is_zero() => {
            let mut it = stm.iter();
            it.advance(len - 1u32)?;
            it.next().expect("1 item should remain after skip(len - 1)")
        },
        Length::Infinite => Err(StreamError::new0("stream is infinite")),
        _ => {
            let mut iter = stm.iter();
            let mut last = match iter.next() {
                Some(Ok(item)) => item,
                Some(Err(err)) => return Err(err),
                None => return Err(StreamError::new0("stream is empty"))
            };
            for res in iter {
                check_stop!();
                last = res?;
            }
            Ok(last)
        }
    }
}

fn eval_last_count<I: ItemType>(head: &Head, stm: &Rc<dyn Stream<I>>, count: UNumber) -> Result<Item, StreamError> {
    if count.is_zero() {
        return Ok(I::from_vec(vec![]));
    }
    match stm.len() {
        Length::Exact(len) if len < count => Ok(stm.into()),
        Length::Exact(len) => {
            Ok(Item::from(Rc::new(Last {
                head: head.clone(),
                source: Rc::clone(stm),
                skip: len - &count,
                count
            }) as Rc<dyn Stream<I>>))
        },
        Length::Infinite => Err(StreamError::new0("stream is infinite")),
        _ => {
            let size = match count.try_into() {
                Ok(size) => size,
                Err(_) => return Err(StreamError::new0("length too large"))
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
            Ok(I::from_vec(Vec::from(vec)))
        }
    }
}

struct Last<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
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
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.count)
            .finish(prec)
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
        test_eval!("[].$lenUF.last" => err);
        test_eval!("(1..10).$lenUF.last" => "10");
        test_eval!("[].$lenUF.last(3)" => "[]");
        test_eval!("[1,2].$lenUF.last(3)" => "[1, 2]");
        test_eval!("(1..10).$lenUF.last(3)" => "[8, 9, 10]");
        test_eval!("\"ab\".$lenUF.last(3)" => "\"ab\"");
        test_eval!("\"abcde\".$lenUF.last(3)" => "\"cde\"");
        test_eval!("\"abcde\".$lenUF.last(1)" => "\"e\"");
        test_eval!("\"abcde\".$lenUF.last(0)" => "\"\"");
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
    symbols.insert("last", eval_last, r#"
Up to the last `count` items of `stream`, or up to the last `count` characters of `string`.
If `count` is not given, only gives the last item or the last character (no stream / string).
= stream.?
= stream.?(count)
= string.?
= string.?(count)
> [1, 2, 3].? => 3
> [1, 2, 3].?(2) => [2, 3]
> ?seq.? => !stream is infinite
> "abc".? => 'c'
> "abc".?(2) => "bc"
: first
"#);
}
