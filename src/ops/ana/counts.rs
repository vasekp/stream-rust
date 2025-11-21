use crate::base::*;

use std::collections::VecDeque;

fn eval_counts(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    match &node.source {
        Some(Item::Stream(stm)) if !node.args.is_empty() =>
            stream_counts_listed_impl(&**stm, &node.args),
        Some(Item::Stream(stm)) => counts_free_impl(&**stm),
        Some(Item::String(stm)) if !node.args.is_empty() => {
            let args = try_with!(node, node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok(vec![ch.clone()]),
                    Item::String(s) if !s.is_empty() => Ok(s.listout()?),
                    item => Err(BaseError::from(format!("expected character or nonempty string, found {:?}", item)))
                })
                .collect::<Result<Vec<_>, _>>()?);
            string_counts_listed_impl(&**stm, &args)
        },
        Some(Item::String(stm)) => counts_free_impl(&**stm),
        _ => Err(StreamError::new("expected: stream.counts(item...) or string.counts(char or string...) or source.counts", node))
    }
}

fn stream_counts_listed_impl(stm: &dyn Stream, args: &[Item]) -> Result<Item, StreamError> {
    let mut counts = args.iter().map(|item| (item, 0)).collect::<Vec<_>>();
    for item in stm.iter() {
        check_stop!();
        let item = item?;
        for (cmp, count) in &mut counts {
            if item.try_eq(cmp)? {
                count.inc();
            }
        }
    }
    let mut ret = counts.into_iter()
        .map(|(_, count)| Item::new_number(count))
        .collect::<Vec<_>>();
    if ret.len() == 1 {
        Ok(ret.pop().unwrap())
    } else {
        Ok(Item::from(ret))
    }
}

fn string_counts_listed_impl(stm: &dyn Stream<Char>, chars: &[Vec<Char>]) -> Result<Item, StreamError> 
{
    let mut counts = chars.iter().map(|arg| (arg, 0)).collect::<Vec<_>>();
    let longest = counts.iter()
        .map(|(s, _)| s.len())
        .reduce(std::cmp::max)
        .unwrap(); // len ≥ 1
    let mut deque = VecDeque::with_capacity(longest);
    for elm in stm.iter() {
        check_stop!();
        if deque.len() == longest {
            deque.pop_front();
        }
        deque.push_back(elm?);
        for (cmp, count) in &mut counts {
            let len = cmp.len();
            let found = if len == 1 {
                deque.back().unwrap() == &cmp[0]
            } else {
                deque.range(longest - len..).eq(cmp.iter())
            };
            if found {
                count.inc();
            }
        }
    }
    let mut res = counts.into_iter()
        .map(|(_, count)| Item::new_number(count))
        .collect::<Vec<_>>();
    if res.len() == 1 {
        Ok(res.pop().unwrap())
    } else {
        Ok(Item::from(res))
    }
}

fn counts_free_impl<I: ItemType>(stm: &dyn Stream<I>) -> Result<Item, StreamError> {
    let mut counts: Vec<(I, usize)> = Vec::new();
    for item in stm.iter() {
        check_stop!();
        let item = item?;
        'a: {
            for (cmp, count) in &mut counts {
                if item.try_eq(cmp)? {
                    count.inc();
                    break 'a;
                }
            }
            counts.push((item, 1));
        }
    }
    let mut ret = counts.into_iter()
        .map(|(item, count)| Item::from(vec![item.into(), Item::new_number(count)]))
        .collect::<Vec<_>>();
    if ret.len() == 1 {
        Ok(ret.pop().unwrap())
    } else {
        Ok(Item::from(ret))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_counts() {
        use super::*;
        test_eval!("[1,2,3].repeat(5).count(2)" => "5");
        test_eval!("[1,2,3].repeat(5).counts(2,2,4)" => "[5, 5, 0]");
        test_eval!("\"This is a test string\".counts('i', \" \", \"is\", \"st\", 'st')" => "[3, 4, 2, 2, 0]");
        test_eval!("pi.first(10).counts@range(0,9)" : 10 => "[0, 2, 1, 2, 1, 2, 1, 0, 0, 1]");
        test_eval!("pi.first(10).counts" : 21 => "[[3, 2], [1, 2], [4, 1], [5, 2], [9, 1], [2, 1], [6, 1]]");
        test_eval!("\"The quick brown fox jumps over the lazy dog.\".lcase.counts@alpha" : 26 => 
        "[1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 1, 1, 1, 1, 4, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1]");
        test_eval!("\"Hello world!\".counts" : 15 => "[['H', 1], ['e', 1], ['l', 3], ['o', 2], [' ', 1], ...]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("counts", eval_counts);
    symbols.insert("count", eval_counts);
    symbols.insert("tally", eval_counts);
    symbols.insert("freq", eval_counts);
}
