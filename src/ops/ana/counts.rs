use crate::base::*;

use std::collections::VecDeque;

fn eval_counts(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    match &node.source {
        Some(Item::Stream(stm)) => {
            let mut counts = node.args.iter().map(|item| (item, 0)).collect::<Vec<_>>();
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
        },
        Some(Item::String(stm)) => {
            let mut counts = try_with!(node, node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok((vec![ch.clone()], 0)),
                    Item::String(s) if !s.is_empty() => Ok((s.listout()?, 0)),
                    item => Err(BaseError::from(format!("expected character or nonempty string, found {:?}", item)))
                })
                .collect::<Result<Vec<_>, _>>()?);
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
                        deque.range(longest - len..).eq(cmp)
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
        },
        _ => Err(StreamError::new("expected: stream.counts(item...)", node))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_counts() {
        use super::*;
        use crate::parser::parse;
        test_eval!("[1,2,3].repeat(5).count(2)" => "5");
        test_eval!("[1,2,3].repeat(5).counts(2,2,4)" => "[5, 5, 0]");
        test_eval!("\"This is a test string\".counts('i', \" \", \"is\", \"st\", 'st')" => "[3, 4, 2, 2, 0]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("counts", eval_counts);
    keywords.insert("count", eval_counts);
    keywords.insert("tally", eval_counts);
    keywords.insert("freq", eval_counts);
}
