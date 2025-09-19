use crate::base::*;

use std::collections::VecDeque;

fn eval_contains(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    match &node.source {
        Some(Item::Stream(stm)) => {
            let mut queries = node.args.into_iter()
                .map(Query::Pending)
                .collect::<Vec<_>>();
            let mut rem = queries.len();
            'a: for elm in stm.iter() {
                check_stop!();
                let elm = elm?;
                for query in &mut queries {
                    if let Query::Pending(item) = query {
                        if elm.try_eq(item)? {
                            *query = Query::Found;
                            rem -= 1;
                        }
                    }
                    if rem == 0 { break 'a; }
                }
            }
            let mut res = queries.into_iter()
                .map(|q| Item::Bool(matches!(q, Query::Found)))
                .collect::<Vec<_>>();
            if res.len() == 1 {
                Ok(res.pop().unwrap()) // len == 1
            } else {
                Ok(Item::from(res))
            }
        },
        Some(Item::String(stm)) => {
            let mut queries = try_with!(node, node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok(Query::Pending(vec![ch.clone()])),
                    Item::String(s) if !s.is_empty() => Ok(Query::Pending(s.listout()?)),
                    item => Err(BaseError::from(format!("expected character or nonempty string, found {:?}", item)))
                })
                .collect::<Result<Vec<_>, _>>()?);
            let longest = queries.iter()
                .map(|q| match q { Query::Pending(vec) => vec.len(), _ => unreachable!() })
                .reduce(std::cmp::max)
                .unwrap(); // len ≥ 1
            let mut rem = queries.len();
            let mut deque = VecDeque::with_capacity(longest);
            'a: for elm in stm.iter() {
                check_stop!();
                if deque.len() == longest {
                    deque.pop_front();
                }
                deque.push_back(elm?);
                for query in &mut queries {
                    if let Query::Pending(item) = query {
                        let len = item.len();
                        let found = if len == 1 {
                            deque.back().unwrap() == &item[0]
                        } else {
                            deque.range(longest - len..).eq(item)
                        };
                        if found {
                            *query = Query::Found;
                            rem -= 1;
                        }
                    }
                    if rem == 0 { break 'a; }
                }
            }
            let mut res = queries.into_iter()
                .map(|q| Item::Bool(matches!(q, Query::Found)))
                .collect::<Vec<_>>();
            if res.len() == 1 {
                Ok(res.pop().unwrap())
            } else {
                Ok(Item::from(res))
            }
        },
        _ => Err(StreamError::new("expected: stream.contains(item...) or string.contains(char or string...)", node))
    }
}

enum Query<I> {
    Pending(I),
    Found
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_contains() {
        use super::*;

        test_eval!("seq.contains(5)" => "true");
        test_eval!("(1..5).contains('a')" => "false");
        test_eval!("[].contains([])" => "false");
        test_eval!("\"abc\".contains('b')" => "true");
        test_eval!("\"abc\".contains(\"\")" => err);
        test_eval!("\"abc\".contains(1)" => err);
        test_eval!("\"abc\".contains(\"bc\")" => "true");
        test_eval!("\"abc\".contains(\"abc\")" => "true");
        test_eval!("\"abc\".contains(\"abcd\")" => "false");
        test_eval!("\"abc\".contains(\"zabc\")" => "false");
        test_eval!("\"abcdefghijklmnopqrstuvwxyz\".repeat().contains(\"yza\")" => "true");
        test_eval!("(1..5).repeat(2).skip(1).contains(1,5,7)" => "[true, true, false]");
        test_eval!("\"abcde\".contains('b',\"b\",\"cd\",\"de\",\"ef\")" => "[true, true, true, true, false]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("contains", eval_contains);
}
