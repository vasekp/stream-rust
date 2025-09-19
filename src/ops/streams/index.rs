use crate::base::*;

use std::collections::VecDeque;

fn eval_index(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    match &node.source {
        Some(Item::Stream(stm)) => {
            let mut queries = node.args.into_iter()
                .map(Query::Pending)
                .collect::<Vec<_>>();
            let mut rem = queries.len();
            'a: for (ix, elm) in stm.iter().enumerate() {
                check_stop!();
                let elm = elm?;
                for query in &mut queries {
                    if let Query::Pending(item) = query {
                        if elm.try_eq(item)? {
                            *query = Query::Found(ix);
                            rem -= 1;
                        }
                    }
                    if rem == 0 { break 'a; }
                }
            }
            let mut res = queries.into_iter()
                .map(|q| match q {
                    Query::Found(ix) => Item::new_number(ix + 1),
                    Query::Pending(_) => Item::new_stream(EmptyStream)
                })
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
            'a: for (ix, elm) in stm.iter().enumerate() {
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
                            *query = Query::Found(ix + 1 - len);
                            rem -= 1;
                        }
                    }
                    if rem == 0 { break 'a; }
                }
            }
            let mut res = queries.into_iter()
                .map(|q| match q {
                    Query::Found(ix) => Item::new_number(ix + 1),
                    Query::Pending(_) => Item::new_stream(EmptyStream)
                })
                .collect::<Vec<_>>();
            if res.len() == 1 {
                Ok(res.pop().unwrap())
            } else {
                Ok(Item::from(res))
            }
        },
        _ => Err(StreamError::new("expected: stream.index(item...) or string.index(char or string...)", node))
    }
}

enum Query<I> {
    Pending(I),
    Found(usize)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_index() {
        use super::*;

        test_eval!("seq(5).index(10)" => "6");
        test_eval!("(1..5).index('a')" => "[]");
        test_eval!("[].index([])" => "[]");
        test_eval!("[[]].index([])" => "1");
        test_eval!("\"abc\".index('b')" => "2");
        test_eval!("\"abc\".index(\"\")" => err);
        test_eval!("\"abc\".index(1)" => err);
        test_eval!("\"abc\".index(\"bc\")" => "2");
        test_eval!("\"abc\".index(\"abc\")" => "1");
        test_eval!("\"abc\".index(\"abcd\")" => "[]");
        test_eval!("\"abc\".index(\"zabc\")" => "[]");
        test_eval!("\"abcdefghijklmnopqrstuvwxyz\".repeat().index(\"yza\")" => "25");
        test_eval!("(1..5).repeat(2).skip(1).index(1,5,7)" => "[5, 4, []]");
        test_eval!("\"abcde\".index('b',\"b\",\"cd\",\"de\",\"ef\")" => "[2, 2, 3, 4, []]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("index", eval_index);
    keywords.insert("find", eval_index);
    keywords.insert("pos", eval_index);
}
