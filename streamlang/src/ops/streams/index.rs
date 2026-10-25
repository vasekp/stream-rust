use crate::base::*;

use std::collections::VecDeque;

fn eval_index(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    match &node.source {
        Some(Item::Stream(stm)) => {
            let mut queries = node.args.into_iter().map(Query::Pending).collect::<Vec<_>>();
            let mut rem = queries.len();
            'a: for (ix, elm) in stm.iter().transposed().enumerate() {
                check_stop!();
                let elm = elm?;
                for query in &mut queries {
                    if let Query::Pending(item) = query
                        && elm.try_eq(item)?
                    {
                        *query = Query::Found(ix);
                        rem -= 1;
                    }
                    if rem == 0 {
                        break 'a;
                    }
                }
            }
            let mut res = queries
                .into_iter()
                .map(|q| match q {
                    Query::Found(ix) => Item::new_number(ix + 1),
                    Query::Pending(_) => Item::empty_stream(),
                })
                .collect::<Vec<_>>();
            if res.len() == 1 {
                Ok(res.pop().unwrap()) // len == 1
            } else {
                Ok(Item::from(res))
            }
        },
        Some(Item::String(stm)) => {
            let mut queries = node
                .args
                .iter()
                .map(|item| Ok(Query::Pending(item.to_char_vec_nonempty()?)))
                .collect::<SResult<Vec<_>>>()?;
            let longest = queries
                .iter()
                .map(|q| match q {
                    Query::Pending(vec) => vec.len(),
                    _ => unreachable!(),
                })
                .reduce(std::cmp::max)
                .unwrap(); // len ≥ 1
            let mut rem = queries.len();
            let mut deque = VecDeque::with_capacity(longest);
            'a: for (ix, elm) in stm.iter().transposed().enumerate() {
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
                            deque.len() >= len && deque.range(deque.len() - len..).eq(item)
                        };
                        if found {
                            *query = Query::Found(ix + 1 - len);
                            rem -= 1;
                        }
                    }
                    if rem == 0 {
                        break 'a;
                    }
                }
            }
            let mut res = queries
                .into_iter()
                .map(|q| match q {
                    Query::Found(ix) => Item::new_number(ix + 1),
                    Query::Pending(_) => Item::empty_stream(),
                })
                .collect::<Vec<_>>();
            if res.len() == 1 { Ok(res.pop().unwrap()) } else { Ok(Item::from(res)) }
        },
        _ => Err(StreamError::usage(&node.head)),
    }
}

enum Query<I> {
    Pending(I),
    Found(usize),
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["index", "find", "pos"], eval_index, r#"
Returns the position (counted from 1) of the first occurrence of `item` in `stream`, or of `char` or `substring` in `string`.
If the match is not found, evaluates to `[]`.
If multiple arguments are provided, evaluates to a stream of the results.
! Never terminates on an infinite input which does not contain all of the queried items.
= stream.?(item)
= stream.?(item, ...)
= string.?(char)
= string.?(substring)
= string.?(char|substring, ...)
> [10, 11, 12, 13].?(13) => 4
> [10, 11, 12, 13].?(1) => []
> [10, 11, 12, 13].?(1, 10, 15) => [[], 1, []]
> "abcde".?('d') => 4
> "abcde".?("cd") => 3
> "abcde".?("def") => []
: contains
: indices
"#);
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
        // Queries shorter than the longest one, matching before the window is full
        test_eval!("\"abcd\".index(\"abcd\",\"bc\")" => "[1, 2]");
        test_eval!("\"abc\".index(\"abc\",\"ab\")" => "[1, 1]");
        test_eval!("\"ab\".index(\"abcd\",\"ab\")" => "[[], 1]");
        test_eval!("\"a\".index(\"abcd\",\"ab\",'a')" => "[[], [], 1]");
    }
}
