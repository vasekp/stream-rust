use crate::base::*;

use std::collections::VecDeque;

fn eval_index(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(item), .. } => {
            for (ix, elm) in stm.iter().enumerate() {
                check_stop!();
                if elm?.try_eq(item)? {
                    return Ok(Item::new_number(ix + 1));
                }
            }
            Ok(Item::new_stream(EmptyStream))
        },
        RNodeS { source: Item::String(stm), args: RArgs::One(Item::Char(ch)), .. } => {
            for (ix, elm) in stm.iter().enumerate() {
                check_stop!();
                if &elm? == ch {
                    return Ok(Item::new_number(ix + 1));
                }
            }
            Ok(Item::new_stream(EmptyStream))
        },
        RNodeS { source: Item::String(stm), args: RArgs::One(Item::String(other)), .. } => {
            let needle = other.listout()?;
            let len = needle.len();
            let mut deque = VecDeque::with_capacity(len);
            for (ix, elm) in stm.iter().enumerate() {
                check_stop!();
                if deque.len() == needle.len() {
                    deque.pop_front();
                }
                deque.push_back(elm?);
                let (lhs1, lhs2) = deque.as_slices();
                let (rhs1, rhs2) = needle[..].split_at(lhs1.len());
                if lhs1 == rhs1 && lhs2 == rhs2 {
                    return Ok(Item::new_number(ix + 2 - len));
                }
            }
            Ok(Item::new_stream(EmptyStream))
        },
        _ => Err(StreamError::new("expected: stream.index(item) or string.index(char) or string.index(string)", rnode))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_index() {
        use super::*;
        use crate::parser::parse;

        test_eval!("seq(5).index(10)" => "6");
        test_eval!("(1..5).index('a')" => "[]");
        test_eval!("[].index([])" => "[]");
        test_eval!("[[]].index([])" => "1");
        test_eval!("\"abc\".index('b')" => "2");
        test_eval!("\"abc\".index(1)" => err);
        test_eval!("\"abc\".index(\"bc\")" => "2");
        test_eval!("\"abc\".index(\"abc\")" => "1");
        test_eval!("\"abc\".index(\"abcd\")" => "[]");
        test_eval!("\"abc\".index(\"zabc\")" => "[]");
        test_eval!("\"abcdefghijklmnopqrstuvwxyz\".repeat().index(\"yza\")" => "25");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("index", eval_index);
    keywords.insert("find", eval_index);
    keywords.insert("pos", eval_index);
}
