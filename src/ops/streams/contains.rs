use crate::base::*;

use std::collections::VecDeque;

fn eval_contains(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(item), .. } => {
            for elm in stm.iter() {
                check_stop!();
                if elm?.try_eq(item)? {
                    return Ok(Item::Bool(true));
                }
            }
            Ok(Item::Bool(false))
        },
        RNodeS { source: Item::String(stm), args: RArgs::One(Item::Char(ch)), .. } => {
            for elm in stm.iter() {
                check_stop!();
                if &elm? == ch {
                    return Ok(Item::Bool(true));
                }
            }
            Ok(Item::Bool(false))
        },
        RNodeS { source: Item::String(stm), args: RArgs::One(Item::String(other)), .. } => {
            let needle = other.listout()?;
            let len = needle.len();
            let mut deque = VecDeque::with_capacity(len);
            for elm in stm.iter() {
                check_stop!();
                if deque.len() == needle.len() {
                    deque.pop_front();
                }
                deque.push_back(elm?);
                let (lhs1, lhs2) = deque.as_slices();
                let (rhs1, rhs2) = needle[..].split_at(lhs1.len());
                if lhs1 == rhs1 && lhs2 == rhs2 {
                    return Ok(Item::Bool(true));
                }
            }
            Ok(Item::Bool(false))
        },
        _ => Err(StreamError::new("expected: stream.contains(item) or string.contains(char) or string.contains(string)", rnode))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_contains() {
        use super::*;
        use crate::parser::parse;

        test_eval!("seq.contains(5)" => "true");
        test_eval!("(1..5).contains('a')" => "false");
        test_eval!("[].contains([])" => "false");
        test_eval!("\"abc\".contains('b')" => "true");
        test_eval!("\"abc\".contains(1)" => err);
        test_eval!("\"abc\".contains(\"bc\")" => "true");
        test_eval!("\"abc\".contains(\"abc\")" => "true");
        test_eval!("\"abc\".contains(\"abcd\")" => "false");
        test_eval!("\"abc\".contains(\"zabc\")" => "false");
        test_eval!("\"abcdefghijklmnopqrstuvwxyz\".repeat().contains(\"yza\")" => "true");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("contains", eval_contains);
}
