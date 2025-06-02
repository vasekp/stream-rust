use crate::base::*;

use std::cmp::Ordering;

#[derive(Clone)]
struct LexOp;

type CritFunc = fn(Ordering) -> bool;

impl LexOp {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        let func = Self::find_fn(&node.head);
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);
        let res = try_with!(node, Self::lex_chain(&node.args, func, env)?);
        Ok(Item::Bool(res))
    }

    fn find_fn(head: &Head) -> CritFunc {
        match head {
            Head::Oper(op) =>
                match op.as_str() {
                    "<<" => |ord| ord == Ordering::Less,
                    ">>" => |ord| ord == Ordering::Greater,
                    "<<=" => |ord| ord != Ordering::Greater,
                    ">>=" => |ord| ord != Ordering::Less,
                    _ => unreachable!("lex-cmp op '{op}'")
                },
            _ => unreachable!()
        }
    }

    fn lex_chain(items: &[Item], func: fn(Ordering) -> bool, env: &Env) -> Result<bool, BaseError> {
        let mut iter = items.iter();
        let mut prev = iter.next().unwrap(); // args checked to be nonempty in eval()
        for next in iter {
            if !func(prev.lex_cmp(next, env.alphabet())?) {
                return Ok(false);
            }
            prev = next;
        }
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_cmp() {
        use super::*;
        use crate::parser::parse;

        test_eval!("1<<=1" => "true");
        test_eval!("'a'<<='b'" => "true");
        test_eval!("'a'<<='A'" => "true");
        test_eval!("'a'<<='á'" => err);
        test_eval!("[1]<<[1]" => "false");
        test_eval!("[1]<<[1,1]" => "true");
        test_eval!("[1,[2,[3]]]<<[1,[2,[4]]]" => "true");
        test_eval!("[1,[2,[3]]]<<=[1,[2,[3]]]" => "true");
        test_eval!("[1,[2,[3]]]<<=[1,[2,[2]]]" => "false");
        test_eval!("\"ab\"<<=['a','b']" => err);
        test_eval!(r#""ab"<<="abc"<<="abc"<<="abd"<<="ac""# => "true");
        test_eval!(r#""ab"<<"abc"<<"abc"<<"abd"<<"ac""# => "false");
        test_eval!(r#""ab"<<"abc"<<"abd"<<"ac""# => "true");
        test_eval!("[1,2,'a']:{1+#}>>=[2,3,4]" => err);
        test_eval!("[1,2,'a']:{1+#}>>=[2,3]" => err);
        test_eval!("[1,2,'a']:{1+#}>>=[2,2]" => "true");
        test_eval!("[1,2,'a']:{1+#}>>=[2,4]" => "false");
        test_eval!("[1,2,'a']:{1+#}>>=[2]" => "true");
        test_eval!("\"abc\"<<=\"ABC\"" => "true");
        test_eval!("\"abc\">>=\"ABC\"" => "true");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("<<", LexOp::eval);
    keywords.insert(">>", LexOp::eval);
    keywords.insert("<<=", LexOp::eval);
    keywords.insert(">>=", LexOp::eval);
}
