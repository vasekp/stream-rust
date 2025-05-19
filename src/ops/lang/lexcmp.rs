use crate::base::*;

use std::cmp::Ordering;

#[derive(Clone)]
struct LexOp;

type CritFunc = fn(Ordering) -> bool;

impl LexOp {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
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

    fn lex_chain(items: &[Item], func: fn(Ordering) -> bool, env: &Rc<Env>) -> Result<bool, BaseError> {
        let mut iter = items.iter();
        let mut prev = iter.next().unwrap(); // args checked to be nonempty in eval()
        for next in iter {
            if !func(prev.lex_cmp(next, env)?) {
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
        use crate::parser::parse;

        assert_eq!(parse("1<<=1").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("'a'<<='b'").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("'a'<<='A'").unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("'a'<<='á'").unwrap().eval_default().is_err());
        assert_eq!(parse("[1]<<[1]").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("[1]<<[1,1]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[1,[2,[3]]]<<[1,[2,[4]]]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[1,[2,[3]]]<<=[1,[2,[3]]]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[1,[2,[3]]]<<=[1,[2,[2]]]").unwrap().eval_default().unwrap().to_string(), "false");
        assert!(parse("\"ab\"<<=['a','b']").unwrap().eval_default().is_err());
        assert_eq!(parse(r#""ab"<<="abc"<<="abc"<<="abd"<<="ac""#).unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse(r#""ab"<<"abc"<<"abc"<<"abd"<<"ac""#).unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse(r#""ab"<<"abc"<<"abd"<<"ac""#).unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("[1,2,'a']:{1+#}>>=[2,3,4]").unwrap().eval_default().is_err());
        assert!(parse("[1,2,'a']:{1+#}>>=[2,3]").unwrap().eval_default().is_err());
        assert_eq!(parse("[1,2,'a']:{1+#}>>=[2,2]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[1,2,'a']:{1+#}>>=[2,4]").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("[1,2,'a']:{1+#}>>=[2]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("\"abc\"<<=\"ABC\"").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("\"abc\">>=\"ABC\"").unwrap().eval_default().unwrap().to_string(), "true");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("<<", LexOp::eval);
    keywords.insert(">>", LexOp::eval);
    keywords.insert("<<=", LexOp::eval);
    keywords.insert(">>=", LexOp::eval);
}
