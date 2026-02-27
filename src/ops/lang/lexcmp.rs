use crate::base::*;

use std::cmp::Ordering;

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
        match head.as_str().expect("head should be symbol or oper") {
            "<<" => |ord| ord == Ordering::Less,
            ">>" => |ord| ord == Ordering::Greater,
            "<<=" => |ord| ord != Ordering::Greater,
            ">>=" => |ord| ord != Ordering::Less,
            op => panic!("lex-cmp: unhandled op '{op}'")
        }
    }

    fn lex_chain(items: &[Item], func: fn(Ordering) -> bool, env: &Env) -> Result<bool, BaseError> {
        let mut iter = items.iter();
        let mut prev = iter.next().unwrap(); // args checked to be nonempty in eval()
        for next in iter {
            if !func(prev.lex_cmp(next, &env.alpha)?) {
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("<<", LexOp::eval, r#"
Lexical comparison: Evaluates to `true` if each `item` compares before the next.
= op1 ? op2 ? ...
> "abc" ? "abd" ? "ac" => true
> "a" ? "A" => false
> 'a' ? "a" => !can't compare characters and strings
> [1,2,3] ? [1,2,4] ? [1,3] => true
: <<=
: >>
: <
: ==
: alpha
: sort
"#);
    symbols.insert(">>", LexOp::eval, r#"
Lexical comparison: Evaluates to `true` if each `item` compares after the next.
= op1 ? op2 ? ...
> "abc" ? "aba" ? "ab" => true
> "a" ? "A" => false
> 'a' ? "a" => !can't compare characters and strings
> [1,2,3] ? [1,2,1] ? [1,2] => true
: <<
: >>=
: >
: ==
: alpha
: sort
"#);
    symbols.insert("<<=", LexOp::eval, r#"
Lexical comparison: Evaluates to `true` if each `item` compares before or equally with the next.
= op1 ? op2 ? ...
> "abc" ? "abd" ? "ac" => true
> "a" ? "A" => true
> 'a' ? "a" => !can't compare characters and strings
> [1,2,3] ? [1,3] ? [1,3] => true
: <<
: >>=
: <=
: ==
: alpha
: sort
"#);
    symbols.insert(">>=", LexOp::eval, r#"
Lexical comparison: Evaluates to `true` if each `item` compares after or equally with the next.
= op1 ? op2 ? ...
> "abc" ? "abc" ? "ab" => true
> "a" ? "A" => true
> 'a' ? "a" => !can't compare characters and strings
> [1,2,1] ? [1,2,1] ? [1,2] => true
: >>
: <<=
: >=
: ==
: alpha
: sort
"#);
}
