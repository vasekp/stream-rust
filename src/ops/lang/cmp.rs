use crate::base::*;

#[derive(Clone)]
struct CmpOp;

type CmpFunc = fn(&[Item]) -> Result<bool, BaseError>;

impl CmpOp {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        let func = Self::find_fn(&node.head);
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);
        let res = try_with!(node, func(&node.args)?);
        Ok(Item::Bool(res))
    }

    fn find_fn(head: &Head) -> CmpFunc {
        match head.as_str().expect("head should be symbol or oper") {
            "==" => Self::eq_func,
            "equal" => Self::eq_func,
            "<>" => Self::ineq_func,
            "<" => Self::lt_func,
            ">" => Self::gt_func,
            "<=" => Self::le_func,
            ">=" => Self::ge_func,
            op => panic!("cmp: unhandled op '{op}'")
        }
    }

    fn eq_func(items: &[Item]) -> Result<bool, BaseError> {
        let mut iter = items.iter();
        let first = iter.next().unwrap(); // args checked to be nonempty in eval()
        for item in iter {
            if !item.try_eq(first)? {
                return Ok(false)
            }
        }
        Ok(true)
    }

    fn ineq_func(items: &[Item]) -> Result<bool, BaseError> {
        match items {
            [lhs, rhs] => lhs.try_eq(rhs).map(|b| !b).map_err(BaseError::from),
            _ => Err("exactly 2 arguments required".into())
        }
    }

    fn lt_func(items: &[Item]) -> Result<bool, BaseError> {
        Self::ineq_chain(items, Number::lt)
    }

    fn gt_func(items: &[Item]) -> Result<bool, BaseError> {
        Self::ineq_chain(items, Number::gt)
    }

    fn le_func(items: &[Item]) -> Result<bool, BaseError> {
        Self::ineq_chain(items, Number::le)
    }

    fn ge_func(items: &[Item]) -> Result<bool, BaseError> {
        Self::ineq_chain(items, Number::ge)
    }

    fn ineq_chain(items: &[Item], cmp: fn(&Number, &Number) -> bool) -> Result<bool, BaseError> {
        let mut iter = items.iter();
        let mut prev = iter.next()
            .unwrap() // args checked to be nonempty in eval()
            .as_num()?;
        for item in iter {
            let next = item.as_num()?;
            if !cmp(prev, next) {
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

        test_eval!("1==1" => "true");
        test_eval!("1==1==1" => "true");
        test_eval!("1==1==2" => "false");
        test_eval!("[]==[]" => "true");
        test_eval!("[]<>[]" => "false");
        test_eval!("[]<>[[]]" => "true");
        test_eval!("(1==1)==true" => "true");
        test_eval!("(1==2)==(3==4)" => "true");
        test_eval!("10<11<12" => "true");
        test_eval!("10<11<11" => "false");
        test_eval!("10<=11<=12" => "true");
        test_eval!("10<=11<=11" => "true");
        test_eval!("10<=11<=10" => "false");
        test_eval!("12>11>10" => "true");
        test_eval!("12>11>11" => "false");
        test_eval!("12>=11>=10" => "true");
        test_eval!("12>=11>=11" => "true");
        test_eval!("12>=11>=12" => "false");
        test_eval!("1<[1]" => err);
        test_eval!("1<'a'" => err);
        test_eval!("'a'=='a'" => "true");
        test_eval!("'a'=='A'" => "false");
        test_eval!("[]==[]" => "true");
        test_eval!("\"\"==\"\"" => "true");
        test_eval!("\"\"==[]" => "false");
        test_eval!("\"abc\"==\"abc\"" => "true");
        test_eval!("\"abc\"==['a','b','c']" => "false");
        test_eval!("\"abc\"==\"ABC\"" => "false");
        test_eval!("1<\"a\"" => err);
        test_eval!("[1]<[2]" => err);
        test_eval!("1+2==3" => "true");
        test_eval!("[1,2,'a']:{1+#}==[2,3]" => "false");
        test_eval!("[1,2,'a']:{1+#}==[2,3,4]" => err);
    }
}

fn eval_assign(node: Node, _env: &Env) -> Result<Item, StreamError> {
    Err(StreamError::new("assignment not possible here, use == for comparisons", node))
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("==", CmpOp::eval);
    keywords.insert("equal", CmpOp::eval);
    keywords.insert("<>", CmpOp::eval);
    keywords.insert("<", CmpOp::eval);
    keywords.insert(">", CmpOp::eval);
    keywords.insert("<=", CmpOp::eval);
    keywords.insert(">=", CmpOp::eval);
    keywords.insert("=", eval_assign);
}
