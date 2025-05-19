use crate::base::*;

#[derive(Clone)]
struct CmpOp;

type CmpFunc = fn(&[Item]) -> Result<bool, BaseError>;

impl CmpOp {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        let func = Self::find_fn(&node.head);
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);
        let res = try_with!(node, func(&node.args)?);
        Ok(Item::Bool(res))
    }

    fn find_fn(head: &Head) -> CmpFunc {
        match head {
            Head::Oper(op) =>
                match op.as_str() {
                    "==" => Self::eq_func,
                    "<>" => Self::ineq_func,
                    "<" => Self::lt_func,
                    ">" => Self::gt_func,
                    "<=" => Self::le_func,
                    ">=" => Self::ge_func,
                    _ => unreachable!("cmp op '{op}'")
                },
            Head::Symbol(sym) =>
                match sym.as_str() {
                    "equal" => Self::eq_func,
                    _ => unreachable!("cmp op '{sym}'")
                },
            _ => unreachable!()
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
            [lhs, rhs] => lhs.try_eq(rhs).map(|b| !b),
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
        use crate::parser::parse;

        assert_eq!(parse("1==1").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("1==1==1").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("1==1==2").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("[]==[]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[]<>[]").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("[]<>[[]]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("(1==1)==true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("(1==2)==(3==4)").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("10<11<12").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("10<11<11").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("10<=11<=12").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("10<=11<=11").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("10<=11<=10").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("12>11>10").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("12>11>11").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("12>=11>=10").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("12>=11>=11").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("12>=11>=12").unwrap().eval_default().unwrap().to_string(), "false");
        assert!(parse("1<[1]").unwrap().eval_default().is_err());
        assert!(parse("1<'a'").unwrap().eval_default().is_err());
        assert_eq!(parse("'a'=='a'").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("'a'=='A'").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("\"abc\"==\"abc\"").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("\"abc\"==['a','b','c']").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("\"abc\"==\"ABC\"").unwrap().eval_default().unwrap().to_string(), "false");
        assert!(parse("1<\"a\"").unwrap().eval_default().is_err());
        assert!(parse("[1]<[2]").unwrap().eval_default().is_err());
        assert_eq!(parse("1+2==3").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("[1,2,'a']:{1+#}==[2,3]").unwrap().eval_default().unwrap().to_string(), "false");
        assert!(parse("[1,2,'a']:{1+#}==[2,3,4]").unwrap().eval_default().is_err());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("==", CmpOp::eval);
    keywords.insert("equal", CmpOp::eval);
    keywords.insert("<>", CmpOp::eval);
    keywords.insert("<", CmpOp::eval);
    keywords.insert(">", CmpOp::eval);
    keywords.insert("<=", CmpOp::eval);
    keywords.insert(">=", CmpOp::eval);
}
