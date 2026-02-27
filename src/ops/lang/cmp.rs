use crate::base::*;

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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("==", CmpOp::eval, r#"
Checks for equality of all `input`s: evaluates to `true` if they all are equal, `false` otherwise.
= op1 == op2 == ...
> 1+1 ? 2 => true
> 2+2 ? 2*2 ? 2^2 ? 4 => true
> 0 ? [] => false
: equal
: <>
: <
: >
: <=
: >=
: <<
: >>
: <<=
: >>=
"#);
    symbols.insert("equal", CmpOp::eval, r#"
Checks for equality of all `input`s: evaluates to `true` if they all are equal, `false` otherwise.
The shorthand for `?(input1, input2, ...)` is `input1 == input2 == ...`.
= ?(input1, input2, ...)
> [1, 2, 2, 3, 3].?windows(2, ?) => [false, true, false, true]
> [1, 2, 2, 2, 3].?windows(3, ?) => [false, true, false]
: ==
: and
: or
: not
"#);
    symbols.insert("<>", CmpOp::eval, r#"
Checks for inequality of `op1` and `op2`: evaluates to `false` if they all are equal, `true` otherwise.
= op1 ? op2
> 0 ? "0" => true
> [] ? "" => true
> [] ? [[]] => true
> [1, 2, 2, 3, 3].?windows(2, {#1 ? #2}) => [true, false, true, false]
: ==
: !
"#);
    symbols.insert("<", CmpOp::eval, r#"
Evaluates to `true` if each number is strictly less than the next.
= op1 ? op2 ? ...
> 10 ? 11 => true
> 10 ? 11 ? 11 ? 12 => false
> 10 ? 11 <= 11 => !can not mix inequalities, use ?and
: <=
: >
: ==
: <>
: <<
"#);
    symbols.insert(">", CmpOp::eval, r#"
Evaluates to `true` if each number is strictly greater than the next.
= op1 ? op2 ? ...
> 11 ? 10 => true
> 12 ? 11 ? 11 ? 10 => false
> 11 ? 10 >= 10 => !can not mix inequalities, use ?and
: <
: >=
: ==
: <>
: >>
"#);
    symbols.insert("<=", CmpOp::eval, r#"
Evaluates to `true` if each number is less than or equal to the next.
= op1 ? op2 ? ...
> 10 ? 11 => true
> 10 ? 11 ? 11 ? 12 => true
> 11 ? 11 < 12 => !can not mix inequalities, use ?and
: <
: >=
: ==
: <>
: <<=
"#);
    symbols.insert(">=", CmpOp::eval, r#"
Evaluates to `true` if each number is greater than or equal to the next.
= op1 ? op2 ? ...
> 11 ? 10 => true
> 12 ? 11 ? 11 ? 11 => true
> 11 ? 11 > 10 => !can not mix inequalities, use ?and
: >
: <=
: ==
: <>
: >>=
"#);
    symbols.insert("=", eval_assign, r#"
Assigns `value` to `name`.
This can only be used for local assignments using `?with` or for global variables. Use `==` for comparison.
= name = variable
> 10 = 11 => !can not assign to 10
> a = 10 => !can only assign to global symbols
> ?with(a = 10, a) => 10
: ==
: with
"#);
}
