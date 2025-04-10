use crate::base::*;

#[derive(Clone)]
struct MathOp {
    node: ENode,
    env: Rc<Env>,
    func: MathFunc
}

type MathFunc = fn(&[Item], &Rc<Env>) -> Result<Item, BaseError>;

impl MathOp {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let func = Self::find_fn(&node.head);
        Self::eval_with(node, env, func)
    }

    fn eval_with(node: Node, env: &Rc<Env>, func: MathFunc) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);
        if node.args.iter().any(Item::is_stream) {
            Ok(Item::new_stream(MathOp{node, env: Rc::clone(env), func}))
        } else {
            Ok(try_with!(node, func(&node.args, env)?))
        }
    }

    fn find_fn(head: &Head) -> MathFunc {
        let Head::Oper(op) = head else { unreachable!() };
        match op.as_str() {
            "+" => Self::plus_func,
            "-" => Self::minus_func,
            "*" => Self::mul_func,
            "/" => Self::div_func,
            "^" => Self::pow_func,
            _ => todo!()
        }
    }

    fn plus_func(items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
        let mut iter = items.iter();
        match iter.next().unwrap() { // args checked to be nonempty in eval_with()
            Item::Number(init) => {
                let ans = iter.try_fold(init.to_owned(), |a, e| e.as_num().map(|num| a + num));
                Ok(Item::new_number(ans?))
            },
            Item::Char(ref ch) => {
                let abc = env.alphabet();
                let (index, case) = abc.ord_case(ch)?;
                let ans = iter.try_fold(index.into(),
                    |a, e| {
                        match e {
                            Item::Number(ref num) => Ok(a + num),
                            Item::Char(ref ch) => Ok(a + abc.ord_case(ch)?.0),
                            _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                        }
                    })?;
                Ok(Item::new_char(abc.chr_case(&ans, case)))
            },
            item => Err(format!("expected number or character, found {:?}", item).into())
        }
    }

    fn minus_func(items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
        match items {
            [item] => Ok(Item::new_number(-item.as_num()?)),
            [lhs, rhs] => match lhs {
                Item::Number(lhs) => Ok(Item::new_number(lhs - rhs.as_num()?)),
                Item::Char(ch) => {
                    let abc = env.alphabet();
                    let (index, case) = abc.ord_case(ch)?;
                    match rhs {
                        Item::Number(ref num) => Ok(Item::new_char(abc.chr_case(&(index - num), case))),
                        Item::Char(ref ch) => Ok(Item::new_number(index - abc.ord_case(ch)?.0)),
                        _ => Err(format!("expected number or character, found {:?}", rhs).into())
                    }
                },
                _ => Err(format!("expected number or character, found {:?}", lhs).into())
            },
            _ => Err("1 or 2 arguments required".into())
        }
    }

    fn mul_func(items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
        let mut iter = items.iter();
        debug_assert!(!items.is_empty());
        match iter.next().unwrap() { // args checked to be nonempty in eval_with()
            Item::Number(init) => {
                let ans = iter.try_fold(init.to_owned(), |a, e| e.as_num().map(|num| a * num))?;
                Ok(Item::new_number(ans))
            },
            Item::Char(ref ch) => {
                let abc = env.alphabet();
                let (index, case) = abc.ord_case(ch)?;
                let ans = iter.try_fold(index.into(), |a, e| e.as_num().map(|num| a * num))?;
                Ok(Item::new_char(abc.chr_case(&ans, case)))
            },
            item => Err(format!("expected number or character, found {:?}", item).into())
        }
    }

    fn div_func(items: &[Item], _env: &Rc<Env>) -> Result<Item, BaseError> {
        match items {
            [lhs, rhs] => {
                let (lhs, rhs) = (lhs.as_num()?, rhs.as_num()?);
                if rhs.is_zero() {
                    Err("division by zero".into())
                } else {
                    Ok(Item::new_number(lhs / rhs))
                }
            },
            _ => Err("exactly 2 argument required".into())
        }
    }

    fn pow_func(items: &[Item], _env: &Rc<Env>) -> Result<Item, BaseError> {
        match items {
            [base, exp] => {
                let (base, exp) = (base.as_num()?, exp.as_num()?);
                if exp.is_negative() {
                    return Err("negative exponent".into());
                }
                let Some(exp) = exp.to_u32() else {
                    return Err("exponent too large".into());
                };
                Ok(Item::new_number(base.pow(exp)))
            },
            _ => Err("exactly 2 argument required".into())
        }
    }
}

impl Describe for MathOp {
    fn describe(&self, prec: u32) -> String {
        self.env.wrap_describe(self.node.describe(prec))
    }
}

impl Stream for MathOp {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(MathOpIter{head: &self.node.head, args, env: &self.env, func: self.func})
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::Infinite
            })
            .reduce(|a, e| Length::intersection(&a, &e))
            .unwrap() // args checked to be nonempty in eval_with()
    }

    fn is_empty(&self) -> bool {
        self.node.args.iter()
            .any(|item| match item {
                Item::Stream(stm) => stm.is_empty(),
                _ => false
            })
    }
}

struct MathOpIter<'node> {
    head: &'node Head,
    args: Vec<Box<dyn SIterator + 'node>>,
    env: &'node Rc<Env>,
    func: MathFunc
}

impl Iterator for MathOpIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.args.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>()
        {
            Some(Ok(inputs)) => {
                let node = Node::new(self.head.clone(), None,
                    inputs.into_iter().map(Expr::from).collect());
                Some(MathOp::eval_with(node, self.env, self.func))
            },
            Some(Err(err)) => Some(Err(err)),
            None => None
        }
    }
}

impl SIterator for MathOpIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let mut remain = UNumber::zero();
        for iter in &mut self.args {
            if let Some(r) = iter.skip_n(n.clone())? {
                remain = std::cmp::max(remain, r);
            }
        }
        if remain.is_zero() { Ok(None) }
        else { Ok(Some(remain)) }
    }

    fn len_remain(&self) -> Length {
        self.args.iter()
            .map(|iter| iter.len_remain())
            .reduce(|a, b| Length::intersection(&a, &b))
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opers() {
        use crate::parser::parse;

        assert_eq!(parse("1+(-2)").unwrap().eval().unwrap().to_string(), "-1");
        assert_eq!(parse("(-1)-(-2)").unwrap().eval().unwrap().to_string(), "1");
        assert_eq!(parse("2*(-4)").unwrap().eval().unwrap().to_string(), "-8");
        assert_eq!(parse("11/2").unwrap().eval().unwrap().to_string(), "5");
        assert_eq!(parse("(-11)/(-2)").unwrap().eval().unwrap().to_string(), "5");
        assert_eq!(parse("1/2").unwrap().eval().unwrap().to_string(), "0");
        assert!(parse("1/0").unwrap().eval().is_err());
        assert_eq!(parse("10^30").unwrap().eval().unwrap().to_string(), "1000000000000000000000000000000");
        assert_eq!(parse("0^0").unwrap().eval().unwrap().to_string(), "1");
        assert_eq!(parse("0^1").unwrap().eval().unwrap().to_string(), "0");
        assert!(parse("1^(-1)").unwrap().eval().is_err());

        assert_eq!(parse("'a'+'b'+'c'").unwrap().eval().unwrap().to_string(), "'f'");
        assert_eq!(parse("'E'+3+'a'").unwrap().eval().unwrap().to_string(), "'I'");
        assert_eq!(parse("'x'+'Y'+'z'").unwrap().eval().unwrap().to_string(), "'w'");
        assert!(parse("1+'a'").unwrap().eval().is_err());

        assert_eq!(parse("1..5+3+[0,10,20]").unwrap().eval().unwrap().to_string(), "[4, 15, 26]");
        assert_eq!(parse("1..3+3+seq").unwrap().eval().unwrap().to_string(), "[5, 7, 9]");
        assert_eq!(parse("'A'..'e'+3+[0,10,20]").unwrap().eval().unwrap().to_string(), "['D', 'O', 'Z']");
        assert_eq!(parse("\"AbC\"+3+[0,10,20]").unwrap().eval().unwrap().to_string(), "['D', 'o', 'Z']");
        assert_eq!(parse(r#""ahoj"+"bebe""#).unwrap().eval().unwrap().to_string(), "['c', 'm', 'q', 'o']");
        assert_eq!(parse("(1..5+3+[]).len").unwrap().eval().unwrap().to_string(), "0");
        assert_eq!(parse("(1..5+3+seq).len").unwrap().eval().unwrap().to_string(), "5");
        assert_eq!(parse(r#""abc"+['d',5,true]"#).unwrap().eval().unwrap().to_string(), "['e', 'g', <!>");
        assert_eq!(parse(r#"['a','b','c']+"def""#).unwrap().eval().unwrap().to_string(), "['e', 'g', 'i']");
        assert_eq!(parse(r#"['a','b',3]+"def""#).unwrap().eval().unwrap().to_string(), "['e', 'g', <!>");
        assert_eq!(parse("seq+true").unwrap().eval().unwrap().to_string(), "[<!>");
        assert_eq!(parse("1+\"a\"").unwrap().eval().unwrap().to_string(), "[<!>");
        assert_eq!(parse("'a'+\"xyz\"").unwrap().eval().unwrap().to_string(), "['y', 'z', 'a']");
        assert!(parse("true+false").unwrap().eval().is_err());
        assert_eq!(parse("[1,[2,[3]]]+1").unwrap().eval().unwrap().to_string(), "[2, [3, [4]]]");
        assert_eq!(parse("['a',['b',['c']]]+[1,2]").unwrap().eval().unwrap().to_string(), "['b', ['d', ['e']]]");
        assert_eq!(parse("['b','b',2]-[1,'a','a']").unwrap().eval().unwrap().to_string(), "['a', 1, <!>");
        assert_eq!(parse("-[1,[1,'a']]").unwrap().eval().unwrap().to_string(), "[-1, [-1, <!>");
        assert_eq!(parse("[2,'b','b']*[2,2,'b']").unwrap().eval().unwrap().to_string(), "[4, 'd', <!>");
        assert!(parse("2*'b'").unwrap().eval().is_err());
        assert!(parse("1/'a'").unwrap().eval().is_err());
        assert!(parse("'a'/1").unwrap().eval().is_err());
        assert_eq!(parse("[2,'b','b']*[2,2,'b']").unwrap().eval().unwrap().to_string(), "[4, 'd', <!>");
        assert_eq!(parse("seq^seq").unwrap().eval().unwrap().to_string(), "[1, 4, 27, 256, 3125, ...]");

        assert_eq!(parse("((1..4).repeat+(1..5).repeat)[10^10]").unwrap().eval().unwrap().to_string(), "9");
        test_len_exact(&parse("[1,2,3]+seq+5").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("5+[1,2,3]+seq+5").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("[1,2,3]+seq+[5]").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("[1,2,3]+[]+seq").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("range(10^10)+seq+5").unwrap().eval().unwrap());
        test_skip_n(&parse("range(10^10)+range(10^11)").unwrap().eval().unwrap());
        test_skip_n(&parse("seq+[]").unwrap().eval().unwrap());
        test_skip_n(&parse("seq*seq").unwrap().eval().unwrap());

        assert_eq!(parse("1+2+3+4-5*6*7/8").unwrap().eval().unwrap().describe(0), "-16");
        assert_eq!(parse("[1]+[2]+[3]+[4]-[5]*[6]*[7]/[8]").unwrap().eval().unwrap().describe(0), "(([1]+[2]+[3]+[4])-(([5]*[6]*[7])/[8]))");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("+", MathOp::eval);
    keywords.insert("-", MathOp::eval);
    keywords.insert("*", MathOp::eval);
    keywords.insert("/", MathOp::eval);
    keywords.insert("^", MathOp::eval);
}
