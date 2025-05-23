use crate::base::*;

#[derive(Clone)]
struct MathOp {
    node: ENode,
    func: MathFunc,
    alpha: Rc<Alphabet>,
}

type MathFunc = fn(&[Item], &Rc<Alphabet>) -> Result<Item, BaseError>;

fn eval_op(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    match try_with!(node, node.first_arg_checked()?) {
        Item::String(_) => StringOp::eval(node, env),
        _ => MathOp::eval(node, env)
    }
}

impl MathOp {
    fn eval(node: ENode, env: &Rc<Env>) -> Result<Item, StreamError> {
        let func = Self::find_fn(&node.head);
        Self::eval_with(node, env.alphabet(), func)
    }

    fn eval_with(node: ENode, alpha: &Rc<Alphabet>, func: MathFunc) -> Result<Item, StreamError> {
        if node.args.iter().any(Item::is_stream) {
            Ok(Item::new_stream(MathOp{node, func, alpha: Rc::clone(alpha)}))
        } else {
            Ok(try_with!(node, func(&node.args, alpha)?))
        }
    }

    fn find_fn(head: &Head) -> MathFunc {
        match head {
            Head::Oper(op) =>
                match op.as_str() {
                    "+" => Self::plus_func,
                    "-" => Self::minus_func,
                    "*" => Self::mul_func,
                    "/" => Self::div_func,
                    "^" => Self::pow_func,
                    _ => unreachable!("math op '{op}'")
                },
            Head::Symbol(sym) =>
                match sym.as_str() {
                    "plus" => Self::plus_func,
                    "times" => Self::mul_func,
                    _ => unreachable!("math op '{sym}'")
                },
            _ => unreachable!()
        }
    }

    fn plus_func(items: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        let mut iter = items.iter();
        match iter.next().unwrap() { // args checked to be nonempty in eval_with()
            Item::Number(init) => {
                let ans = iter.try_fold(init.to_owned(), |a, e| e.as_num().map(|num| a + num));
                Ok(Item::new_number(ans?))
            },
            Item::Char(ref ch) => {
                let (index, case) = alpha.ord_case(ch)?;
                let ans = iter.try_fold(index.into(),
                    |a, e| {
                        match e {
                            Item::Number(ref num) => Ok(a + num),
                            Item::Char(ref ch) => Ok(a + alpha.ord_case(ch)?.0),
                            _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                        }
                    })?;
                Ok(Item::new_char(alpha.chr_case(&ans, case)))
            },
            item => Err(format!("expected number or character, found {:?}", item).into())
        }
    }

    fn minus_func(items: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        match items {
            [item] => Ok(Item::new_number(-item.as_num()?)),
            [lhs, rhs] => match lhs {
                Item::Number(lhs) => Ok(Item::new_number(lhs - rhs.as_num()?)),
                Item::Char(ch) => {
                    let (index, case) = alpha.ord_case(ch)?;
                    let ord = match rhs {
                        Item::Number(ref num) => index - num,
                        Item::Char(ref ch) => (index - alpha.ord_case(ch)?.0).into(),
                        _ => return Err(format!("expected number or character, found {:?}", rhs).into())
                    };
                    Ok(Item::new_char(alpha.chr_case(&ord, case)))
                },
                _ => Err(format!("expected number or character, found {:?}", lhs).into())
            },
            _ => Err("1 or 2 arguments required".into())
        }
    }

    fn mul_func(items: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        let mut iter = items.iter();
        debug_assert!(!items.is_empty());
        match iter.next().unwrap() { // args checked to be nonempty in eval_with()
            Item::Number(init) => {
                let ans = iter.try_fold(init.to_owned(), |a, e| e.as_num().map(|num| a * num))?;
                Ok(Item::new_number(ans))
            },
            Item::Char(ref ch) => {
                let (index, case) = alpha.ord_case(ch)?;
                let ans = iter.try_fold(index.into(), |a, e| e.as_num().map(|num| a * num))?;
                Ok(Item::new_char(alpha.chr_case(&ans, case)))
            },
            item => Err(format!("expected number or character, found {:?}", item).into())
        }
    }

    fn div_func(items: &[Item], _alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        match items {
            [lhs, rhs] => {
                let (lhs, rhs) = (lhs.as_num()?, rhs.as_num()?);
                if rhs.is_zero() {
                    Err("division by zero".into())
                } else {
                    Ok(Item::new_number(lhs / rhs))
                }
            },
            _ => Err("exactly 2 arguments required".into())
        }
    }

    fn pow_func(items: &[Item], _alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
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
            _ => Err("exactly 2 arguments required".into())
        }
    }
}

impl Describe for MathOp {
    fn describe_prec(&self, prec: u32) -> String {
        self.alpha.wrap_describe(|prec| self.node.describe_prec(prec), prec)
    }
}

impl Stream for MathOp {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(MathOpIter{head: &self.node.head, args, alpha: &self.alpha, func: self.func})
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::Infinite
            })
            .reduce(Length::intersection)
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
    alpha: &'node Rc<Alphabet>,
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
                let node = ENode { head: self.head.clone(), source: None, args: inputs };
                Some(MathOp::eval_with(node, self.alpha, self.func))
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
            .reduce(Length::intersection)
            .unwrap()
    }
}

#[derive(Clone)]
struct StringOp {
    first: BoxedStream,
    node_rem: ENode,
    func: StringFunc,
    alpha: Rc<Alphabet>,
}

type StringFunc = fn(&Char, &[Item], &Rc<Alphabet>) -> Result<Item, BaseError>;

impl StringOp {
    fn eval(mut node: ENode, env: &Rc<Env>) -> Result<Item, StreamError> {
        let func = try_with!(node, Self::find_fn(&node.head)?);
        let alpha = env.alphabet();
        if node.args.len() < 2 {
            return Err(StreamError::new("not available for strings", node));
        }
        let Item::String(first) = node.args.remove(0) else { unreachable!() };
        Ok(Item::new_string_stream(StringOp{first: first.into(), node_rem: node, func, alpha: Rc::clone(alpha)}))
    }

    fn find_fn(head: &Head) -> Result<StringFunc, BaseError> {
        match head {
            Head::Oper(op) =>
                match op.as_str() {
                    "+" => Ok(Self::plus_func),
                    "-" => Ok(Self::minus_func),
                    _ => Err(format!("operation {op} not available for strings").into())
                },
            Head::Symbol(sym) =>
                match sym.as_str() {
                    "plus" => Ok(Self::plus_func),
                    _ => Err(format!("operation {sym} not available for strings").into())
                },
            _ => unreachable!()
        }
    }

    fn plus_func(first: &Char, rest: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        let (index, case) = alpha.ord_case(first)?;
        let ans = rest.iter().try_fold(index.into(),
            |a, e| {
                match e {
                    Item::Number(ref num) => Ok(a + num),
                    Item::Char(ref ch) => Ok(a + alpha.ord_case(ch)?.0),
                    _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                }
            })?;
        Ok(Item::new_char(alpha.chr_case(&ans, case)))
    }

    fn minus_func(first: &Char, rest: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        let (index, case) = alpha.ord_case(first)?;
        let ord = match rest {
            [other] => match other {
                Item::Number(ref num) => index - num,
                Item::Char(ref ch) => (index - alpha.ord_case(ch)?.0).into(),
                _ => return Err(BaseError::from(format!("expected number or character, found {:?}", other)))
            },
            _ => return Err("not available for strings".into())
        };
        Ok(Item::new_char(alpha.chr_case(&ord, case)))
    }
}

impl Describe for StringOp {
    fn describe_prec(&self, prec: u32) -> String {
        self.alpha.wrap_describe(|prec| Node::describe_helper(
                &self.node_rem.head,
                None::<&Item>,
                [self.first.to_item()].iter().chain(self.node_rem.args.iter()),
                prec
        ), prec)
    }
}

impl Stream for StringOp {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let first = self.first.string_iter();
        let rest = self.node_rem.args.iter()
            .map(|item| match item {
                Item::Stream(stm) | Item::String(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(StringOpIter{first, rest, alpha: &self.alpha, func: self.func})
    }

    fn length(&self) -> Length {
        self.node_rem.args.iter()
            .map(|item| match item {
                Item::Stream(stm) | Item::String(stm) => stm.length(),
                _ => Length::Infinite
            })
            .map(|len| match len {
                Length::Infinite => Length::Infinite,
                _ => Length::Unknown
            })
            .fold(self.first.length(), Length::intersection)
    }

    fn is_empty(&self) -> bool {
        self.first.is_empty() || self.node_rem.args.iter()
            .any(|item| match item {
                Item::Stream(stm) => stm.is_empty(),
                _ => false
            })
    }
}

struct StringOpIter<'node> {
    first: StringIterator<'node>,
    rest: Vec<Box<dyn SIterator + 'node>>,
    alpha: &'node Rc<Alphabet>,
    func: StringFunc
}

impl Iterator for StringOpIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(base: Char, mut inputs: Vec<Item>) -> Node {
            inputs.insert(0, Item::Char(base));
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }

        let ch = match self.first.next() {
            None => return None,
            Some(Ok(ch)) => ch,
            Some(Err(err)) => return Some(Err(err))
        };
        if !self.alpha.contains(&ch) {
            return Some(Ok(Item::Char(ch)));
        }

        let rest = self.rest.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>();
        match rest {
            None => None,
            Some(Ok(inputs)) => {
                match (self.func)(&ch, &inputs, self.alpha) {
                    Ok(item) => Some(Ok(item)),
                    Err(err) => Some(Err(StreamError::new(err, aux_node(ch, inputs))))
                }
            },
            Some(Err(err)) => Some(Err(err))
        }
    }
}

impl SIterator for StringOpIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let mut n_chars = UNumber::zero();
        let mut remain = n;
        while !remain.is_zero() {
            match self.first.next() {
                Some(Ok(ch)) => {
                    if self.alpha.contains(&ch) {
                        n_chars.inc();
                    }
                },
                Some(Err(err)) => return Err(err),
                None => break
            }
            remain.dec();
        }
        for iter in self.rest.iter_mut() {
            if let Some(r) = iter.skip_n(n_chars.clone())? {
                remain = std::cmp::max(remain, r);
            }
        }
        if remain.is_zero() { Ok(None) }
        else { Ok(Some(remain)) }
    }

    fn len_remain(&self) -> Length {
        self.rest.iter()
            .map(|iter| iter.len_remain())
            .map(|len| match len {
                Length::Infinite => Length::Infinite,
                _ => Length::Unknown
            })
            .fold(self.first.len_remain(), Length::intersection)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opers() {
        use crate::parser::parse;

        assert_eq!(parse("1+(-2)").unwrap().eval_default().unwrap().to_string(), "-1");
        assert_eq!(parse("(-1)-(-2)").unwrap().eval_default().unwrap().to_string(), "1");
        assert_eq!(parse("2*(-4)").unwrap().eval_default().unwrap().to_string(), "-8");
        assert_eq!(parse("11/2").unwrap().eval_default().unwrap().to_string(), "5");
        assert_eq!(parse("(-11)/(-2)").unwrap().eval_default().unwrap().to_string(), "5");
        assert_eq!(parse("1/2").unwrap().eval_default().unwrap().to_string(), "0");
        assert!(parse("1/0").unwrap().eval_default().is_err());
        assert_eq!(parse("10^30").unwrap().eval_default().unwrap().to_string(), "1000000000000000000000000000000");
        assert_eq!(parse("0^0").unwrap().eval_default().unwrap().to_string(), "1");
        assert_eq!(parse("0^1").unwrap().eval_default().unwrap().to_string(), "0");
        assert!(parse("1^(-1)").unwrap().eval_default().is_err());

        assert_eq!(parse("'a'+'b'+'c'").unwrap().eval_default().unwrap().to_string(), "'f'");
        assert_eq!(parse("'E'+3+'a'").unwrap().eval_default().unwrap().to_string(), "'I'");
        assert_eq!(parse("'x'+'Y'+'z'").unwrap().eval_default().unwrap().to_string(), "'w'");
        assert!(parse("1+'a'").unwrap().eval_default().is_err());

        assert_eq!(parse("1..5+3+[0,10,20]").unwrap().eval_default().unwrap().to_string(), "[4, 15, 26]");
        assert_eq!(parse("1..3+3+seq").unwrap().eval_default().unwrap().to_string(), "[5, 7, 9]");
        assert_eq!(parse("'A'..'e'+3+[0,10,20]").unwrap().eval_default().unwrap().to_string(), "['D', 'O', 'Z']");
        assert_eq!(parse("(1..5+3+[]).len").unwrap().eval_default().unwrap().to_string(), "0");
        assert_eq!(parse("(1..5+3+seq).len").unwrap().eval_default().unwrap().to_string(), "5");
        assert_eq!(parse(r#"['a','b','c']+"def""#).unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq+true").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert!(parse("true+false").unwrap().eval_default().is_err());
        assert_eq!(parse("[1,[2,[3]]]+1").unwrap().eval_default().unwrap().to_string(), "[2, [3, [4]]]");
        assert_eq!(parse("['a',['b',['c']]]+[1,2]").unwrap().eval_default().unwrap().to_string(), "['b', ['d', ['e']]]");
        assert_eq!(parse("['b','b',2]-[1,'a','a']").unwrap().eval_default().unwrap().to_string(), "['a', 'a', <!>");
        assert_eq!(parse("-[1,[1,'a']]").unwrap().eval_default().unwrap().to_string(), "[-1, [-1, <!>");
        assert_eq!(parse("[2,'b','b']*[2,2,'b']").unwrap().eval_default().unwrap().to_string(), "[4, 'd', <!>");
        assert!(parse("2*'b'").unwrap().eval_default().is_err());
        assert!(parse("1/'a'").unwrap().eval_default().is_err());
        assert!(parse("'a'/1").unwrap().eval_default().is_err());
        assert_eq!(parse("[2,'b','b']*[2,2,'b']").unwrap().eval_default().unwrap().to_string(), "[4, 'd', <!>");
        assert_eq!(parse("seq^seq").unwrap().eval_default().unwrap().to_string(), "[1, 4, 27, 256, 3125, ...]");
        assert_eq!(parse("plus@range(10)").unwrap().eval_default().unwrap().to_string(), "55");
        assert_eq!(parse("times@range(10)").unwrap().eval_default().unwrap().to_string(), "3628800");
        assert_eq!(parse("plus@range(1)").unwrap().eval_default().unwrap().to_string(), "1");
        assert!(parse("plus@range(0)").unwrap().eval_default().is_err());

        assert_eq!(parse("\"A\"+1").unwrap().eval_default().unwrap().to_string(), "\"B\"");
        assert!(parse("1+\"a\"").unwrap().eval_default().is_err());
        assert_eq!(parse("\"AbC\"+3+[0,10,20]").unwrap().eval_default().unwrap().to_string(), "\"DoZ\"");
        assert_eq!(parse(r#""abc"+['d',5,true]"#).unwrap().eval_default().unwrap().to_string(), "\"eg<!>");
        assert_eq!(parse("\"xyz\"+'a'").unwrap().eval_default().unwrap().to_string(), "\"yza\"");
        assert_eq!(parse(r#""ahoj"+"bebe""#).unwrap().eval_default().unwrap().to_string(), "\"cmqo\"");
        assert_eq!(parse(r#""ahoj"-"bebe""#).unwrap().eval_default().unwrap().to_string(), "\"ycme\"");
        assert_eq!(parse(r#""ahoj"-"bebe"+"bcbc""#).unwrap().eval_default().unwrap().to_string(), "\"afoh\"");
        assert_eq!(parse("\"Test\"+13+13").unwrap().eval_default().unwrap().to_string(), "\"Test\"");
        assert_eq!(parse(r#""Hello world!"+[]"#).unwrap().eval_default().unwrap().to_string(), r#""""#);
        assert_eq!(parse(r#""Hello world!"+"ab""#).unwrap().eval_default().unwrap().to_string(), r#""Ig""#);
        assert_eq!(parse(r#""Hello world!"+"ab".repeat"#).unwrap().eval_default().unwrap().to_string(), r#""Igmnp yptmf!""#);
        assert_eq!(parse(r#""Hello world!"+seq"#).unwrap().eval_default().unwrap().to_string(), r#""Igopt cvzun!""#);
        assert_eq!(parse(r#""ab".repeat+seq"#).unwrap().eval_default().unwrap().to_string(), r#""bddffhhjjllnnpprrttv..."#);
        assert_eq!(parse(r#""a b".repeat+seq"#).unwrap().eval_default().unwrap().to_string(), r#""b dd ff hh jj ll nn ..."#);
        assert_eq!(&parse("'u'.repeat+'a'..'c'").unwrap().eval_default().unwrap().to_string(), "\"vwx\"");
        assert_eq!(&parse("' '.repeat+'a'..'c'").unwrap().eval_default().unwrap().to_string(), "\"                    ...");
        assert!(parse(r#"+"ahoj""#).unwrap().eval_default().is_err());
        assert!(parse(r#"-"bebe""#).unwrap().eval_default().is_err());
        assert!(parse(r#""ahoj"*2"#).unwrap().eval_default().is_err());
        assert!(parse(r#"2*"ahoj""#).unwrap().eval_default().is_err());

        assert_eq!(parse("((1..4).repeat+(1..5).repeat)[10^10]").unwrap().eval_default().unwrap().to_string(), "9");
        test_len_exact(&parse("[1,2,3]+seq+5").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("5+[1,2,3]+seq+5").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("[1,2,3]+seq+[5]").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("[1,2,3]+[]+seq").unwrap().eval_default().unwrap(), 0);
        test_skip_n(&parse("range(10^10)+seq+5").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^10)+range(10^11)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq+[]").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq*seq").unwrap().eval_default().unwrap());

        assert_eq!(parse("1+2+3+4-5*6*7/8").unwrap().eval_default().unwrap().describe(), "-16");
        assert_eq!(parse("1+2+3+4-5*6*7/8").unwrap().describe(), "(1+2+3+4)-(5*6*7)/8");
        assert_eq!(parse("1+(2+3)").unwrap().describe(), "1+(2+3)");

        test_len_exact(&parse("\"abc\"+seq").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("\"a b c!\"+1..3+1").unwrap().eval_default().unwrap(), 6);
        test_len_exact(&parse("\"\"+seq").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("'x'.repeat+'a'..'c'").unwrap().eval_default().unwrap(), 3);
        test_skip_n(&parse(r#""abcdefghijk"+seq+"abcdefghijklmn""#).unwrap().eval_default().unwrap());
        test_skip_n(&parse(r#""ab".repeat(10)+seq"#).unwrap().eval_default().unwrap());
        test_skip_n(&parse(r#""a b".repeat(10)+seq"#).unwrap().eval_default().unwrap());
        test_skip_n(&parse(r#""a b".repeat(10)+range('a','e')"#).unwrap().eval_default().unwrap());
        test_skip_n(&parse("'u'.repeat+'a'..'c'").unwrap().eval_default().unwrap());
        test_skip_n(&parse("' '.repeat+'a'..'c'").unwrap().eval_default().unwrap());
        assert_eq!(&parse("(('a'.repeat+\"bc\")~\"xyz\")[2]").unwrap().eval_default().unwrap().to_string(), "'d'");
        assert_eq!(&parse("(('a'.repeat+\"bc\")~\"xyz\")[3]").unwrap().eval_default().unwrap().to_string(), "'x'");
        assert_eq!(&parse("(('a'.repeat+\"bc\")~\"xyz\")[4]").unwrap().eval_default().unwrap().to_string(), "'y'");
        assert_eq!(&parse("' '.repeat+'a'..'c'").unwrap().eval_default().unwrap().as_stream().unwrap().length(), 
            &Length::Unknown);
        assert_eq!(&parse("' '.repeat+1").unwrap().eval_default().unwrap().as_stream().unwrap().length(), 
            &Length::Infinite);
        assert_eq!(&parse("\"a b c\"+'a'..'c'").unwrap().eval_default().unwrap().as_stream().unwrap().length(), 
            &Length::AtMost(5usize.into()));
        assert_eq!(&parse("\"a b c\"+'a'").unwrap().eval_default().unwrap().as_stream().unwrap().length(), 
            &Length::Exact(5usize.into()));

        assert_eq!(parse("\"AbC\"+3+[0,10,20]").unwrap().eval_default().unwrap().describe(), "\"AbC\"+3+[0, 10, 20]");
        assert_eq!(parse("\"a b c!\"+1..3+1").unwrap().eval_default().unwrap().describe(), "\"a b c!\"+1..3+1");
        assert_eq!(parse("alpha(\"bac\", \"abc\"+1)").unwrap().eval_default().unwrap().to_string(), "\"cab\"");
        assert_eq!(parse("alpha(\"bac\", \"abc\"+1)").unwrap().eval_default().unwrap().describe(), "alpha(['b', 'a', 'c'], \"abc\"+1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("+", eval_op);
    keywords.insert("plus", eval_op);
    keywords.insert("-", eval_op);
    keywords.insert("*", eval_op);
    keywords.insert("times", eval_op);
    keywords.insert("/", eval_op);
    keywords.insert("^", eval_op);
}
