use crate::base::*;

#[derive(Clone)]
struct MathOp {
    node: ENode,
    func: MathFunc,
    alpha: Rc<Alphabet>,
}

type MathFunc = fn(&[Item], &Rc<Alphabet>) -> Result<Item, BaseError>;

fn eval_op(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    match try_with!(node, node.first_arg_checked()?) {
        Item::String(_) => StringOp::eval(node, env),
        _ => MathOp::eval(node, env)
    }
}

impl MathOp {
    fn eval(node: ENode, env: &Env) -> Result<Item, StreamError> {
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
        match head.as_str().expect("head should be symbol or oper") {
            "+" => Self::plus_func,
            "plus" => Self::plus_func,
            "-" => Self::minus_func,
            "*" => Self::mul_func,
            "times" => Self::mul_func,
            "/" => Self::div_func,
            "%" => Self::mod_func,
            "^" => Self::pow_func,
            sym => panic!("mathops: unhandled head '{sym}'")
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
                let index = alpha.ord(ch)?;
                let case = ch.case();
                let ans = iter.try_fold(index.into(),
                    |a, e| {
                        match e {
                            Item::Number(ref num) => Ok(a + num),
                            Item::Char(ref ch) => Ok(a + alpha.ord(ch)?),
                            _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                        }
                    })?;
                Ok(Item::new_char(alpha.chr(&ans, case)))
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
                    let index = alpha.ord(ch)?;
                    let case = ch.case();
                    let ord = match rhs {
                        Item::Number(ref num) => index - num,
                        Item::Char(ref ch) => (index - alpha.ord(ch)?).into(),
                        _ => return Err(format!("expected number or character, found {:?}", rhs).into())
                    };
                    Ok(Item::new_char(alpha.chr(&ord, case)))
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
                let index = alpha.ord(ch)?;
                let case = ch.case();
                let ans = iter.try_fold(index.into(), |a, e| e.as_num().map(|num| a * num))?;
                Ok(Item::new_char(alpha.chr(&ans, case)))
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

    fn mod_func(items: &[Item], _alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        use num::traits::Euclid;
        match items {
            [lhs, rhs] => {
                let (lhs, rhs) = (lhs.as_num()?, rhs.as_num()?);
                if rhs.is_zero() {
                    Err("division by zero".into())
                } else {
                    Ok(Item::new_number(lhs.rem_euclid(rhs)))
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
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        self.node.describe_inner(prec, env)
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

    fn len(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.len(),
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
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let mut remain = UNumber::zero();
        for iter in &mut self.args {
            if let Some(r) = iter.advance(n.clone())? {
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
    first: BoxedStream<Char>,
    node_rem: ENode,
    func: StringFunc,
    alpha: Rc<Alphabet>,
}

type StringFunc = fn(&Char, &[Item], &Rc<Alphabet>) -> Result<Char, BaseError>;

impl StringOp {
    fn eval(mut node: ENode, env: &Env) -> Result<Item, StreamError> {
        let func = try_with!(node, Self::find_fn(&node.head)?);
        let alpha = env.alphabet();
        if node.args.len() < 2 {
            return Err(StreamError::new("not available for strings", node));
        }
        let Item::String(first) = node.args.remove(0) else { unreachable!() };
        Ok(Item::new_string(StringOp{first: first.into(), node_rem: node, func, alpha: Rc::clone(alpha)}))
    }

    fn find_fn(head: &Head) -> Result<StringFunc, BaseError> {
        match head.as_str().expect("head should be symbol or oper") {
            "+" => Ok(Self::plus_func),
            "plus" => Ok(Self::plus_func),
            "-" => Ok(Self::minus_func),
            sym => Err(format!("operation {sym} not available for strings").into())
        }
    }

    fn plus_func(first: &Char, rest: &[Item], alpha: &Rc<Alphabet>) -> Result<Char, BaseError> {
        let index = alpha.ord(first)?;
        let case = first.case();
        let ans = rest.iter().try_fold(index.into(),
            |a, e| {
                match e {
                    Item::Number(ref num) => Ok(a + num),
                    Item::Char(ref ch) => Ok(a + alpha.ord(ch)?),
                    _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                }
            })?;
        Ok(alpha.chr(&ans, case))
    }

    fn minus_func(first: &Char, rest: &[Item], alpha: &Rc<Alphabet>) -> Result<Char, BaseError> {
        let index = alpha.ord(first)?;
        let case = first.case();
        let ord = match rest {
            [other] => match other {
                Item::Number(ref num) => index - num,
                Item::Char(ref ch) => (index - alpha.ord(ch)?).into(),
                _ => return Err(BaseError::from(format!("expected number or character, found {:?}", other)))
            },
            _ => return Err("not available for strings".into())
        };
        Ok(alpha.chr(&ord, case))
    }
}

impl Describe for StringOp {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_alpha(
            &self.alpha,
            &self.node_rem.head,
            None::<&Item>,
            std::iter::once(ProxyItem::String(&*self.first))
                .chain(self.node_rem.args.iter().map(ProxyItem::from)),
            prec, env)
    }
}

impl Stream<Char> for StringOp {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        let first = self.first.iter();
        let rest = self.node_rem.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                Item::String(stm) => stm.map_iter(|ch| Ok(Item::Char(ch))),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(StringOpIter{first, rest, alpha: &self.alpha, func: self.func})
    }

    fn len(&self) -> Length {
        self.node_rem.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.len(),
                Item::String(stm) => stm.len(),
                _ => Length::Infinite
            })
            .map(|len| match len {
                Length::Infinite => Length::Infinite,
                _ => Length::Unknown
            })
            .fold(self.first.len(), Length::intersection)
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
    first: Box<dyn SIterator<Char> + 'node>,
    rest: Vec<Box<dyn SIterator + 'node>>,
    alpha: &'node Rc<Alphabet>,
    func: StringFunc
}

impl Iterator for StringOpIter<'_> {
    type Item = Result<Char, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(base: Char, mut inputs: Vec<Item>) -> Node {
            inputs.insert(0, Item::Char(base));
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }

        let ch = iter_try_expr!(self.first.next()?);
        if !self.alpha.contains(&ch) {
            return Some(Ok(ch));
        }

        let rest = self.rest.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>();
        let inputs = iter_try_expr!(rest?);
        let res = (self.func)(&ch, &inputs, self.alpha);
        Some(res.map_err(|err| StreamError::new(err, aux_node(ch, inputs))))
    }
}

impl SIterator<Char> for StringOpIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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
            if let Some(r) = iter.advance(n_chars.clone())? {
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

        test_eval!("1+(-2)" => "-1");
        test_eval!("(-1)-(-2)" => "1");
        test_eval!("2*(-4)" => "-8");
        test_eval!("11/2" => "5");
        test_eval!("(-11)/(-2)" => "5");
        test_eval!("1/2" => "0");
        test_eval!("1/0" => err);
        test_eval!("10^30" => "1000000000000000000000000000000");
        test_eval!("0^0" => "1");
        test_eval!("0^1" => "0");
        test_eval!("1^(-1)" => err);
        test_eval!("157%10" => "7");
        test_eval!("(-157)%10" => "3");
        test_eval!("(-157)%(-10)" => "3");

        test_eval!("'a'+'b'+'c'" => "'f'");
        test_eval!("'E'+3+'a'" => "'I'");
        test_eval!("'x'+'Y'+'z'" => "'w'");
        test_eval!("1+'a'" => err);

        test_eval!("1..5+3+[0,10,20]" => "[4, 15, 26]");
        test_eval!("1..3+3+seq" => "[5, 7, 9]");
        test_eval!("'A'..'e'+3+[0,10,20]" => "['D', 'O', 'Z']");
        test_eval!("(1..5+3+[]).len" => "0");
        test_eval!("(1..5+3+seq).len" => "5");
        test_eval!(r#"['a','b','c']+"def""# => "[<!>");
        test_eval!("seq+true" => "[<!>");
        test_eval!("true+false" => err);
        test_eval!("[1,[2,[3]]]+1" => "[2, [3, [4]]]");
        test_eval!("['a',['b',['c']]]+[1,2]" => "['b', ['d', ['e']]]");
        test_eval!("['b','b',2]-[1,'a','a']" => "['a', 'a', <!>");
        test_eval!("-[1,[1,'a']]" => "[-1, [-1, <!>");
        test_eval!("[2,'b','b']*[2,2,'b']" => "[4, 'd', <!>");
        test_eval!("2*'b'" => err);
        test_eval!("1/'a'" => err);
        test_eval!("'a'/1" => err);
        test_eval!("[2,'b','b']*[2,2,'b']" => "[4, 'd', <!>");
        test_eval!("seq^seq" => "[1, 4, 27, 256, 3125, ...]");
        test_eval!("plus@range(10)" => "55");
        test_eval!("times@range(10)" => "3628800");
        test_eval!("plus@range(1)" => "1");
        test_eval!("plus@range(0)" => err);

        test_eval!("\"A\"+1" => "\"B\"");
        test_eval!("1+\"a\"" => err);
        test_eval!("\"AbC\"+3+[0,10,20]" => "\"DoZ\"");
        test_eval!(r#""abc"+['d',5,true]"# => "\"eg<!>");
        test_eval!("\"xyz\"+'a'" => "\"yza\"");
        test_eval!(r#""ahoj"+"bebe""# => "\"cmqo\"");
        test_eval!(r#""ahoj"-"bebe""# => "\"ycme\"");
        test_eval!(r#""ahoj"-"bebe"+"bcbc""# => "\"afoh\"");
        test_eval!("\"Test\"+13+13" => "\"Test\"");
        test_eval!(r#""Hello world!"+[]"# => r#""""#);
        test_eval!(r#""Hello world!"+"ab""# => r#""Ig""#);
        test_eval!(r#""Hello world!"+"ab".repeat"# => r#""Igmnp yptmf!""#);
        test_eval!(r#""Hello world!"+seq"# => r#""Igopt cvzun!""#);
        test_eval!(r#""ab".repeat+seq"# => r#""bddffhhjjllnnpprrttv..."#);
        test_eval!(r#""a b".repeat+seq"# => r#""b dd ff hh jj ll nn ..."#);
        test_eval!("'u'.repeat+'a'..'c'" => "\"vwx\"");
        test_eval!("' '.repeat+'a'..'c'" => "\"                    ...");
        test_eval!(r#"+"ahoj""# => err);
        test_eval!(r#"-"bebe""# => err);
        test_eval!(r#""ahoj"*2"# => err);
        test_eval!(r#"2*"ahoj""# => err);

        test_eval!("((1..4).repeat+(1..5).repeat)[10^10]" => "9");
        test_len!("[1,2,3]+seq+5" => 3);
        test_len!("5+[1,2,3]+seq+5" => 3);
        test_len!("[1,2,3]+seq+[5]" => 1);
        test_len!("[1,2,3]+[]+seq" => 0);
        test_advance("range(10^10)+seq+5");
        test_advance("range(10^10)+range(10^11)");
        test_advance("seq+[]");
        test_advance("seq*seq");

        test_describe!("1+2+3+4-5*6*7/8" => "-16");
        assert_eq!(parse("1+2+3+4-5*6*7/8").unwrap().describe(), "(1+2+3+4)-(5*6*7)/8");
        assert_eq!(parse("1+(2+3)").unwrap().describe(), "1+(2+3)");

        test_len!("\"abc\"+seq" => 3);
        test_len!("\"a b c!\"+1..3+1" => 6);
        test_len!("\"\"+seq" => 0);
        test_len!("'x'.repeat+'a'..'c'" => 3);
        test_advance(r#""abcdefghijk"+seq+"abcdefghijklmn""#);
        test_advance(r#""ab".repeat(10)+seq"#);
        test_advance(r#""a b".repeat(10)+seq"#);
        test_advance(r#""a b".repeat(10)+range('a','e')"#);
        test_advance("'u'.repeat+'a'..'c'");
        test_advance("' '.repeat+'a'..'c'");
        test_eval!("(('a'.repeat+\"bc\")~\"xyz\")[2]" => "'d'");
        test_eval!("(('a'.repeat+\"bc\")~\"xyz\")[3]" => "'x'");
        test_eval!("(('a'.repeat+\"bc\")~\"xyz\")[4]" => "'y'");
        test_len!("' '.repeat+'a'..'c'" => Length::Unknown);
        test_len!("' '.repeat+1" => Length::Infinite);
        test_len!("\"a b c\"+'a'..'c'" => Length::AtMost(5usize.into()));
        test_len!("\"a b c\"+'a'" => Length::Exact(5usize.into()));

        test_describe!("\"AbC\"+3+[0,10,20]" => "\"AbC\"+3+[0, 10, 20]");
        test_describe!("\"a b c!\"+1..3+1" => "\"a b c!\"+1..3+1");
        test_eval!("alpha(\"bac\", \"abc\"+1)" => "\"cab\"");
        test_describe!("alpha(\"bac\", \"abc\"+1)" => "alpha(['b', 'a', 'c'], \"abc\"+1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("+", eval_op);
    keywords.insert("plus", eval_op);
    keywords.insert("-", eval_op);
    keywords.insert("*", eval_op);
    keywords.insert("times", eval_op);
    keywords.insert("/", eval_op);
    keywords.insert("%", eval_op);
    keywords.insert("^", eval_op);
}
