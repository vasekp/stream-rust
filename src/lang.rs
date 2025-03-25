use crate::base::*;
use crate::alphabet::*;
use crate::utils::{NumWithin, TriState};
use std::rc::Rc;


/// A `Stream` formed by direct enumeration of its `Item`s.
#[derive(Clone)]
pub struct List(Vec<Item>);

impl List {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        Ok(Item::new_stream(List::from(node.args)))
    }
}

impl Stream for List {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.0.iter().map(|x| Ok(x.clone())))
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for List {
    fn describe(&self) -> String {
        let mut ret = String::new();
        ret.push('[');
        let mut it = self.0.iter().map(Describe::describe);
        if let Some(s) = it.next() {
            ret += &s;
        }
        for s in it {
            ret += ", ";
            ret += &s;
        }
        ret.push(']');
        ret
    }
}

impl From<Vec<Item>> for List {
    fn from(vec: Vec<Item>) -> List {
        List(vec)
    }
}

#[test]
fn test_list() {
    use crate::parser::parse;
    assert_eq!(parse("[1,2,3]").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    test_len_exact(&parse("[1,2,3]").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[1]").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("[]").unwrap().eval().unwrap(), 0);
    test_skip_n(&parse("[1,2,3]").unwrap().eval().unwrap());
    test_skip_n(&parse("[1]").unwrap().eval().unwrap());
    test_skip_n(&parse("[]").unwrap().eval().unwrap());
}


#[derive(Clone)]
pub struct LiteralString(Vec<Char>);

impl Stream for LiteralString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(self.0.iter().map(|x| Ok(Item::new_char(x.clone()))))
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for LiteralString {
    fn describe(&self) -> String {
        let mut ret = String::new();
        ret.push('"');
        for ch in &self.0 {
            ret += &format!("{ch:#}");
        }
        ret.push('"');
        ret
    }
}

impl From<String> for LiteralString {
    fn from(s: String) -> Self {
        LiteralString(s.chars().map(Char::from).collect())
    }
}


#[derive(Clone)]
pub struct Part {
    source: BoxedStream,
    indices: BoxedStream,
    rest: Vec<Expr>,
    env: Rc<Env>
}

impl Part {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        use once_cell::unsync::Lazy;
        let mut node = node.eval_source(env)?;
        let source = try_with!(node, node.source_checked()?.as_item()?.to_stream()?);
        let length = Lazy::new(|| match source.length() {
            Length::Exact(len) => Ok(len),
            Length::Infinite => Err(BaseError::from("stream is infinite")),
            _ => Ok(source.iter().count().into())
        });
        type R = Result<Number, BaseError>;
        fn subs_len(expr: &mut Expr, length: &Lazy<R, impl Fn() -> R>) ->
            Result<(), BaseError>
        {
            match expr {
                Expr::Imm(_) => Ok(()),
                Expr::Eval(node) => {
                    if &node.head == "len" && node.source.is_none() {
                        match Lazy::force(length) {
                            Ok(len) => *expr = Expr::new_number(len.to_owned()),
                            Err(err) => return Err(err.clone())
                        }
                        return Ok(());
                    }
                    node.source.iter_mut().try_for_each(|sbox| subs_len(sbox, length))?;
                    match &mut node.head {
                        Head::Lang(LangItem::Part) => return Ok(()), // $part does not enter into args
                        Head::Block(expr) => subs_len(expr, length)?,
                        Head::Args(head) => {
                            if let Head::Block(ref mut expr) = **head {
                                subs_len(expr, length)?
                            }
                        },
                        _ => ()
                    }
                    node.args.iter_mut().try_for_each(|arg| subs_len(arg, length))?;
                    Ok(())
                }
            }
        }
        try_with!(node, subs_len(node.first_arg_checked_mut()?, &length)?);
        let first = node.args.remove(0).eval_env(env)?;
        match first {
            Item::Number(index) => {
                macro_rules! orig_node {
                    () => { {
                        node.args.insert(0, Expr::new_number(index));
                        node
                    } }
                }
                try_with!(orig_node!(), index.check_within(Number::one()..)?);
                match source.length() {
                    Length::Exact(len) | Length::AtMost(len) if len < index =>
                        return Err(StreamError::new("index past end of stream", orig_node!())),
                    _ => ()
                }
                let mut iter = source.iter();
                if iter.skip_n(&(&index - 1))?.is_some() {
                    return Err(StreamError::new("index past end of stream", orig_node!()));
                }
                let item = match iter.next() {
                    Some(value) => value?,
                    None => return Err(StreamError::new("index past end of stream", orig_node!()))
                };
                if node.args.is_empty() {
                    Ok(item)
                } else {
                    Part::eval(Node::new(node.head, Some(item.into()), node.args), env)
                }
            },
            Item::Stream(indices) => {
                Ok(Item::new_stream(Part{source: source.into(), indices: indices.into(), rest: node.args, env: Rc::clone(env)}))
            },
            item => {
                node.args.insert(0, item.into());
                Err(StreamError::new(format!("expected number or stream, found {:?}", node.args[0]), node))
            }
        }
    }
}

impl Stream for Part {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PartIter{parent: self, iter: self.indices.iter()})
    }
}

impl Describe for Part {
    fn describe(&self) -> String {
        let mut args = self.rest.clone();
        let source = self.source.to_expr();
        args.insert(0, self.indices.to_expr());
        Node::describe_helper(&Head::Lang(LangItem::Part), Some(&source), &args)
    }
}

struct PartIter<'node> {
    parent: &'node Part,
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for PartIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let part = self.iter.next()?;
        let Ok(part) = part else {
            return Some(part);
        };
        // TODO: smarter - number tracks increments, stream unfolds?
        let mut args = self.parent.rest.clone();
        args.insert(0, Expr::Imm(part));
        let node = Node::new(LangItem::Part, Some(self.parent.source.to_expr()), args);
        Some(Part::eval(node, &self.parent.env))
    }
}

impl SIterator for PartIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        self.iter.skip_n(n)
    }
}


#[test]
fn test_part() {
    use crate::parser::parse;
    assert_eq!(parse("range(3)[1]").unwrap().eval().unwrap().to_string(), "1");
    assert_eq!(parse("range(3)[3]").unwrap().eval().unwrap().to_string(), "3");
    assert!(parse("range(3)[4]").unwrap().eval().is_err());
    assert!(parse("range(3)[10]").unwrap().eval().is_err());
    assert!(parse("range(3)[0]").unwrap().eval().is_err());
    assert!(parse("range(3)[-1]").unwrap().eval().is_err());
    assert_eq!(parse("[[1,2],[3,4]][2,1]").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("[[1,2],[3,4]][2][1]").unwrap().eval().unwrap().to_string(), "3");

    assert_eq!(parse("seq(5,2)[100...]").unwrap().eval().unwrap().to_string(), "[203, 203, 203, 203, 203, ...]");
    assert_eq!(parse("seq(5,2)[2*seq+1]").unwrap().eval().unwrap().to_string(), "[9, 13, 17, 21, 25, ...]");
    assert_eq!(parse("seq[seq][seq]").unwrap().eval().unwrap().to_string(), "[1, 2, 3, 4, 5, ...]");
    assert_eq!(parse("seq[seq, seq]").unwrap().eval().unwrap().to_string(), "[<!>");
    assert_eq!(parse("seq:{seq^#}[seq,4]").unwrap().eval().unwrap().to_string(), "[4, 16, 64, 256, 1024, ...]");
    assert_eq!(parse("seq:{seq^#}[seq][4]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
    assert_eq!(parse("seq:{seq^#}[4,seq]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
    assert_eq!(parse("seq:{seq^#}[4][seq]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
    assert_eq!(parse("seq:{seq^#}[[1,2],[1,2,3]]").unwrap().eval().unwrap().to_string(), "[[1, 2, 3], [...]]");
    assert!(parse("seq[2,5]").unwrap().eval().is_err());
    assert_eq!(parse("seq[[2,5]]").unwrap().eval().unwrap().to_string(), "[2, 5]");
    assert_eq!(parse("seq[[[2,5]]]").unwrap().eval().unwrap().to_string(), "[[2, 5]]"); // subject to change

    assert_eq!(parse("[1,2,3][len]").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("[1,2,3][[len]]").unwrap().eval().unwrap().to_string(), "[3]");
    assert_eq!(parse("[1,2,3][len-seq]").unwrap().eval().unwrap().to_string(), "[2, 1, <!>");
    assert_eq!(parse("[[1], [1,2,3]][len, len]").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("[[1], [1,2,3]][1..2, len]").unwrap().eval().unwrap().to_string(), "[1, 3]");
    assert_eq!(parse("[1,2,3][[1,2].len]").unwrap().eval().unwrap().to_string(), "2");
    assert!(parse("[1,2,3][seq[len]]").unwrap().eval().is_err());
    assert_eq!(parse("[1,2,3][range(len-1)[len]]").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("[1,2,3,4][{#1..(#1+1)}(len/2)]").unwrap().eval().unwrap().to_string(), "[2, 3]");
    assert_eq!(parse("[1,2,3][{len}]").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("[1,2,3][{len}@[]]").unwrap().eval().unwrap().to_string(), "3");
}


#[derive(Clone)]
struct Map {
    source: BoxedStream,
    body: Node,
    env: Rc<Env>
}

struct MapIter<'node> {
    parent: &'node Map,
    source: Box<dyn SIterator + 'node>
}

impl Map {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_source(env)?;
        let source = try_with!(node, node.source_checked()?.as_item()?.to_stream()?);
        let body = match node.args[..] {
            [ref expr] => try_with!(node, expr.to_node()?),
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        if body.source.is_some() {
            return Err(StreamError::new("body already has source", node));
        }
        Ok(Item::new_stream(Map{source: source.into(), body, env: Rc::clone(env)}))
    }
}

impl Describe for Map {
    fn describe(&self) -> String {
        let mut ret = self.source.describe();
        ret.push(':');
        ret += &self.body.describe();
        self.env.wrap_describe(ret)
    }
}

impl Stream for Map {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(MapIter{parent: self, source: self.source.iter()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Iterator for MapIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source.next()?;
        let Ok(source) = source else {
            return Some(source)
        };
        let expr = Expr::Eval(Node{
            source: Some(Box::new(source.into())),
            head: self.parent.body.head.clone(),
            args: self.parent.body.args.clone()
        });
        Some(expr.eval_env(&self.parent.env))
    }
}

impl SIterator for MapIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        self.source.skip_n(n)
    }
}

#[test]
fn test_map() {
    use crate::parser::parse;
    assert_eq!(parse("[1,2,3]:{#*10}").unwrap().eval().unwrap().to_string(), "[10, 20, 30]");
    assert_eq!(parse("seq:{#^2}").unwrap().eval().unwrap().to_string(), "[1, 4, 9, 16, 25, ...]");
    assert_eq!(parse("seq:{#1}").unwrap().eval().unwrap().to_string(), "[<!>");
    assert_eq!(parse("seq:{range(#)}").unwrap().eval().unwrap().to_string(), "[[1], [1, 2], ...]");
    test_len_exact(&parse("[1,2,3]:{#}").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[]:{#}").unwrap().eval().unwrap(), 0);
    test_skip_n(&parse("range(10^10):{#}").unwrap().eval().unwrap());
    test_skip_n(&parse("seq:{#}").unwrap().eval().unwrap());
}


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
        assert!(!items.is_empty());
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
    fn describe(&self) -> String {
        self.env.wrap_describe(self.node.describe())
    }
}

impl Stream for MathOp {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(Forever{item})
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
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        let mut remain = Number::zero();
        for iter in &mut self.args {
            if let Some(r) = iter.skip_n(n)? {
                remain = std::cmp::max(remain, r);
            }
        }
        if remain.is_zero() { Ok(None) }
        else { Ok(Some(remain)) }
    }
}

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
    test_skip_n(&parse("range(10^10)+seq+5").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)+range(10^11)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq+[]").unwrap().eval().unwrap());
    test_skip_n(&parse("seq*seq").unwrap().eval().unwrap());
}


#[derive(Clone)]
struct Join {
    node: ENode,
    string: bool
}

impl Join {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);

        use crate::utils::TriState;
        fn is_string(item: &Item) -> TriState {
            match item {
                Item::Stream(stm) => stm.is_string(),
                Item::Char(_) => TriState::Either,
                _ => TriState::False
            }
        }

        let string = try_with!(node, node.args.iter()
            .map(is_string)
            .try_fold(TriState::Either, TriState::join)
            .map_err(|()| BaseError::from("mixed strings and non-strings"))?)
            .is_true();
        Ok(Item::new_stream(Join{node, string}))
    }
}

impl Describe for Join {
    fn describe(&self) -> String {
        self.node.describe()
    }
}

impl Stream for Join {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let first = match &self.node.args[0] {
            Item::Stream(stm) => stm.iter(),
            item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
        };
        Box::new(JoinIter{node: &self.node, index: 0, cur: first})
    }

    fn is_string(&self) -> TriState {
        self.string.into()
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::from(1)
            })
            .reduce(|acc, e| acc + e).unwrap() // args checked to be nonempty in eval()
    }
}

struct JoinIter<'node> {
    node: &'node ENode,
    index: usize,
    cur: Box<dyn SIterator + 'node>
}

impl Iterator for JoinIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.cur.next();
            if next.is_some() {
                return next;
            } else {
                self.index += 1;
                self.cur = match self.node.args.get(self.index)? {
                    Item::Stream(stm) => stm.iter(),
                    item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
                };
            }
        }
    }
}

impl SIterator for JoinIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        assert!(!n.is_negative());
        let mut n = n.to_owned();
        loop {
            let Some(m) = self.cur.skip_n(&n)?
                else { return Ok(None); };
            n = m;
            self.index += 1;
            let Some(next) = self.node.args.get(self.index)
                else { return Ok(Some(n)); };
            self.cur = match next {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
            };
        }
    }
}

#[test]
fn test_join() {
    use crate::parser::parse;

    assert_eq!(parse("[10]~seq").unwrap().eval().unwrap().to_string(), "[10, 1, 2, 3, 4, ...]");
    assert_eq!(parse("range(2)~seq").unwrap().eval().unwrap().to_string(), "[1, 2, 1, 2, 3, ...]");
    assert_eq!(parse("range(10^10).{#~#~#}.len").unwrap().eval().unwrap().to_string(), "30000000000");
    assert!(parse("([5]~seq).len").unwrap().eval().is_err());
    assert_eq!(parse("(range(10^10)~seq)[10^11]").unwrap().eval().unwrap().to_string(), "90000000000");

    assert_eq!(parse("(\"ab\"~\"cd\").len").unwrap().eval().unwrap().to_string(), "4");

    assert_eq!(parse("[1]~[2]").unwrap().eval().unwrap().to_string(), "[1, 2]");
    assert_eq!(parse("[1]~[[2]]").unwrap().eval().unwrap().to_string(), "[1, [2]]");
    assert_eq!(parse("[1]~2~'c'").unwrap().eval().unwrap().to_string(), "[1, 2, 'c']");
    assert_eq!(parse("1~2~3").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("10~seq").unwrap().eval().unwrap().to_string(), "[10, 1, 2, 3, 4, ...]");
    assert_eq!(parse("(0~1..0~2)").unwrap().eval().unwrap().to_string(), "[0, 2]");
    assert_eq!(parse("(0~1..3~4)[3]").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("(0~1..3~4)[4]").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("\"ab\"~\"cd\"").unwrap().eval().unwrap().to_string(), "\"abcd\"");
    assert_eq!(parse("\"ab\"~'c'").unwrap().eval().unwrap().to_string(), "\"abc\"");
    assert_eq!(parse("'a'~\"b\"~'c'").unwrap().eval().unwrap().to_string(), "\"abc\"");
    assert!(parse("\"a\"~1").unwrap().eval().is_err());
    assert!(parse("\"a\"~[1]").unwrap().eval().is_err());
    assert!(parse("\"a\"~['b']").unwrap().eval().is_err());
    assert!(parse("[1]~\"a\"").unwrap().eval().is_err());

    test_len_exact(&parse("[1,2,3]~4~[5]~[[5,6]]").unwrap().eval().unwrap(), 6);
    test_len_exact(&parse("1~2~3").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("0~1..2~3").unwrap().eval().unwrap(), 4);
    test_len_exact(&parse("0~1..0~3").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("\"ab\"~\"cd\"").unwrap().eval().unwrap(), 4);
    test_len_exact(&parse("\"ab\"~'ch'").unwrap().eval().unwrap(), 3);
    test_skip_n(&parse("range(10^10)~range(10^9)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)~range(-10^10)~range(10^9)").unwrap().eval().unwrap());
    test_skip_n(&parse("('a'..'z').repeat(10^10)~['A'].repeat(10^10)").unwrap().eval().unwrap());
}


pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$list", List::eval);
    keywords.insert("$part", Part::eval);
    keywords.insert("$map", Map::eval);
    keywords.insert("+", MathOp::eval);
    keywords.insert("-", MathOp::eval);
    keywords.insert("*", MathOp::eval);
    keywords.insert("/", MathOp::eval);
    keywords.insert("^", MathOp::eval);
    keywords.insert("~", Join::eval);
}
