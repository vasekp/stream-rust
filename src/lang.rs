#![allow(clippy::redundant_closure_call)]
use crate::base::*;
use crate::alphabet::*;
use crate::utils::NumWithin;
use std::rc::Rc;


/// A `Stream` formed by direct enumeration of its `Item`s.
#[derive(Clone)]
pub struct List(Vec<Item>);

impl List {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
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

    fn is_string(&self) -> bool {
        true
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


fn eval_part(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let mut item = try_with!(node, node.source_checked()).to_owned();
    if node.args.is_empty() {
        return Err(StreamError::new("at least 1 argument required", node));
    }
    for arg in &node.args {
        let index = try_with!(node, arg.as_num());
        try_with!(node, index.check_within(Number::one()..));
        let stm = try_with!(node, item.into_stream());
        match stm.length() {
            Length::Exact(len) | Length::AtMost(len) if &len < index =>
                return Err(StreamError::new("index past end of stream", node)),
            _ => ()
        }
        let mut iter = stm.iter();
        if iter.skip_n(&(index - 1))?.is_some() {
            return Err(StreamError::new("index past end of stream", node));
        }
        item = match iter.next() {
            Some(value) => value?,
            None => return Err(StreamError::new("index past end of stream", node))
        };
    }
    Ok(item)
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
}


struct Map {
    source: Box<dyn Stream>,
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
        let source = try_with!(node, node.source_checked()?.as_item()?.to_stream());
        let body = match node.args.len() {
            1 => try_with!(node, node.args[0].to_node()),
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        if body.source.is_some() {
            return Err(StreamError::new("body already has source", node));
        }
        Ok(Item::new_stream(Map{source, body, env: Rc::clone(env)}))
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

impl Clone for Map {
    fn clone(&self) -> Map {
        Map{source: dyn_clone::clone_box(&*self.source), body: self.body.clone(), env: Rc::clone(&self.env)}
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
    assert_eq!(parse("seq:{#^2}").unwrap().eval().unwrap().to_string(), "[1, 4, 9, ...");
    assert_eq!(parse("seq:{#1}").unwrap().eval().unwrap().to_string(), "[<!>");
    assert_eq!(parse("seq:{range(#)}").unwrap().eval().unwrap().to_string(), "[[1], [1, 2], [1, 2, 3], ...");
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
        try_with!(node, node.check_no_source());
        try_with!(node, node.check_args_nonempty());
        if node.args.iter().any(Item::is_stream) {
            Ok(Item::new_stream(MathOp{node, env: Rc::clone(env), func}))
        } else {
            Ok(try_with!(node, func(&node.args, env)))
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
        match iter.next().unwrap() {
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

    fn minus_func(items: &[Item], _env: &Rc<Env>) -> Result<Item, BaseError> {
        match items {
            [item] => Ok(Item::new_number(-item.as_num()?)),
            [lhs, rhs] => Ok(Item::new_number(lhs.as_num()? - rhs.as_num()?)),
            _ => Err("1 or 2 arguments required".into())
        }
    }

    fn mul_func(items: &[Item], _env: &Rc<Env>) -> Result<Item, BaseError> {
        let ret = items.iter().try_fold(Number::one(), |a, e| e.as_num().map(|e| a * e));
        Ok(Item::new_number(ret?))
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
            }).reduce(|a, e| Length::intersection(&a, &e)).unwrap()
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
            .map(|iter| (*iter).next())
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
            if let Some(r) = (*iter).skip_n(n)? {
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
    assert_eq!(parse(r#""ahoj"+"bebe""#).unwrap().eval().unwrap().to_string(), "['c', 'm', 'q', ...");
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

    assert_eq!(parse("((1..4).repeat+(1..5).repeat)[10^10]").unwrap().eval().unwrap().to_string(), "9");
    test_len_exact(&parse("[1,2,3]+seq+5").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("5+[1,2,3]+seq+5").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[1,2,3]+seq+[5]").unwrap().eval().unwrap(), 1);
    test_skip_n(&parse("range(10^10)+seq+5").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)+range(10^11)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq+[]").unwrap().eval().unwrap());
}


#[derive(Clone)]
struct Join {
    node: ENode
}

struct JoinIter<'node> {
    node: &'node ENode,
    index: usize,
    cur: Box<dyn SIterator + 'node>
}


impl Join {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        try_with!(node, node.check_args_nonempty());
        let mut iter = node.args.iter();
        let string = Join::is_string(iter.next().unwrap());
        for arg in iter {
            if Join::is_string(arg) != string {
                return Err(StreamError::new("mixed strings and non-strings", node));
            }
        }
        Ok(Item::new_stream(Join{node}))
    }

    fn is_string(item: &Item) -> bool {
        match item {
            Item::Stream(stm) => stm.is_string(),
            _ => false
        }
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

    fn is_string(&self) -> bool {
        Join::is_string(&self.node.args[0])
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::from(1)
            })
            .reduce(|acc, e| acc + e).unwrap()
    }
}

impl Iterator for JoinIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = (*self.cur).next();
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

    assert_eq!(parse("[10]~seq").unwrap().eval().unwrap().to_string(), "[10, 1, 2, ...");
    assert_eq!(parse("range(2)~seq").unwrap().eval().unwrap().to_string(), "[1, 2, 1, ...");
    assert_eq!(parse("range(10^10).{#~#~#}.len").unwrap().eval().unwrap().to_string(), "30000000000");
    assert!(parse("([5]~seq).len").unwrap().eval().is_err());
    assert_eq!(parse("(range(10^10)~seq)[10^11]").unwrap().eval().unwrap().to_string(), "90000000000");

    assert_eq!(parse("\"ab\"~\"cd\"").unwrap().eval().unwrap().to_string(), "\"abcd\"");
    assert_eq!(parse("(\"ab\"~\"cd\").len").unwrap().eval().unwrap().to_string(), "4");

    assert_eq!(parse("[1]~[2]").unwrap().eval().unwrap().to_string(), "[1, 2]");
    assert_eq!(parse("[1]~[[2]]").unwrap().eval().unwrap().to_string(), "[1, [2]]");
    assert_eq!(parse("[1]~2~[3]").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("1~2~3").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("0~seq").unwrap().eval().unwrap().to_string(), "[0, 1, 2, ...");
    assert_eq!(parse("(0~1..0~2)").unwrap().eval().unwrap().to_string(), "[0, 2]");
    assert_eq!(parse("(0~1..3~4)[3]").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("(0~1..3~4)[4]").unwrap().eval().unwrap().to_string(), "3");
    assert!(parse("[1]~\"a\"").unwrap().eval().is_err());
    assert!(parse("\"a\"~[1]").unwrap().eval().is_err());
    assert!(parse("\"a\"~['b']").unwrap().eval().is_err());

    test_len_exact(&parse("[1,2,3]~4~[5]~[[5,6]]").unwrap().eval().unwrap(), 6);
    test_len_exact(&parse("1~2~3").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("0~1..2~3").unwrap().eval().unwrap(), 4);
    test_len_exact(&parse("0~1..0~3").unwrap().eval().unwrap(), 2);
    test_skip_n(&parse("range(10^10)~range(10^9)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)~range(-10^10)~range(10^9)").unwrap().eval().unwrap());
    test_skip_n(&parse("('a'..'z').repeat(10^10)~'A'.repeat(10^10)").unwrap().eval().unwrap());
}


fn eval_args(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    if node.args.len() != 1 {
        return Err(StreamError::new("exactly 1 argument expected", node));
    }
    let src_stream = try_with!(node, node.args[0].as_stream());
    if src_stream.length() == Length::Infinite {
        return Err(StreamError::new("stream is infinite", node));
    }
    let Head::Lang(LangItem::Args(head)) = node.head
        else { panic!("eval_args() called on something else than $args") };
    let expr = Expr::Eval(Node{
        head: *head,
        source: node.source.map(|item| Box::new(item.into())),
        args: src_stream.iter()
            .map(|res| res.map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?
    });
    expr.eval_env(env)
}

#[test]
fn test_args() {
    use crate::parser::parse;
    assert_eq!(parse("range@[3]").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("range@range(3)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("range@range(3)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("range@[3][2]").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("range@range(3)[1]").unwrap().eval().unwrap().to_string(), "1");
    assert!(parse("range@3").unwrap().eval().is_err());
    assert!(parse("range@seq").unwrap().eval().is_err());
}


pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$list", List::eval);
    keywords.insert("$part", eval_part);
    keywords.insert("$map", Map::eval);
    keywords.insert("$args", eval_args);
    keywords.insert("+", MathOp::eval);
    keywords.insert("-", MathOp::eval);
    keywords.insert("*", MathOp::eval);
    keywords.insert("/", MathOp::eval);
    keywords.insert("^", MathOp::eval);
    keywords.insert("~", Join::eval);
}
