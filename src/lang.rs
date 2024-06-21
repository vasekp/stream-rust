#![allow(clippy::redundant_closure_call)]
use crate::base::*;
use crate::try_with;
use num::{One, Signed, Zero};
use crate::base::Describe;


/// A `Stream` formed by direct enumeration of its `Item`s.
#[derive(Clone)]
pub struct List(Vec<Item>);

impl List {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        let vec = try_with!(node, node.args.iter()
            .map(|expr| expr.to_item())
            .collect::<Result<Vec<_>, _>>());
        Ok(Item::new_stream(List::from(vec)))
    }
}

impl Stream for List {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(self.0.clone().into_iter().map(|x| Ok(x.clone())))
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl Describe for List {
    fn describe(&self) -> String {
        let mut ret = String::new();
        ret.push('[');
        let mut it = self.0.iter();
        if let Some(item) = it.next() {
            ret += &item.describe();
        }
        for item in it {
            ret += ", ";
            ret += &item.describe();
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


#[derive(Clone)]
pub struct LiteralString(Vec<Char>);

impl Stream for LiteralString {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(self.0.clone().into_iter().map(|x| Ok(Item::new_char(x.clone()))))
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


fn eval_part(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let mut item = try_with!(node, node.source_checked()?.to_item());
    if node.args.is_empty() {
        return Err(StreamError::new("at least 1 argument required", node));
    }
    for arg in &node.args {
        let index = try_with!(node, arg.to_item()?.into_num());
        try_with!(node, index.check_within(Number::one()..));
        let mut iter = try_with!(node, item.into_stream()).iter();
        if iter.skip_n(index - 1).is_err() {
            return Err(StreamError::new("index past end of stream", node));
        }
        item = match iter.next() {
            Some(value) => value?,
            None => return Err(StreamError::new("index past end of stream", node))
        };
    }
    Ok(item)
}

struct Map {
    source: Box<dyn Stream>,
    body: Node,
    env: Env
}

struct MapIter {
    source: Box<dyn SIterator>,
    body: Node,
    env: Env
}

impl Map {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_source(env)?;
        let source = try_with!(node, node.source_checked()?.to_item()?.into_stream());
        let body = match node.args.len() {
            1 => try_with!(node, node.args[0].to_node()),
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        if body.source.is_some() {
            return Err(StreamError::new("body already has source", node));
        }
        Ok(Item::new_stream(Map{source, body, env: env.clone()}))
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
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(MapIter{source: self.source.iter(), body: self.body.clone(), env: self.env.clone()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Clone for Map {
    fn clone(&self) -> Map {
        Map{source: dyn_clone::clone_box(&*self.source), body: self.body.clone(), env: self.env.clone()}
    }
}

impl Iterator for MapIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source.next()?;
        let Ok(source) = source else {
            return Some(source)
        };
        let expr = Expr::Eval(Node{
            source: Some(Box::new(Expr::new_imm(source))),
            head: self.body.head.clone(),
            args: self.body.args.clone()
        });
        Some(expr.eval(&self.env))
    }
}

impl SIterator for MapIter {
    fn skip_n(&mut self, n: Number) -> Result<(), Number> {
        self.source.skip_n(n)
    }
}

#[test]
fn test_map() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("[1,2,3]:{#*10}").unwrap().eval(&env).unwrap().to_string(), "[10, 20, 30]");
    assert_eq!(parse("seq:{#^2}").unwrap().eval(&env).unwrap().to_string(), "[1, 4, 9, ...");
    assert_eq!(parse("seq:{#1}").unwrap().eval(&env).unwrap().to_string(), "[<!>");
    assert_eq!(parse("seq:{range(#)}").unwrap().eval(&env).unwrap().to_string(), "[[1], [1, 2], [1, 2, 3], ...");
}


fn eval_plus(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    try_with!(node, node.check_args_nonempty());
    let ans = try_with!(node, node.args.iter().try_fold(Number::zero(),
        |a, e| Ok(a + e.to_item()?.into_num()?)));
    Ok(Item::new_number(ans))
}

fn eval_minus(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    let args = &node.args;
    let ans = match args.len() {
        1 => -try_with!(node, args[0].to_item()?.into_num()),
        2 => try_with!(node, Ok(args[0].to_item()?.into_num()? - args[1].to_item()?.into_num()?)),
        _ => return Err(StreamError::new("1 or 2 arguments required", node))
    };
    Ok(Item::new_number(ans))
}

fn eval_times(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    try_with!(node, node.check_args_nonempty());
    let ans = try_with!(node, node.args.iter().try_fold(Number::one(), |a, e| Ok(a * e.to_item()?.into_num()?)));
    Ok(Item::new_number(ans))
}

fn eval_div(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    if node.args.len() != 2 {
        return Err(StreamError::new("exactly 2 argument required", node));
    }
    let x = try_with!(node, node.args[0].to_item()?.into_num());
    let y = try_with!(node, node.args[1].to_item()?.into_num());
    if y.is_zero() {
        return Err(StreamError::new("division by zero", node));
    }
    Ok(Item::new_number(&x / &y))
}

fn eval_pow(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    if node.args.len() != 2 {
        return Err(StreamError::new("exactly 2 argument required", node));
    }
    let x = try_with!(node, node.args[0].to_item()?.into_num());
    let y = try_with!(node, node.args[1].to_item()?.into_num());
    if y.is_negative() {
        return Err(StreamError::new("negative exponent", node));
    }
    let ans = x.pow(y.try_into().map_err(|_| StreamError::new("exponent too large", node))?);
    Ok(Item::new_number(ans))
}

#[test]
fn test_opers() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("1+(-2)").unwrap().eval(&env).unwrap().to_string(), "-1");
    assert_eq!(parse("(-1)-(-2)").unwrap().eval(&env).unwrap().to_string(), "1");
    assert_eq!(parse("2*(-4)").unwrap().eval(&env).unwrap().to_string(), "-8");
    assert_eq!(parse("11/2").unwrap().eval(&env).unwrap().to_string(), "5");
    assert_eq!(parse("(-11)/(-2)").unwrap().eval(&env).unwrap().to_string(), "5");
    assert_eq!(parse("1/2").unwrap().eval(&env).unwrap().to_string(), "0");
    assert!(parse("1/0").unwrap().eval(&env).is_err());
    assert_eq!(parse("10^30").unwrap().eval(&env).unwrap().to_string(), "1000000000000000000000000000000");
    assert_eq!(parse("0^0").unwrap().eval(&env).unwrap().to_string(), "1");
    assert_eq!(parse("0^1").unwrap().eval(&env).unwrap().to_string(), "0");
    assert!(parse("1^(-1)").unwrap().eval(&env).is_err());
}


#[derive(Clone)]
struct Join {
    node: Node
}

impl Join {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        try_with!(node, node.check_args_nonempty());
        let mut string = None;
        for arg in &node.args {
            let stream = try_with!(node, arg.as_item()?.as_stream());
            match string {
                None => string = Some(stream.is_string()),
                Some(string) => {
                    if stream.is_string() != string {
                        return Err(StreamError::new("mixed streams and strings", node));
                    }
                }
            }
        }
        Ok(Item::new_stream(Join{node}))
    }
    // TODO: string+char, stream+item, item+item etc.
}

impl Describe for Join {
    fn describe(&self) -> String {
        self.node.describe()
    }
}

impl Stream for Join {
    fn iter(&self) -> Box<dyn SIterator> {
        let mut iter = self.node.args.clone().into_iter();
        let first = iter.next().unwrap()
            .as_item().unwrap()
            .as_stream().unwrap()
            .iter();
        Box::new(JoinIter{sources: iter, cur: first})
    }

    fn is_string(&self) -> bool {
        self.node.args[0]
            .as_item().unwrap()
            .as_stream().unwrap()
            .is_string()
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|expr| expr.as_item().unwrap().as_stream().unwrap().length())
            .reduce(|acc, e| acc + e).unwrap()
    }
}

struct JoinIter {
    sources: std::vec::IntoIter<Expr>,
    cur: Box<dyn SIterator>
}

impl Iterator for JoinIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = (*self.cur).next();
            if next.is_some() {
                return next;
            } else {
                self.cur = self.sources.next()?
                    .as_item().unwrap()
                    .as_stream().unwrap()
                    .iter();
            }
        }
    }
}

impl SIterator for JoinIter {
    fn skip_n(&mut self, mut n: Number) -> Result<(), Number> {
        assert!(!n.is_negative());
        loop {
            let Err(m) = self.cur.skip_n(n)
                else { return Ok(()); };
            n = m;
            let Some(next) = self.sources.next()
                else { return Err(n); };
            self.cur = next.as_item().unwrap()
                .as_stream().unwrap()
                .iter();
        }
    }
}

#[test]
fn test_join() {
    use crate::parser::parse;
    let env = Default::default();

    assert_eq!(parse("[10]~seq").unwrap().eval(&env).unwrap().to_string(), "[10, 1, 2, ...");
    assert_eq!(parse("range(2)~seq").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 1, ...");
    assert_eq!(parse("range(10^10).{#~#~#}.len").unwrap().eval(&env).unwrap().to_string(), "30000000000");
    assert!(parse("([5]~seq).len").unwrap().eval(&env).is_err());
    assert_eq!(parse("(range(10^10)~seq)[10^11]").unwrap().eval(&env).unwrap().to_string(), "90000000000");

    assert_eq!(parse("\"ab\"~\"cd\"").unwrap().eval(&env).unwrap().to_string(), "\"abcd\"");
    assert_eq!(parse("(\"ab\"~\"cd\").len").unwrap().eval(&env).unwrap().to_string(), "4");

    assert_eq!(parse("[1]~[2]").unwrap().eval(&env).unwrap().to_string(), "[1, 2]");
    assert_eq!(parse("[1]~[[2]]").unwrap().eval(&env).unwrap().to_string(), "[1, [2]]");
    assert!(parse("[1]~2").unwrap().eval(&env).is_err());
    assert!(parse("[1]~\"a\"").unwrap().eval(&env).is_err());
    assert!(parse("\"a\"~[1]").unwrap().eval(&env).is_err());
    assert!(parse("\"a\"~[\"b\"]").unwrap().eval(&env).is_err());
}


pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("list", List::eval);
    keywords.insert("part", eval_part);
    keywords.insert("map", Map::eval);
    keywords.insert("+", eval_plus);
    keywords.insert("-", eval_minus);
    keywords.insert("*", eval_times);
    keywords.insert("/", eval_div);
    keywords.insert("^", eval_pow);
    keywords.insert("~", Join::eval);
}
