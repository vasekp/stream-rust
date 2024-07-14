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
        return Err(StreamError::new("at least 1 argument required", node.into()));
    }
    for arg in &node.args {
        let index = try_with!(node, arg.as_num());
        try_with!(node, index.check_within(Number::one()..));
        let stm = try_with!(node, item.into_stream());
        match stm.length() {
            Length::Exact(len) | Length::AtMost(len) if &len < index =>
                return Err(StreamError::new("index past end of stream", node.into())),
            _ => ()
        }
        let mut iter = stm.iter();
        if iter.skip_n(&(index - 1))?.is_some() {
            return Err(StreamError::new("index past end of stream", node.into()));
        }
        item = match iter.next() {
            Some(value) => value?,
            None => return Err(StreamError::new("index past end of stream", node.into()))
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
struct Plus {
    node: ENode,
    env: Rc<Env>
}

impl Plus {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        try_with!(node, node.check_args_nonempty());
        match &node.args[0] {
            Item::Stream(ref stm) => Ok(if stm.is_string() {
                Item::new_stream(StringPlus{node, env: Rc::clone(env)})
            } else {
                Item::new_stream(Plus{node, env: Rc::clone(env)})
            }),
            _ => Ok(try_with!(node, Plus::helper(&node.args, env)))
        }
    }

    fn helper(items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
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
}

impl Describe for Plus {
    fn describe(&self) -> String {
        self.env.wrap_describe(self.node.describe())
    }
}

impl Stream for Plus {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(Forever{item})
            }).collect();
        Box::new(PlusIter{args, env: &self.env})
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::Infinite
            }).reduce(|a, e| Length::intersection(&a, &e)).unwrap()
    }
}

struct PlusIter<'node> {
    args: Vec<Box<dyn SIterator + 'node>>,
    env: &'node Rc<Env>
}

impl Iterator for PlusIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(inputs: Vec<Item>) -> Node {
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }
        let inputs = self.args.iter_mut()
            .map(|iter| (*iter).next())
            .collect::<Option<Result<Vec<_>, _>>>();
        match inputs {
            None => None,
            Some(Ok(inputs)) => {
                match Plus::helper(&inputs, self.env) {
                    Ok(item) => Some(Ok(item)),
                    Err(err) => Some(Err(StreamError::new(err, aux_node(inputs))))
                }
            },
            Some(Err(err)) => Some(Err(err))
        }
    }
}

impl SIterator for PlusIter<'_> {
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

#[derive(Clone)]
struct StringPlus {
    node: ENode,
    env: Rc<Env>
}

impl Describe for StringPlus {
    fn describe(&self) -> String {
        self.env.wrap_describe(self.node.describe())
    }
}

impl Stream for StringPlus {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(Forever{item})
            }).collect();
        Box::new(StringPlusIter{args, node: &self.node, env: &self.env})
    }

    fn is_string(&self) -> bool {
        true
    }

    fn length(&self) -> Length {
        self.node.args[0].as_stream().unwrap().length()
    }
}

struct StringPlusIter<'node> {
    args: Vec<Box<dyn SIterator + 'node>>,
    node: &'node ENode,
    env: &'node Rc<Env>
}

impl Iterator for StringPlusIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(inputs: Vec<Item>) -> Node {
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }

        let first = match self.args[0].next() {
            None => return None,
            Some(Ok(item)) => item,
            Some(Err(err)) => return Some(Err(err))
        };
        if let Item::Char(ref ch) = first {
            if !self.env.alphabet().contains(ch) {
                return Some(Ok(first));
            }
        } else {
            return Some(Err(StreamError::new("malformed string", self.node.clone().into())))
        }

        let rest = self.args.iter_mut()
            .skip(1)
            .map(|iter| (*iter).next())
            .collect::<Option<Result<Vec<_>, _>>>();
        match rest {
            None => Some(Err(StreamError::new("another operand ended earlier than the first", self.node.clone().into()))),
            Some(Ok(mut inputs)) => {
                inputs.insert(0, first);
                match Plus::helper(&inputs, self.env) {
                    Ok(item) => Some(Ok(item)),
                    Err(err) => Some(Err(StreamError::new(err, aux_node(inputs))))
                }
            },
            Some(Err(err)) => Some(Err(err))
        }
    }
}

impl SIterator for StringPlusIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        let mut args_iter = self.args.iter_mut();
        let first_iter = args_iter.next().unwrap();
        let mut n = n.to_owned();
        let mut n_chars = Number::zero();
        while n.is_positive() {
            match first_iter.next() {
                Some(Ok(Item::Char(ref ch))) => {
                    if self.env.alphabet().contains(ch) {
                        n_chars.inc();
                    }
                },
                Some(Err(err)) => return Err(err),
                Some(_) => return Err(StreamError::new("malformed string", self.node.clone().into())),
                None => return Ok(Some(n))
            }
            n.dec();
        }
        for iter in args_iter {
            if iter.skip_n(&n_chars)?.is_some() {
                return Err(StreamError::new("another operand ended earlier than the first", self.node.clone().into()));
            }
        }
        Ok(None)
    }
}


fn eval_minus(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    let args = &node.args;
    let ans = match args.len() {
        1 => -try_with!(node, args[0].as_num()),
        2 => try_with!(node, Ok(args[0].as_num()? - args[1].as_num()?)),
        _ => return Err(StreamError::new("1 or 2 arguments required", node.into()))
    };
    Ok(Item::new_number(ans))
}

fn eval_times(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    try_with!(node, node.check_args_nonempty());
    let ans = try_with!(node, node.args.iter().try_fold(Number::one(),
        |a, e| Ok(a * e.as_num()?)));
    Ok(Item::new_number(ans))
}

fn eval_div(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    if node.args.len() != 2 {
        return Err(StreamError::new("exactly 2 argument required", node.into()));
    }
    let x = try_with!(node, node.args[0].as_num());
    let y = try_with!(node, node.args[1].as_num());
    if y.is_zero() {
        return Err(StreamError::new("division by zero", node.into()));
    }
    Ok(Item::new_number(x / y))
}

fn eval_pow(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source());
    if node.args.len() != 2 {
        return Err(StreamError::new("exactly 2 argument required", node.into()));
    }
    let x = try_with!(node, node.args[0].as_num());
    let y = try_with!(node, node.args[1].as_num());
    if y.is_negative() {
        return Err(StreamError::new("negative exponent", node.into()));
    }
    let Some(exp) = y.to_u32() else {
        return Err(StreamError::new("exponent too large", node.into()));
    };
    Ok(Item::new_number(x.pow(exp)))
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
    assert_eq!(parse("\"AbC\"+3+[0,10,20]").unwrap().eval().unwrap().to_string(), "\"DoZ\"");
    assert_eq!(parse("\"Test\"+13+13").unwrap().eval().unwrap().to_string(), "\"Test\"");
    assert_eq!(parse(r#""ahoj"+"bebe""#).unwrap().eval().unwrap().to_string(), "\"cmqo\"");
    assert_eq!(parse("(1..5+3+[]).len").unwrap().eval().unwrap().to_string(), "0");
    assert_eq!(parse("(1..5+3+seq).len").unwrap().eval().unwrap().to_string(), "5");
    assert_eq!(parse(r#""abc"+['d',5,'f']"#).unwrap().eval().unwrap().to_string(), "\"egi\"");
    assert_eq!(parse(r#"['a','b','c']+"def""#).unwrap().eval().unwrap().to_string(), "['e', 'g', 'i']");
    assert_eq!(parse(r#"['a','b',3]+"def""#).unwrap().eval().unwrap().to_string(), "['e', 'g', <!>");
    assert_eq!(parse("seq+true").unwrap().eval().unwrap().to_string(), "[<!>");
    assert!(parse("true+false").unwrap().eval().is_err());
    assert!(parse("1+\"a\"").unwrap().eval().is_err());
    assert_eq!(parse(r#""Hello world!"+seq"#).unwrap().eval().unwrap().to_string(), r#""Igopt cvzun!""#);
    assert_eq!(parse(r#""Hello world!"+"ab""#).unwrap().eval().unwrap().to_string(), r#""Ig<!>"#);
    assert_eq!(parse(r#"("Hello world!"+"ab")[2]"#).unwrap().eval().unwrap().to_string(), "'g'");
    assert!(parse(r#"("Hello world!"+"ab")[3]"#).unwrap().eval().is_err());
    assert_eq!(parse(r#""Hello world!"+[]"#).unwrap().eval().unwrap().to_string(), r#""<!>"#);
    assert_eq!(parse(r#""Hello world!"+"ab".repeat"#).unwrap().eval().unwrap().to_string(), r#""Igmnp yptmf!""#);
    assert_eq!(parse(r#""ab".repeat+seq"#).unwrap().eval().unwrap().to_string(), r#""bddffhhjjllnnpprrttv..."#);
    assert_eq!(parse(r#"("ab".repeat+seq)[20]"#).unwrap().eval().unwrap().to_string(), "'v'");

    assert_eq!(parse("((1..4).repeat+(1..5).repeat)[10^10]").unwrap().eval().unwrap().to_string(), "9");
    test_len_exact(&parse("[1,2,3]+seq+5").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[1,2,3]+seq+[5]").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("\"abc\"+seq").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("\"a b c!\"+1..3+1").unwrap().eval().unwrap(), 6);
    test_skip_n(&parse("range(10^10)+seq+5").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)+range(10^11)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq+[]").unwrap().eval().unwrap());
    test_skip_n(&parse(r#""abcdefgh"+seq+"abcdefghijkl""#).unwrap().eval().unwrap());
    test_skip_n(&parse(r#""ab".repeat(3)+seq"#).unwrap().eval().unwrap());
    test_skip_n(&parse(r#""a b".repeat(3)+seq"#).unwrap().eval().unwrap());
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
                return Err(StreamError::new("mixed strings and non-strings", node.into()));
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
        return Err(StreamError::new("exactly 1 argument expected", node.into()));
    }
    let src_stream = try_with!(node, node.args[0].as_stream());
    if src_stream.length() == Length::Infinite {
        return Err(StreamError::new("stream is infinite", node.into()));
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
    keywords.insert("+", Plus::eval);
    keywords.insert("-", eval_minus);
    keywords.insert("*", eval_times);
    keywords.insert("/", eval_div);
    keywords.insert("^", eval_pow);
    keywords.insert("~", Join::eval);
}
