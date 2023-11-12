use crate::base::*;
use crate::session::Session;
use num::{One, Signed, Zero};
use num::pow::pow;
use crate::base::Describe;


/// A `Stream` formed by direct enumeration of its `Item`s.
#[derive(Clone)]
pub struct List(Vec<Item>);

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

fn construct_list(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 0..)?;
    node.args.into_iter()
        .map(|x| session.eval(x))
        .collect::<Result<Vec<Item>, _>>()
        .map(|vec| Item::new_stream(List::from(vec)))
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
            ret += &format!("{ch}");
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



fn construct_part(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(true, 1..)?;
    let mut item = session.eval(*node.source.unwrap())?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num_within(Number::one()..))
        .collect::<Result<Vec<_>, _>>()?;
    for index in args {
        let mut iter = item.into_stream()?.iter();
        if iter.skip_n(&(index - Number::one())).is_err() {
            return Err(BaseError::from("index past end of stream"));
        }
        item = iter.next().unwrap_or(Err(BaseError::from("index past end of stream")))?;
    }
    Ok(item)
}

struct Map {
    source: Box<dyn Stream>,
    body: Node
}

struct MapIter {
    source: Box<dyn SIterator>,
    body: Node
}

impl Map {
    fn construct(session: &Session, mut node: Node) -> Result<Item, BaseError> {
        node.check_args(true, 1..=1)?;
        let source = session.eval(*node.source.unwrap())?.into_stream()?;
        let body = node.args.pop().unwrap().into_node()?;
        if body.source.is_some() {
            return Err(BaseError::from("body already has source"));
        }
        Ok(Item::new_stream(Map{source, body}))
    }
}

impl Describe for Map {
    fn describe(&self) -> String {
        let mut ret = self.source.describe();
        ret.push(':');
        ret += &self.body.describe();
        ret
    }
}

impl Stream for Map {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(MapIter{source: self.source.iter(), body: self.body.clone()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Clone for Map {
    fn clone(&self) -> Map {
        Map{source: dyn_clone::clone_box(&*self.source), body: self.body.clone()}
    }
}

impl Iterator for MapIter {
    type Item = Result<Item, BaseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source.next()?;
        let Ok(source) = source else {
            return Some(source)
        };
        println!("<- {}", source);
        let expr = Expr::Eval(Node{
            source: Some(Box::new(Expr::new_imm(source))),
            head: self.body.head.clone(),
            args: self.body.args.clone()
        });
        println!("-> {}", expr.describe());
        // TODO!!!
        let sess = Session::new();
        Some(sess.eval(expr))
    }
}

impl SIterator for MapIter {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        self.source.skip_n(n)
    }
}

fn construct_plus(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..)?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = args.into_iter().reduce(|a, e| a + e).unwrap();
    Ok(Item::new_number(ans))
}

fn construct_minus(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..=2)?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = match args.len() {
        1 => -&args[0],
        2 => &args[0] - &args[1],
        _ => unreachable!()
    };
    Ok(Item::new_number(ans))
}

fn construct_times(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..)?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = args.into_iter().reduce(|a, e| a * e).unwrap();
    Ok(Item::new_number(ans))
}

fn construct_div(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 2..=2)?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    if args[1].is_zero() {
        return Err(BaseError::from("division by zero"));
    }
    let ans = &args[0] / &args[1];
    Ok(Item::new_number(ans))
}

fn construct_pow(session: &Session, node: Node) -> Result<Item, BaseError> {
    node.check_args(false, 2..=2)?;
    let args = node.args.into_iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let mut it = args.into_iter();
    let x = it.next().unwrap();
    let y = it.next().unwrap();
    if y.is_negative() {
        return Err(BaseError::from("negative exponent"));
    }
    let ans = pow(x, y.try_into().map_err(|_| BaseError::from("exponent too large"))?);
    Ok(Item::new_number(ans))
}

pub(crate) fn init(session: &mut Session) {
    session.register_symbol("list", construct_list);
    session.register_symbol("part", construct_part);
    session.register_symbol("map", Map::construct);
    session.register_symbol("+", construct_plus);
    session.register_symbol("-", construct_minus);
    session.register_symbol("*", construct_times);
    session.register_symbol("/", construct_div);
    session.register_symbol("^", construct_pow);
}
