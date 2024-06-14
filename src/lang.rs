use crate::base::*;
use crate::session::Session;
use num::{One, Signed, Zero};
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

fn construct_list(session: &Session, node: Node) -> Result<Item, StreamError> {
    let node = node.check_args(false, 0..)?;
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



fn construct_part(session: &Session, node: Node) -> Result<Item, StreamError> {
    node.check_args(true, 1..)?
        .eval_all(session)?
        .with(|node| {
            let mut item = node.source.as_ref().unwrap().to_item()?;
            for arg in &node.args {
                let index = arg.to_item()?.into_num()?;
                index.check_within(Number::one()..)?;
                let mut iter = item.into_stream()?.iter();
                if iter.skip_n(&(index - 1)).is_err() {
                    return Err("index past end of stream".into());
                }
                item = match iter.next() {
                    Some(value) => value?,
                    None => return Err("index past end of stream".into())
                };
            }
            Ok(item)
        })
}

/*struct Map {
    source: Box<dyn Stream>,
    body: Node
}

struct MapIter {
    source: Box<dyn SIterator>,
    body: Node
}

impl Map {
    fn construct(session: &Session, node: Node) -> Result<Item, StreamError> {
        let mut node = node.check_args(true, 1..=1)?
            .eval_source(session)?;
        let source = node.source.unwrap().to_item()?.into_stream()?;
        let body = node.args.pop().unwrap().into_node()?;
        if body.source.is_some() {
            return Err("body already has source".into());
        }
        Ok(Item::new_stream(Map{source, body, session}))
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
        Box::new(MapIter{source: self.source.iter(), body: self.body.clone(), session: self.session})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Clone for Map {
    fn clone(&self) -> Map {
        Map{source: dyn_clone::clone_box(&*self.source), body: self.body.clone(), session: self.session}
    }
}

impl Iterator for MapIter {
    type Item = Result<Item, StreamError>;

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
        Some(self.session.eval(expr))
    }
}

impl SIterator for MapIter {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        self.source.skip_n(n)
    }
}*/

fn construct_plus(session: &Session, node: Node) -> Result<Item, StreamError> {
    let ans = node.check_args(false, 1..)?
        .eval_all(session)?
        .with(|node| -> Result<_, StreamError> {
            node.args.iter().try_fold(Number::zero(), |a, e| Ok(a + e.to_item()?.into_num()?))
        });
    Ok(Item::new_number(ans?))
}

fn construct_minus(session: &Session, node: Node) -> Result<Item, StreamError> {
    let ans = node.check_args(false, 1..=2)?
        .eval_all(session)?
        .with(|node| {
            let args = &node.args;
            Ok(match args.len() {
                1 => -args[0].to_item()?.into_num()?,
                2 => args[0].to_item()?.into_num()? - args[1].to_item()?.into_num()?,
                _ => unreachable!()
            })
        });
    Ok(Item::new_number(ans?))
}

fn construct_times(session: &Session, node: Node) -> Result<Item, StreamError> {
    let ans = node.check_args(false, 1..)?
        .eval_all(session)?
        .with(|node| {
            node.args.iter().try_fold(Number::one(), |a, e| Ok(a * e.to_item()?.into_num()?))
        });
    Ok(Item::new_number(ans?))
}

fn construct_div(session: &Session, node: Node) -> Result<Item, StreamError> {
    let ans = node.check_args(false, 2..=2)?
        .eval_all(session)?
        .with(|node| {
            let nums = [node.args[0].to_item()?.into_num()?, node.args[1].to_item()?.into_num()?];
            if nums[1].is_zero() {
                return Err("division by zero".into());
            }
            Ok(&nums[0] / &nums[1])
        });
    Ok(Item::new_number(ans?))
}

fn construct_pow(session: &Session, node: Node) -> Result<Item, StreamError> {
    let ans = node.check_args(false, 2..=2)?
        .eval_all(session)?
        .with(|node| {
            let x = node.args[0].to_item()?.into_num()?;
            let y = node.args[1].to_item()?.into_num()?;
            if y.is_negative() {
                return Err("negative exponent".into());
            }
            Ok(x.pow(y.try_into().map_err(|_| StreamError::from("exponent too large"))?))
        });
    Ok(Item::new_number(ans?))
}

#[test]
fn test_opers() {
    use crate::parser::parse;
    let sess = Session::new();
    assert_eq!(sess.eval(parse("1+(-2)").unwrap()).unwrap().to_string(), "-1");
    assert_eq!(sess.eval(parse("(-1)-(-2)").unwrap()).unwrap().to_string(), "1");
    assert_eq!(sess.eval(parse("2*(-4)").unwrap()).unwrap().to_string(), "-8");
    assert_eq!(sess.eval(parse("11/2").unwrap()).unwrap().to_string(), "5");
    assert_eq!(sess.eval(parse("(-11)/(-2)").unwrap()).unwrap().to_string(), "5");
    assert_eq!(sess.eval(parse("1/2").unwrap()).unwrap().to_string(), "0");
    assert!(sess.eval(parse("1/0").unwrap()).is_err());
    assert_eq!(sess.eval(parse("10^30").unwrap()).unwrap().to_string(), "1000000000000000000000000000000");
    assert_eq!(sess.eval(parse("0^0").unwrap()).unwrap().to_string(), "1");
    assert_eq!(sess.eval(parse("0^1").unwrap()).unwrap().to_string(), "0");
    assert!(sess.eval(parse("1^(-1)").unwrap()).is_err());
}


pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("list", construct_list);
    keywords.insert("part", construct_part);
    //keywords.insert("map", |sess, node| Map::construct(sess, node));
    keywords.insert("+", construct_plus);
    keywords.insert("-", construct_minus);
    keywords.insert("*", construct_times);
    keywords.insert("/", construct_div);
    keywords.insert("^", construct_pow);
}
