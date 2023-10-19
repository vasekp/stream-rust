use crate::base::*;
use crate::session::Session;
use num::{One, Signed, Zero};
use num::pow::pow;


/// A `Stream` formed by direct enumeration of its `Item`s.
pub type List = Vec<Item>;

impl Stream for List {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(self.clone().into_iter().map(|x| Ok(x.clone())))
    }

    fn describe(&self) -> String {
        let mut s = String::new();
        s.push('[');
        let mut it = <[Item]>::iter(self);
        if let Some(item) = it.next() {
            s += &format!("{item}");
            for item in it {
                s += &format!(",{item}");
            }
        }
        s.push(']');
        s
    }

    fn length(&self) -> Length {
        Length::from(self.len())
    }
}

fn construct_list(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 0..)?;
    node.args.iter()
        .map(|x| session.eval(x))
        .collect::<Result<Vec<Item>, _>>()
        .map(Item::new_stream)
}

#[derive(Clone)]
pub struct LiteralString(Vec<Char>);

impl Stream for LiteralString {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(self.0.clone().into_iter().map(|x| Ok(Item::new_char(x.clone()))))
    }

    fn describe(&self) -> String {
        let mut ret = String::new();
        ret.push('"');
        for c in &self.0 {
            ret += &format!("{c:#}");
        }
        ret.push('"');
        ret
    }

    fn length(&self) -> Length {
        Length::from(self.0.len())
    }
}

impl From<String> for LiteralString {
    fn from(s: String) -> Self {
        LiteralString(s.chars().map(|c| Char::from(c)).collect())
    }
}

/*
impl Stream for String {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(chars.iter())
    }

    fn describe(&self) -> String {
        let mut ret = String::new();
        ret.push('"');
        for c in self.iter().map(|item| item.unwrap().into_char().unwrap()) {
            ret += &format!("{c:#}");
        }
        ret.push('"');
        ret
    }

    fn length(&self) -> Length {
        Length::UnknownFinite
    }
}
*/

fn construct_part(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(true, 1..)?;
    let mut item = session.eval(node.source.as_ref().unwrap())?;
    let args = node.args.iter()
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

fn construct_plus(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..)?;
    let args = node.args.iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = args.into_iter().reduce(|a, e| a + e).unwrap();
    Ok(Item::new_number(ans))
}

fn construct_minus(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..=2)?;
    let args = node.args.iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = match args.len() {
        1 => -&args[0],
        2 => &args[0] - &args[1],
        _ => unreachable!()
    };
    Ok(Item::new_number(ans))
}

fn construct_times(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..)?;
    let args = node.args.iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    let ans = args.into_iter().reduce(|a, e| a * e).unwrap();
    Ok(Item::new_number(ans))
}

fn construct_div(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 2..=2)?;
    let args = node.args.iter()
        .map(|x| session.eval(x)?.into_num())
        .collect::<Result<Vec<_>, _>>()?;
    if args[1].is_zero() {
        return Err(BaseError::from("division by zero"));
    }
    let ans = &args[0] / &args[1];
    Ok(Item::new_number(ans))
}

fn construct_pow(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 2..=2)?;
    let args = node.args.iter()
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
    session.register_symbol("+", construct_plus);
    session.register_symbol("-", construct_minus);
    session.register_symbol("*", construct_times);
    session.register_symbol("/", construct_div);
    session.register_symbol("^", construct_pow);
}
