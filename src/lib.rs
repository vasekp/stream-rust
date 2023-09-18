use std::fmt::{Display, Formatter, Debug};
use num_bigint::BigInt;

type TNumber = BigInt;

pub struct StreamError { }

impl Debug for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(f, "error")
    }
}

type Result<T> = ::core::result::Result<T, StreamError>;

pub enum Node {
    Atom(TAtom),
    Stream(Box<dyn TStream>)
}

use Node::{Atom, Stream};

pub enum TAtom {
    Number(TNumber)
}

use TAtom::Number;

pub trait TIter {
    fn next(&mut self) -> Result<Option<Node>>;
}

pub trait TStream {
    fn iter(&self) -> Box<dyn TIter>;
}

impl<T, U> TIter for U
where T: Into<TNumber>, U: Iterator<Item = T> {
    fn next(&mut self) -> Result<Option<Node>> {
        Ok(Iterator::next(self).map(|x| Atom(Number(x.into()))))
    }
}

pub struct IotaStream();

impl IotaStream {
    fn new_node() -> Node {
        Stream(Box::new(IotaStream()))
    }
}

impl TStream for IotaStream {
    fn iter(&self) -> Box<dyn TIter> {
        Box::new(1..3)
    }
}

impl Node {
    pub fn parse(_s: &str) -> Result<Node> {
        Ok(IotaStream::new_node())
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(f, "{}", match self {
            Atom(a) => a.to_string(),
            Stream(s) => writeout(&mut (*s).iter()) // TODO f
        })
    }
}

impl Display for TAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(f, "{}", match self {
            Number(n) => n.to_string()
        })
    }
}

fn writeout(iter: &mut Box<dyn TIter>) -> String {
    format!("[{}, {}, ...]", iter.next().unwrap().unwrap(), iter.next().unwrap().unwrap())
}
