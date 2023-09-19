use std::fmt::{Display, Formatter, Debug};

pub type TNumber = num_bigint::BigInt;


pub enum Imm {
    Number(TNumber)
}

pub use Imm::Number;

impl<T> From<T> for Imm where T : Into<TNumber> {
    fn from(value: T) -> Self {
        Number(value.into())
    }
}

impl Display for Imm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", match self {
            Number(n) => n.to_string()
        })
    }
}

impl TryInto<TNumber> for Imm {
    type Error = StreamError;

    fn try_into(self) -> StreamResult<TNumber> {
        match self {
            Number(x) => Ok(x)//,
            //_ => Err(StreamError())
        }
    }
}


pub enum Item {
    Atom(Imm),
    Stream(Box<dyn TStream>)
}

pub use Item::{Atom, Stream};

impl Item {
    pub fn imm(self) -> StreamResult<Imm> {
        match self {
            Atom(x) => Ok(x),
            _ => Err(StreamError())
        }
    }
}

impl<T> From<T> for Item where T: Into<Imm> {
    fn from(value: T) -> Self {
        Atom(value.into())
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", match self {
            Atom(a) => a.to_string(),
            Stream(s) => writeout((*s).iter()) // TODO f
        })
    }
}


pub struct StreamError();

impl Debug for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "error")
    }
}

pub type StreamResult<T> = Result<T, StreamError>;

pub trait TStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>>;

    fn into_item(self) -> Item where Self: Sized + 'static {
        Stream(Box::new(self))
    }
}

/*impl<T, U> TIterator for U
where T: Into<TNumber>, U: Iterator<Item = T> {
    fn next(&mut self) -> StreamResult<Option<Item>> {
        Ok(Iterator::next(self).map(|x| Item::from(x.into())))
    }
}*/

fn writeout(mut iter: Box<dyn Iterator<Item = StreamResult<Item>>>) -> String {
    format!("[{}, {}, ...]", iter.next().unwrap().unwrap(), iter.next().unwrap().unwrap())
}
