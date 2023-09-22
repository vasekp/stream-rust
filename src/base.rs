use std::fmt::{Display, Formatter, Debug};

pub type TNumber = num_bigint::BigInt;


#[derive(PartialEq)]
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

impl Debug for Imm {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", match self {
            Number(n) => "number ".to_string() + &n.to_string()
        })
    }
}


pub enum Item {
    Atom(Imm),
    Stream(Box<dyn TStream>)
}

pub use Item::{Atom, Stream};

impl Item {
    pub fn new_imm(value: impl Into<Imm>) -> Item {
        Atom(value.into())
    }

    pub fn new_stream(value: impl TStream + 'static) -> Item {
        Stream(Box::new(value))
    }

    pub fn as_num(&self) -> StreamResult<&TNumber> {
        match self {
            Atom(Number(x)) => Ok(&x),
            _ => Err(StreamError())
        }
    }

    pub fn into_num(self) -> StreamResult<TNumber> {
        match self {
            Atom(Number(x)) => Ok(x),
            _ => Err(StreamError())
        }
    }

    pub fn as_stream(&self) -> StreamResult<&dyn TStream> {
        match self {
            Stream(s) => Ok(&**s),
            _ => Err(StreamError())
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        match self {
            Atom(a) => Display::fmt(&a, f),
            Stream(s) => writeout((*s).iter(), f)
        }
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        match self {
            Atom(a) => Debug::fmt(&a, f),
            Stream(_) => write!(f, "stream")
        }
    }
}

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Atom(i1), Atom(i2)) => i1 == i2,
            _ => false
        }
    }
}


#[derive(PartialEq)]
pub struct StreamError();

impl Debug for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "error")
    }
}

pub type StreamResult<T> = Result<T, StreamError>;

pub trait TStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>>;
}

fn writeout(mut iter: Box<dyn Iterator<Item = StreamResult<Item>>>, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
    if let Some(prec) = f.precision() {
        if prec < 4 {
            return Err(::std::fmt::Error)
        }
        let mut s = String::from('[');
        'a: while s.len() < prec {
            match iter.next() {
                None => {
                    s += "]";
                    break 'a;
                },
                Some(Ok(item)) => {
                    s += &format!("{:.*}", prec - s.len(), item);
                    s += ", ";
                },
                Some(Err(_err)) => {
                    s += "<!>";
                    break 'a;
                }
            }
        }
        if s.len() < prec {
            write!(f, "{}", s)
        } else {
            write!(f, "{:.*}...", prec - 3, s)
        }
    } else {
        let mut s = String::from('[');
        'a: {
            for _ in 0..3 {
                match iter.next() {
                    None => {
                        s += "]";
                        break 'a;
                    },
                    Some(Ok(item)) => {
                        s += &format!("{}", item);
                        s += ", ";
                    },
                    Some(Err(_err)) => {
                        s += "<!>";
                        break 'a;
                    }
                }
            }
            s += match iter.next() {
                None => "]",
                Some(_) => "..."
            };
        }
        write!(f, "{}", s)
    }
}
