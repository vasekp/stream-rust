use std::fmt::{Display, Formatter, Debug};


/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type TNumber = num_bigint::BigInt;


/// Encompasses all "immediate" values. This is directly comparable and cloneable, and appear both
/// as an [`Item`] and a [`Node`].
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


/// An `Item` is the result of evaluation. Either an `Atom`, which is an immediate value ([`Imm`]),
/// or a `Stream` (see [`TStream`]).
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
            Stream(s) => (*s).writeout(f)
        }
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        match self {
            Atom(a) => Debug::fmt(&a, f),
            Stream(s) => write!(f, "stream {}", s.describe())
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


/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]'s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait TStream {
    /// Create an [`Iterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>>;

    /// Write the contents of the stream (i.e., the items returned by its iterator) in a
    /// human-readable form. This is called by the [`Display`] trait. The formatter may specify a
    /// maximum width (using the `"{:.n}"` syntax), in which case the output is truncated using
    /// ellipsis (the width must be at least 4 to accommodate the string `"[..."`); if no width is
    /// given, first three items are written out.  If an error happens during reading the stream,
    /// it is represented as `"<!>"`.
    fn writeout(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (prec, usize::MAX),
            None => (usize::MAX, 3)
        };
        if prec < 4 {
            return Err(::std::fmt::Error)
        }
        let mut s = String::from('[');
        let mut i = 0;
        'a: {
            while s.len() < prec && i < max {
                match iter.next() {
                    None => {
                        s += "]";
                        break 'a;
                    },
                    Some(Ok(item)) => {
                        if i > 0 {
                            s += ", ";
                        }
                        s += &format!("{:.*}", prec - s.len(), item);
                    },
                    Some(Err(_err)) => {
                        s += "<!>";
                        break 'a;
                    }
                };
                i = i + 1;
            }
            s += match iter.next() {
                None => "]",
                Some(_) => ", ..."
            };
        }
        if s.len() < prec {
            write!(f, "{}", s)
        } else {
            write!(f, "{:.*}...", prec - 3, s)
        }
    }

    /// Describe the stream using the stream syntax. This should be parseable back to reconstruct a
    /// copy, but it is not a strict requirement. The primary purpose of this function is for the
    /// implementation of the [`Debug`] trait.
    fn describe(&self) -> String;
}
