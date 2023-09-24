use std::fmt::{Display, Formatter, Debug};


/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type TNumber = num::BigInt;


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
            _ => Err(StreamError(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn into_num(self) -> StreamResult<TNumber> {
        match self {
            Atom(Number(x)) => Ok(x),
            _ => Err(StreamError(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn as_stream(&self) -> StreamResult<&dyn TStream> {
        match self {
            Stream(s) => Ok(&**s),
            _ => Err(StreamError(format!("expected stream, found {:?}", &self)))
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


/// The base error type for use for this library. Currently only holds a String description.
#[derive(PartialEq, Debug)]
pub struct StreamError(pub String);

impl ::std::error::Error for StreamError { }

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", self.0)
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

    /// Returns the length of this stream, in as much information as available *without* consuming
    /// the iterator. See [`Length`] for the possible return values. The default implementation
    /// relies on [`Iterator::size_hint()`] to return one of `Exact`, `AtMost`, `Unknown` or
    /// `LikelyInfinite`. The latter is produced if `size_hint` returned `(usize::MAX, None)`,
    /// which is a customary indication of infiniteness in the standard library, but may have false
    /// positives, like an iterator whose size can't fit into `usize`.
    fn length(&self) -> Length {
        let iter = self.iter();
        match iter.size_hint() {
            (lo, Some(hi)) => if lo == hi { Exact(lo.into()) } else { AtMost(hi.into()) },
            (usize::MAX, None) => Infinite,
            _ => Unknown
        }
    }
}


/// The enum returned by [`TStream::length`]. The information must be consistent with the actual
/// behaviour of the stream.
#[derive(Debug, Clone, PartialEq)]
pub enum Length {
    /// The length is known exactly, including empty streams.
    Exact(TNumber),
    /// The length has a known upper bound.
    AtMost(TNumber),
    /// The stream is known to be infinite.
    Infinite,
    /// A special value for when a standard [`Iterator::size_hint()`] returns `(usize::MAX, None)`.
    LikelyInfinite,
    /// The length is not known but promises to be finite.
    UnknownFinite,
    /// Nothing can be inferred about the length.
    Unknown
}

pub use Length::*;

impl Length {
    fn _at_most(value: &Length) -> Length {
        match value {
            Exact(x) => AtMost(x.clone()),
            AtMost(x) => AtMost(x.clone()),
            UnknownFinite => UnknownFinite,
            _ => Unknown
        }
    }
}

impl<T> From<T> for Length where T: Into<TNumber> {
    fn from(value: T) -> Self {
        Exact(value.into())
    }
}
