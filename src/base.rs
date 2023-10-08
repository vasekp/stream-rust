use std::fmt::{Display, Formatter, Debug};
use std::ops::RangeBounds;


/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = num::BigInt;


/// Encompasses all atomic values.
#[derive(PartialEq)]
pub enum Atom {
    Number(Number)
}

impl<T> From<T> for Atom where T : Into<Number> {
    fn from(value: T) -> Self {
        Atom::Number(value.into())
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", match self {
            Atom::Number(n) => n.to_string()
        })
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", match self {
            Atom::Number(n) => "number ".to_string() + &n.to_string()
        })
    }
}


/// An `Item` is the result of evaluation. Either an [`Atom`], which is an atomic value,
/// or a [`Stream`].
pub enum Item {
    Atom(Atom),
    Stream(Box<dyn Stream>)
}

pub use Item::*;

impl Item {
    pub fn new_atomic(value: impl Into<Atom>) -> Item {
        Atom(value.into())
    }

    pub fn new_stream(value: impl Stream + 'static) -> Item {
        Stream(Box::new(value))
    }

    pub fn as_num(&self) -> Result<&Number, BaseError> {
        match self {
            Atom(Atom::Number(x)) => Ok(x),
            _ => Err(BaseError::from(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn into_num(self) -> Result<Number, BaseError> {
        match self {
            Atom(Atom::Number(x)) => Ok(x),
            _ => Err(BaseError::from(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn as_stream(&self) -> Result<&dyn Stream, BaseError> {
        match self {
            Stream(s) => Ok(&**s),
            _ => Err(BaseError::from(format!("expected stream, found {:?}", &self)))
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Atom(a) => Display::fmt(&a, f),
            Stream(s) => (*s).writeout(f)
        }
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
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
pub struct BaseError(String);

impl std::error::Error for BaseError { }

impl<T> From<T> for BaseError where T: Into<String> {
    fn from(text: T) -> BaseError {
        BaseError(text.into())
    }
}

impl Display for BaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}


pub(crate) type SIterator = dyn Iterator<Item = Result<Item, BaseError>>;


/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream {
    /// Construct this stream with given arguments.
    fn construct(ins: Vec<Item>) -> Result<Item, BaseError> where Self: Sized;

    /// Create an [`Iterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    fn iter(&self) -> Box<SIterator>;

    /// Write the contents of the stream (i.e., the items returned by its iterator) in a
    /// human-readable form. This is called by the [`Display`] trait. The formatter may specify a
    /// maximum width (using the `"{:.n}"` syntax), in which case the output is truncated using
    /// ellipsis (the width must be at least 4 to accommodate the string `"[..."`); if no width is
    /// given, first three items are written out.  If an error happens during reading the stream,
    /// it is represented as `"<!>"`.
    fn writeout(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
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
                i += 1;
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
    ///
    /// The return value must be consistent with the actual behaviour of the stream.
    fn length(&self) -> Length {
        let iter = self.iter();
        match iter.size_hint() {
            (lo, Some(hi)) => if lo == hi { Length::Exact(lo.into()) } else { Length::AtMost(hi.into()) },
            (usize::MAX, None) => Length::Infinite,
            _ => Length::Unknown
        }
    }
}


/// The enum returned by [`Stream::length()`].
#[derive(Debug, Clone, PartialEq)]
pub enum Length {
    /// The length is known exactly, including empty streams.
    Exact(Number),
    /// The length has a known upper bound.
    AtMost(Number),
    /// The stream is known to be infinite.
    Infinite,
    /// A special value for when a standard [`Iterator::size_hint()`] returns `(usize::MAX, None)`.
    LikelyInfinite,
    /// The length is not known but promises to be finite.
    UnknownFinite,
    /// Nothing can be inferred about the length.
    Unknown
}

impl Length {
    fn _at_most(value: &Length) -> Length {
        use Length::*;
        match value {
            Exact(x) => AtMost(x.clone()),
            AtMost(x) => AtMost(x.clone()),
            UnknownFinite => UnknownFinite,
            _ => Unknown
        }
    }
}

impl<T> From<T> for Length where T: Into<Number> {
    fn from(value: T) -> Self {
        Length::Exact(value.into())
    }
}


pub(crate) fn check_args(args: &Vec<Item>, range: impl RangeBounds<usize>) -> Result<(), BaseError> {
    use std::ops::Bound::*;
    if range.contains(&args.len()) {
        Ok(())
    } else {
        Err(BaseError::from(match (range.start_bound(), range.end_bound()) {
            (Included(0), Included(0)) => "no arguments allowed".to_string(),
            (Included(min), Included(max)) if min == max => format!("exactly {min} arguments required"),
            (Included(min), Included(max)) => format!("between {min} and {max} arguments required"),
            (Included(min), Unbounded) => format!("at least {min} arguments required"),
            (Unbounded, Included(max)) => format!("at most {max} arguments required"),
            _ => panic!("checkArgs: bounds {:?}, {:?}", range.start_bound(), range.end_bound())
        }))
    }
}


/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Direct(Item),
    Node(Node)
}

/// A `Node` is a type of [`Expr`] representing a core function along with, optionally, its source and arguments. This eventually evaluates to `Item`.
#[derive(Debug, PartialEq)]
pub struct Node {
    pub core: Core,
    pub source: Option<Box<Expr>>,
    pub args: Vec<Expr>
}

/// The core of a [`Node`]. This can either be an identifier (`source.ident(args)`), or a body
/// formed by an entire expression (`source.{body}(args)`). In the latter case, the `source` and
/// `args` are accessed via `#` and `#1`, `#2` etc., respectively.
#[derive(Debug, PartialEq)]
pub enum Core {
    Simple(String),
    Block(Box<Expr>)
}
