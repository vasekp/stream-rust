use std::fmt::{Display, Formatter, Debug};
use std::ops::RangeBounds;
use dyn_clone::DynClone;
use crate::utils::describe_range;
use num::{Signed, One, Zero};
use crate::session::Session;


/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = num::BigInt;

pub(crate) trait NumWithin : PartialOrd {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), StreamError>;
}

impl NumWithin for Number {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), StreamError> {
        match range.contains(self) {
            true => Ok(()),
            false => Err(StreamError::from(format!("expected {}, found {}", describe_range(&range), &self)))
        }
    }
}


/// A trait for the ability to turn a Stream language object (notably, [`Expr`]) into an input form.
pub trait Describe {
    /// Construct a string representation of `self`. This is meant for storing object across
    /// sessions. The resulting `String` must be a syntactically valid input that reconstruct a
    /// copy of the original object on [`parser::parse()`](crate::parser::parse()) and
    /// [`Session::eval()`]. For this reason, all session-local data should be baked into the
    /// expression.
    fn describe(&self) -> String;
}


/// An `Item` is a concrete value or stream, the result of evaluation of a [`Node`].
pub enum Item {
    Number(Number),
    Bool(bool),
    Char(Char),
    Stream(Box<dyn Stream>)
}

impl Item {
    pub fn new_number(value: impl Into<Number>) -> Item {
        Item::Number(value.into())
    }

    pub fn new_bool(value: bool) -> Item {
        Item::Bool(value)
    }

    pub fn new_char(value: impl Into<Char>) -> Item {
        Item::Char(value.into())
    }

    pub fn new_stream(value: impl Stream + 'static) -> Item {
        Item::Stream(Box::new(value))
    }

    pub fn as_num(&self) -> Result<&Number, StreamError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(StreamError::from(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn into_num(self) -> Result<Number, StreamError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(StreamError::from(format!("expected number, found {:?}", &self)))
        }
    }

    pub fn as_char(&self) -> Result<&Char, StreamError> {
        match self {
            Item::Char(c) => Ok(c),
            _ => Err(StreamError::from(format!("expected char, found {:?}", &self)))
        }
    }

    pub fn as_stream(&self) -> Result<&dyn Stream, StreamError> {
        match self {
            Item::Stream(s) => Ok(&**s),
            _ => Err(StreamError::from(format!("expected stream, found {:?}", &self)))
        }
    }

    pub fn into_stream(self) -> Result<Box<dyn Stream>, StreamError> {
        match self {
            Item::Stream(s) => Ok(s),
            _ => Err(StreamError::from(format!("expected stream, found {:?}", &self)))
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Item::*;
        match self {
            Number(n) => write!(f, "{n}"),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "'{c}'"),
            Stream(s) => (*s).writeout(f)
        }
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Item::*;
        match self {
            Number(n) => write!(f, "number {n}"),
            Bool(b) => write!(f, "bool {b}"),
            Char(c) => write!(f, "char '{c}'"),
            Stream(s) => write!(f, "{} {s}", if s.is_string() { "string" } else { "stream" })
        }
    }
}

impl Describe for Item {
    fn describe(&self) -> String {
        use Item::*;
        match self {
            Number(n) if n.is_negative() => format!("({n})"),
            Number(n) => format!("{n}"),
            Bool(b) => format!("{b}"),
            Char(c) => format!("'{c}'"),
            Stream(s) => s.describe()
        }
    }
}

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        use Item::*;
        match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            _ => todo!()
        }
    }
}

impl Clone for Item {
    fn clone(&self) -> Item {
        use Item::*;
        match self {
            Number(x) => Number(x.clone()),
            Bool(x) => Bool(*x),
            Char(x) => Char(x.clone()),
            Stream(s) => Stream(dyn_clone::clone_box(&**s))
        }
    }
}


/// A 'character' in Stream may represent a single code point or a multigraph (such as 'dz').
#[derive(Debug, Clone, PartialEq)]
pub enum Char {
    Single(char),
    Multi(String)
}

impl From<char> for Char {
    fn from(c: char) -> Char {
        Char::Single(c)
    }
}

impl From<String> for Char {
    fn from(s: String) -> Char {
        let mut it = s.chars();
        if let (Some(c), None) = (it.next(), it.next()) {
            Char::Single(c)
        } else {
            Char::Multi(s)
        }
    }
}

impl From<&str> for Char {
    fn from(x: &str) -> Char {
        Char::from(x.to_string())
    }
}

impl Display for Char {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let alt = f.alternate();
        let escape = |c| -> String {
            match c {
                '\n' => "\\n".into(),
                '\r' => "\\r".into(),
                '\t' => "\\t".into(),
                '\\' => "\\\\".into(),
                '\'' => if alt { c.into() } else { "\\'".into() },
                '"' => if alt { "\\\"".into() } else { c.into() },
                _ => c.into()
            }
        };
        match self {
            Char::Single(c) => write!(f, "{}", escape(*c))?,
            Char::Multi(s) => {
                for c in s.chars() {
                    write!(f, "{}", escape(c))?;
                }
            }
        }
        Ok(())
    }
}

#[test]
fn test_char() {
    assert_eq!(Char::from('a'), Char::Single('a'));
    assert_eq!(Char::from("a"), Char::Single('a'));
    assert_eq!(Char::from('❤'), Char::Single('❤')); // multi-byte
    assert_eq!(Char::from("❤"), Char::Single('❤'));
    assert_eq!(Char::from("é"), Char::Multi("é".into())); // combining mark
    assert_eq!(Char::from("as"), Char::Multi("as".into()));
    assert_eq!(Char::from('\n').to_string(), "\\n");
}


/// The base error type for use for this library. Currently only holds a String description.
#[derive(PartialEq, Debug)]
pub struct StreamError{
    reason: String,
    node: Option<Node>
}

impl StreamError {
    pub fn new<T>(text: T, node: Node) -> StreamError where T: Into<String> {
        StreamError{reason: text.into(), node: Some(node)}
    }

    pub(crate) fn with_node(mut self, node: Node) -> StreamError {
        if self.node.is_none() {
            self.node = Some(node);
        }
        self
    }
}

impl std::error::Error for StreamError { }

impl<T> From<T> for StreamError where T: Into<String> {
    fn from(text: T) -> StreamError {
        StreamError{reason: text.into(), node: None}
    }
}

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.node {
            Some(node) => write!(f, "{}: {}", node.describe(), self.reason),
            None => write!(f, "{}", self.reason)
        }
    }
}


/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream: DynClone + Describe {
    /// Create an [`SIterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    fn iter(&self) -> Box<dyn SIterator>;

    /// Write the contents of the stream (i.e., the items returned by its iterator) in a
    /// human-readable form. This is called by the [`Display`] trait. The formatter may specify a
    /// maximum width (using the `"{:.n}"` syntax), in which case the output is truncated using
    /// ellipsis (the width must be at least 4 to accommodate the string `"[..."`); if no width is
    /// given, first three items are written out.  If an error happens during reading the stream,
    /// it is represented as `"<!>"`.
    ///
    /// If this is `Stream` represents a string, as expressed by its [`Stream::is_string()`]
    /// method, the formatting follows that of a string, including character escapes. If no length
    /// is given, up to 20 characters are printed. Any value returned by the iterator which is not
    /// a [`Char`] is treated as a reading error.
    fn writeout(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.is_string() {
            self.writeout_string(f)
        } else {
            self.writeout_stream(f)
        }
    }

    #[doc(hidden)]
    fn writeout_stream(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (prec, usize::MAX),
            None => (usize::MAX, 3)
        };
        if prec < 4 {
            return Err(::std::fmt::Error)
        }
        let mut s = String::new();
        let mut i = 0;
        s.push('[');
        'a: {
            while s.len() < prec && i < max {
                match iter.next() {
                    None => {
                        s.push(']');
                        break 'a;
                    },
                    Some(Ok(item)) => {
                        let plen = s.len();
                        if i > 0 {
                            s += ", ";
                        }
                        s += &format!("{:.*}", prec - plen, item);
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

    #[doc(hidden)]
    fn writeout_string(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (prec, usize::MAX),
            None => (usize::MAX, 20)
        };
        if prec < 4 {
            return Err(::std::fmt::Error)
        }
        let mut s = String::new();
        let mut i = 0;
        s.push('"');
        'a: {
            while s.len() < prec && i < max {
                match iter.next() {
                    None => {
                        s.push('"');
                        break 'a;
                    },
                    Some(Ok(Item::Char(c))) => {
                        s += &format!("{c:#}");
                    },
                    _ => {
                        s += "<!>";
                        break 'a;
                    }
                };
                i += 1;
            }
            s += match iter.next() {
                None => "\"",
                Some(_) => "..."
            };
        }
        if s.len() < prec {
            write!(f, "{}", s)
        } else {
            write!(f, "{:.*}...", prec - 3, s)
        }
    }

    /// An indication whether this stream should be treated as a string. The implementation should
    /// only return `true` if it can be sure that the iterator will produce a stream of [`Char`]s.
    /// If so, this affects the behaviour of [`Stream::writeout()`].
    ///
    /// The default implementation returns `false`.
    fn is_string(&self) -> bool {
        false
    }

    /// Returns the length of this stream, in as much information as available *without* consuming
    /// the iterator. See [`Length`] for the possible return values. The default implementation
    /// relies on [`SIterator::len_remain()`] and [`Iterator::size_hint()`] to return one of
    /// `Exact`, `AtMost`, `Unknown` or `LikelyInfinite`. The latter is produced if `size_hint`
    /// returned `(usize::MAX, None)`, which is a customary indication of infiniteness in the
    /// standard library, but may have false positives, like an iterator whose size can't fit into
    /// `usize`.
    ///
    /// The return value must be consistent with the actual behaviour of the stream.
    fn length(&self) -> Length {
        use Length::*;
        let iter = self.iter();
        if let Some(len) = iter.len_remain() {
            return Exact(len);
        }
        match iter.size_hint() {
            (_, Some(hi)) => AtMost(hi.into()),
            (usize::MAX, None) => LikelyInfinite,
            _ => Unknown
        }
    }
}

impl Display for dyn Stream {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.writeout(f)
    }
}


/// The iterator trait returned by [`Stream::iter()`]. Every call to `next` returns either:
/// - `Some(Ok(item))`: any [`Item`] ready for direct consumption,
/// - `Some(Err(err))`: an error occurred at some point,
/// - `None`: the stream ended.
///
/// `next()` should not be called any more after *either* of the two latter conditions.
/// The iterators are not required to be fused and errors are not meant to be recoverable or
/// replicable, so the behaviour of doing so is undefined.
pub trait SIterator: Iterator<Item = Result<Item, StreamError>> {
    /// Returns the number of items remaining in the iterator, if it can be deduced from its
    /// current state. If it can't, or is known to be infinite, returns `None`.
    ///
    /// [`SIterator::skip_n`] may use this value for optimization. It is also used by the default
    /// implementation of [`Stream::length()`].
    fn len_remain(&self) -> Option<Number> {
        match self.size_hint() {
            (lo, Some(hi)) if lo == hi => Some(lo.into()),
            _ => None
        }
    }

    /// Inspired by (at the moment, experimental) `Iterator::advance_by()`, advances the iterator
    /// by `n` elements.
    ///
    /// The return value is `Ok(())` if `n` elements were skipped. If the iterator finishes early,
    /// the result is `Err(k)`, where `k` is the number of remaining elements. This is important to
    /// know when multiple iterators are chained. Calling `next()` after this condition is
    /// undefined behaviour.
    ///
    /// The default implementation calls `next()` an appropriate number of times, and thus is
    /// reasonably usable only for small values of `n`, except when `n` is found to exceed the
    /// value given by [`SIterator::len_remain()`].
    ///
    /// # Panics
    /// This function may panic if a negative value is passed in `n`.
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        assert!(!n.is_negative());
        if let Some(len) = self.len_remain() {
            if n > &len {
                return Err(n - len);
            }
        }
        let mut n = n.clone();
        let one = Number::one();
        while !n.is_zero() {
            if self.next().is_none() {
                return Err(n)
            }
            n -= &one;
        }
        Ok(())
    }
}

impl<T, U, V> SIterator for std::iter::Map<T, U>
where T: Iterator<Item = V>,
      U: FnMut(V) -> Result<Item, StreamError>
{ }


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


/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Imm(Item),
    Eval(Node)
}

/// A `Node` is a type of [`Expr`] representing a head object along with, optionally, its source
/// and arguments. This is an abstract representation, which may evaluate to a stream or an atomic
/// value, potentially depending on the nature of the source or arguments provided. This evaluation
/// happens in [`Session::eval()`](crate::session::Session::eval).
#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub head: Head,
    pub source: Option<Box<Expr>>,
    pub args: Vec<Expr>
}

/// The head of a [`Node`]. This can either be an identifier (`source.ident(args)`), or a body
/// formed by an entire expression (`source.{body}(args)`). In the latter case, the `source` and
/// `args` are accessed via `#` and `#1`, `#2` etc., respectively.
#[derive(Debug, PartialEq, Clone)]
pub enum Head {
    Symbol(String),
    Oper(String),
    Block(Box<Expr>)
}

impl Expr {
    /// Creates a new `Expr` of a value type.
    pub fn new_imm(item: Item) -> Expr {
        Expr::Imm(item)
    }

    /// Creates a new `Expr` of a node with a symbolic head.
    pub fn new_node(symbol: impl Into<String>, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{
            head: Head::Symbol(symbol.into()),
            source: source.map(Box::new),
            args
        })
    }

    /// Creates a new `Expr` of a node with an operation head.
    pub fn new_op(symbol: impl Into<String>, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{
            head: Head::Oper(symbol.into()),
            source: source.map(Box::new),
            args
        })
    }

    /// Creates a new `Expr` of a node with a block head.
    pub fn new_block(body: Expr, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{
            head: Head::Block(Box::new(body)),
            source: source.map(Box::new),
            args
        })
    }

    /// For an `Expr::Imm(value)`, returns a owned copy of the `value`.
    pub fn to_item(&self) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item.clone()),
            Expr::Eval(node) => Err(StreamError::from(format!("expected value, found {:?}", &node)))
        }
    }

    pub fn into_node(self) -> Result<Node, StreamError> {
        match self {
            Expr::Eval(node) => Ok(node),
            Expr::Imm(value) => Err(StreamError::from(format!("expected node, found {:?}", &value)))
        }
    }
}

impl From<Item> for Expr {
    fn from(item: Item) -> Expr {
        Expr::new_imm(item)
    }
}

impl Describe for Expr {
    fn describe(&self) -> String {
        match self {
            Expr::Imm(item) => item.describe(),
            Expr::Eval(node) => node.describe()
        }
    }
}

impl Node {
    pub(crate) fn check_args(self, source: bool, range: impl RangeBounds<usize>) -> Result<Node, StreamError> {
        use std::ops::Bound::*;
        match (&self.source, source) {
            (Some(_), false) => return Err(StreamError::new("no source accepted", self)),
            (None, true) => return Err(StreamError::new("source requested", self)),
            _ => { }
        };
        if range.contains(&self.args.len()) {
            Ok(self)
        } else {
            Err(StreamError::new(
                    match (range.start_bound(), range.end_bound()) {
                        (Included(0), Included(0)) => "no arguments allowed".to_string(),
                        _ => format!("{} arguments required", describe_range(&range))
                    }, self))
        }
    }

    pub(crate) fn eval_all(self, session: &Session) -> Result<Node, StreamError> {
        let source = self.source.map(|x| session.eval(*x))
            .transpose()?
            .map(|x| Box::new(Expr::new_imm(x)));
        let args = self.args.into_iter()
            .map(|x| session.eval(x).map(|x| Expr::new_imm(x)))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Node{head: self.head, source, args})
    }

    pub(crate) fn eval_source(self, session: &Session) -> Result<Node, StreamError> {
        let source = self.source.map(|x| session.eval(*x))
            .transpose()?
            .map(|x| Box::new(Expr::new_imm(x)));
        Ok(Node{head: self.head, source, args: self.args})
    }

    pub(crate) fn with<T, F>(self, f: F) -> Result<T, StreamError>
        where F: FnOnce(&Node) -> Result<T, StreamError>
    {
        f(&self).map_err(|e| e.with_node(self))
    }
}

impl Describe for Node {
    fn describe(&self) -> String {
        let mut ret = String::new();
        if let Some(source) = &self.source {
            ret += &source.describe();
            ret.push('.');
        }
        match &self.head {
            Head::Symbol(s) => ret += &s,
            Head::Block(b) => {
                ret.push('{');
                ret += &b.describe();
                ret.push('}');
            },
            Head::Oper(o) => { // special, early return
                ret.push('(');
                let mut it = self.args.iter();
                if self.args.len() > 1 {
                    ret += &it.next().unwrap().describe();
                }
                for expr in it {
                    ret += &o;
                    ret += &expr.describe();
                }
                ret.push(')');
                return ret;
            }
        };
        if self.args.len() > 0 {
            let mut it = self.args.iter();
            ret.push('(');
            ret += &it.next().unwrap().describe();
            for expr in it {
                ret += ", ";
                ret += &expr.describe();
            }
            ret.push(')');
        }
        ret
    }
}

#[test]
fn test_describe() {
    use crate::parser::parse;

    // chaining, args, block
    let orig = parse("a.b(c,d).{e}(f,g)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // value types
    let orig = parse("[1,true,'a\"b']").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);
    // TODO: strings

    // character escaping
    let orig = parse("'a\\n\\r\\t\\'\\\"'").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // operators, precedence
    let orig = parse("1+(-2-3-4^(-5)*2)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // lists, parts
    let orig = parse("[1,2][3,4] + [[1,2]][[3,4]]").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);
}
