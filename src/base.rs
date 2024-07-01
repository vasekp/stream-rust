use std::fmt::{Display, Formatter, Debug};
use std::ops::RangeBounds;
use dyn_clone::DynClone;
use crate::utils::describe_range;
use num::{Signed, One, Zero};
use crate::keywords::find_keyword;
use std::cell::Cell;
use std::rc::Rc;
pub use crate::alphabet::*;

/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = num::BigInt;

pub(crate) trait NumWithin : PartialOrd {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), BaseError>;
}

impl NumWithin for Number {
    fn check_within(&self, range: impl RangeBounds<Self>) -> Result<(), BaseError> {
        match range.contains(self) {
            true => Ok(()),
            false => Err(format!("expected {}, found {}", describe_range(&range), &self).into())
        }
    }
}


/// A trait for the ability to turn a Stream language object (notably, [`Expr`]) into an input form.
pub trait Describe {
    /// Construct a string representation of `self`. This is meant for storing object across
    /// sessions. The resulting `String` must be a syntactically valid input that reconstruct a
    /// copy of the original object on [`parser::parse()`](crate::parser::parse()) and
    /// [`Expr::eval()`].
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

    pub fn as_num(&self) -> Result<&Number, BaseError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }

    pub fn as_num_mut(&mut self) -> Result<&mut Number, BaseError> {
        match self {
            Item::Number(ref mut x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }

    /*pub fn to_num(&self) -> Result<Number, BaseError> {
        self.as_num().map(ToOwned::to_owned)
    }*/

    pub fn check_num(&self) -> Result<(), BaseError> {
        self.as_num().map(|_| ())
    }

    /*pub fn into_num(self) -> Result<Number, BaseError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }*/

    pub fn as_char(&self) -> Result<&Char, BaseError> {
        match self {
            Item::Char(c) => Ok(c),
            _ => Err(format!("expected char, found {:?}", &self).into())
        }
    }

    pub fn as_stream(&self) -> Result<&dyn Stream, BaseError> {
        match self {
            Item::Stream(s) => Ok(&**s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn into_stream(self) -> Result<Box<dyn Stream>, BaseError> {
        match self {
            Item::Stream(s) => Ok(s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn to_stream(&self) -> Result<Box<dyn Stream>, BaseError> {
        match self {
            Item::Stream(s) => Ok(dyn_clone::clone_box(&**s)),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn format(&self, max_len: usize) -> (String, FormatError) {
        struct Stateful<'item> {
            item: &'item Item,
            cell: Cell<FormatError>
        }

        impl<'item> Display for Stateful<'item> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.item.format_int(f, &self.cell)
            }
        }

        let s = Stateful{item: self, cell: Default::default()};
        let result = format!("{:.*}", max_len, s);
        (result, s.cell.take())
    }

    pub(crate) fn format_int(&self, f: &mut Formatter<'_>, error: &Cell<FormatError>)
        -> std::fmt::Result
    {
        use Item::*;
        match self {
            Number(n) => write!(f, "{n}"),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "{c}"),
            Stream(s) => (*s).writeout(f, error)
        }
    }

    pub(crate) fn type_str(&self) -> &'static str {
        use Item::*;
        match self {
            Number(_) => "number",
            Bool(_) => "bool",
            Char(_) => "char",
            Stream(s) if s.is_string() => "string",
            Stream(_) => "stream"
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.format_int(f, &Default::default())
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.type_str())?;
        self.format_int(f, &Default::default())
    }
}

impl Describe for Item {
    fn describe(&self) -> String {
        use Item::*;
        match self {
            Number(n) if n.is_negative() => format!("({n})"),
            Number(n) => format!("{n}"),
            Bool(b) => format!("{b}"),
            Char(c) => format!("{c}"),
            Stream(s) => s.describe()
        }
    }
}

impl PartialEq for Item {
    /// `PartialEq::eq()` must be used with caution because if asked of two infinite streams it
    /// will never return.
    fn eq(&self, other: &Self) -> bool {
        use Item::*;
        match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            (Stream(x1), Stream(x2)) => {
                let l1 = x1.length();
                let l2 = x2.length();
                if !Length::possibly_eq(&l1, &l2) { return false; }
                x1.iter().zip(x2.iter())
                    .all(|(x, y)| x == y)
            },
            _ => false
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


/// The runtime error type with an indication of the [`Node`] whose evaluation caused it.
#[derive(PartialEq, Debug)]
pub struct StreamError {
    reason: BaseError,
    node: Node
}

impl StreamError {
    pub fn new<T>(reason: T, node: Node) -> StreamError where T: Into<BaseError> {
        StreamError{reason: reason.into(), node}
    }
}

impl std::error::Error for StreamError { }

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.node.describe(), self.reason)
    }
}

/// The base error returned by helper functions. In most situations this is intended to be
/// turned into [`StreamError`] by supplementing a [`Node`].
#[derive(Debug, PartialEq)]
pub struct BaseError(String);

impl<T> From<T> for BaseError where T: Into<String> {
    fn from(string: T) -> BaseError {
        BaseError(string.into())
    }
}

impl Display for BaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A special error type which can hold both [`StreamError`] or [`BaseError`], i.e., has an
/// optional [`Node`] attached.
///
/// The situation where a [`Node`] is not available can happen in [`Stream::writeout`] for strings
/// when an [`Item`] fails to be a [`Item::Char`]. For that reason this error type is returned by
/// [`Item::format`].
#[derive(Default, PartialEq)]
pub enum FormatError {
    #[default]
    None,
    StreamError(StreamError),
    BaseError(BaseError)
}

impl FormatError {
    pub fn is_some(&self) -> bool {
        self != &FormatError::None
    }
}

impl From<StreamError> for FormatError {
    fn from(err: StreamError) -> FormatError {
        FormatError::StreamError(err)
    }
}

impl From<BaseError> for FormatError {
    fn from(err: BaseError) -> FormatError {
        FormatError::BaseError(err)
    }
}

impl Display for FormatError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatError::None => write!(f, "no error"),
            FormatError::BaseError(err) => write!(f, "{err}"),
            FormatError::StreamError(err) => write!(f, "{err}"),
        }
    }
}


/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream: DynClone + Describe {
    /// Create an [`SIterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    #[must_use]
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node>;

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
    fn writeout(&self, f: &mut Formatter<'_>, error: &Cell<FormatError>)
        -> std::fmt::Result
    {
        if self.is_string() {
            self.writeout_string(f, error)
        } else {
            self.writeout_stream(f, error)
        }
    }

    #[doc(hidden)]
    fn writeout_stream(&self, f: &mut Formatter<'_>, error: &Cell<FormatError>)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (std::cmp::max(prec, 4), usize::MAX),
            None => (usize::MAX, 3)
        };
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
                        let (string, err) = item.format(prec - plen);
                        s += &string;
                        if err.is_some() {
                            error.set(err);
                            break 'a;
                        }
                    },
                    Some(Err(err)) => {
                        if i > 0 {
                            s += ", ";
                        }
                        s += "<!>";
                        error.set(err.into());
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
    fn writeout_string(&self, f: &mut Formatter<'_>, error: &Cell<FormatError>)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (std::cmp::max(prec, 4), usize::MAX),
            None => (usize::MAX, 20)
        };
        let mut s = String::new();
        let mut i = 0;
        s.push('"');
        'a: {
            while s.len() < prec && i < max {
                if let Some(next) = iter.next() {
                    match next {
                        Ok(item) => match item.as_char() {
                            Ok(ch) => s += &format!("{ch:#}"),
                            Err(err) => {
                                s += "<!>";
                                error.set(err.into());
                                break 'a;
                            }
                        },
                        Err(err) => {
                            s += "<!>";
                            error.set(err.into());
                            break 'a;
                        }
                    }
                } else {
                    s.push('"');
                    break 'a;
                }
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
    /// `Exact`, `AtMost`, or `Unknown`.
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
            _ => Unknown
        }
    }

    /// Checks for emptiness. The default implementation first tries to answer statically from
    /// looking at [`length()`]. If the information is insufficient, constructs the iterator and
    /// tries answering using `Iterator::size_hint()`. As a last resort, the iterator is consumed.
    ///
    /// This function can't return an error. If the first call to `iter().next()` produces an
    /// error, i.e. `Some(Err(_))`, it's reported that the stream is nonempty.
    fn is_empty(&self) -> bool {
        match self.length() {
            Length::Exact(len) => len.is_zero(),
            Length::Infinite => false,
            _ => {
                let mut iter = self.iter();
                match iter.size_hint() {
                    (1.., _) => false,
                    (0, Some(0)) => true,
                    _ => {
                        match iter.next() {
                            None => true,
                            Some(_) => false
                        }
                    }
                }
            }
        }
    }
}

impl Display for dyn Stream {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.writeout(f, &Default::default())
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
    /// implementation of [`Stream::length()`]. If you override both methods then you don't need
    /// to override this one, unless you want to use it for similar purposes.
    fn len_remain(&self) -> Option<Number> {
        match self.size_hint() {
            (lo, Some(hi)) if lo == hi => Some(lo.into()),
            _ => None
        }
    }

    /// Inspired by (at the moment, experimental) `Iterator::advance_by()`, advances the iterator
    /// by `n` elements.
    ///
    /// The return value is `Ok(None)` if `n` elements were skipped. If the iterator finishes
    /// early, the result is `Ok(Some(k))`, where `k` is the number of remaining elements. This is
    /// important to know when multiple iterators are chained. Calling `next()` after this
    /// condition, or after an `Err` is returned, is undefined behaviour.
    ///
    /// The default implementation calls `next()` an appropriate number of times, and thus is
    /// reasonably usable only for small values of `n`, except when `n` is found to exceed the
    /// value given by [`SIterator::len_remain()`].
    ///
    /// # Panics
    /// This function may panic if a negative value is passed in `n`.
    fn skip_n(&mut self, mut n: Number) -> Result<Option<Number>, StreamError> {
        assert!(!n.is_negative());
        if let Some(len) = self.len_remain() {
            if n > len {
                return Ok(Some(n - len));
            }
        }
        let one = Number::one();
        while !n.is_zero() {
            match self.next() {
                Some(Ok(_)) => (),
                Some(Err(err)) => return Err(err),
                None => return Ok(Some(n))
            }
            n -= &one;
        }
        Ok(None)
    }
}

impl<T, U, V> SIterator for std::iter::Map<T, U>
where T: Iterator<Item = V>,
      U: FnMut(V) -> Result<Item, StreamError>
{ }

impl SIterator for std::iter::Once<Result<Item, StreamError>> { }

pub struct Repeat<'node> {
    pub item: &'node Item
}

impl Iterator for Repeat<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok(self.item.clone()))
    }
}

impl SIterator for Repeat<'_> {
    fn skip_n(&mut self, _n: Number) -> Result<Option<Number>, StreamError> {
        Ok(None)
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
    /// The length is not known but promises to be finite.
    UnknownFinite,
    /// Nothing can be inferred about the length.
    Unknown
}

impl Length {
    pub fn at_most(value: &Length) -> Length {
        use Length::*;
        match value {
            Exact(x) => AtMost(x.to_owned()),
            AtMost(x) => AtMost(x.to_owned()),
            UnknownFinite => UnknownFinite,
            _ => Unknown
        }
    }

    pub fn possibly_eq(l1: &Length, l2: &Length) -> bool {
        use Length::*;
        match (l1, l2) {
            (Unknown, _) | (_, Unknown) => true,
            (Infinite, Infinite) => true,
            (Infinite, _) | (_, Infinite) => false,
            (Exact(x), Exact(y)) => x == y,
            (Exact(x), AtMost(y)) => x <= y,
            (AtMost(x), Exact(y)) => y <= x,
            _ => true
        }
    }

    pub fn smaller(l1: &Length, l2: &Length) -> Length {
        use Length::*;
        match (l1, l2) {
            (Infinite, len) | (len, Infinite) => len.to_owned(),
            (Unknown, len) | (len, Unknown) => Length::at_most(len),
            // can't be merged with previous, otherwise (UnkFin, Unk) would give at_most(Unk) == Unk
            (UnknownFinite, len) | (len, UnknownFinite) => Length::at_most(len),
            (Exact(x), Exact(y)) => Exact(std::cmp::min(x, y).to_owned()),
            (Exact(x) | AtMost(x), Exact(y) | AtMost(y)) => AtMost(std::cmp::min(x, y).to_owned())
        }
    }
}

impl<T> From<T> for Length where T: Into<Number> {
    fn from(value: T) -> Self {
        Length::Exact(value.into())
    }
}

impl std::ops::Add for Length {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        use Length::*;
        match (self, rhs) {
            (_, Infinite) | (Infinite, _) => Infinite,
            (_, Unknown) | (Unknown, _) => Unknown,
            (_, UnknownFinite) | (UnknownFinite, _) => UnknownFinite,
            (Exact(a), Exact(b)) => Exact(a + b),
            (Exact(a) | AtMost(a), Exact(b) | AtMost(b)) => AtMost(a + b)
        }
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
/// happens in [`Expr::eval()`].
#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub head: Head,
    pub source: Option<Box<Expr>>,
    pub args: Vec<Expr>
}

#[derive(Default, Clone)]
pub struct Env { }

impl Env {
    fn is_trivial(&self) -> bool { true }

    pub(crate) fn wrap_describe(&self, expr: impl Into<String> + std::fmt::Display) -> String {
        match self.is_trivial() {
            true => expr.into(),
            false => format!("env({}, {})", self.describe(), expr)
        }
    }

    pub fn alphabet(&self) -> &Alphabet { &Alphabet::Std26 }
}

impl Describe for Env {
    fn describe(&self) -> String { todo!() }
}

/// The head of a [`Node`]. This can either be an identifier (`source.ident(args)`), or a body
/// formed by an entire expression (`source.{body}(args)`). In the latter case, the `source` and
/// `args` are accessed via `#` and `#1`, `#2` etc., respectively.
#[derive(Debug, PartialEq, Clone)]
pub enum Head {
    Symbol(String),
    Oper(String),
    Block(Box<Expr>),
    Lang(LangItem),
    Repl(char, Option<usize>)
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LangItem {
    List,
    Part,
    Map,
    Args
}

impl LangItem {
    fn keyword(&self) -> &'static str {
        use LangItem::*;
        match self {
            List => "$list",
            Part => "$part",
            Map => "$map",
            Args => "$args"
        }
    }
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

    /// Creates a new `Expr` of a node with an operation head.
    pub fn new_repl(chr: char, ix: Option<usize>) -> Expr {
        Expr::Eval(Node{
            head: Head::Repl(chr, ix),
            source: None,
            args: vec![]
        })
    }

    pub(crate) fn new_lang(head: LangItem, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{
            head: Head::Lang(head),
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

    /// For an `Expr::Imm(value)`, returns a reference to `value`.
    pub fn as_item(&self) -> Result<&Item, BaseError> {
        match self {
            Expr::Imm(ref item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn as_item_mut(&mut self) -> Result<&mut Item, BaseError> {
        match self {
            Expr::Imm(ref mut item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    /// For an `Expr::Imm(value)`, returns a owned copy of the `value`.
    pub fn to_item(&self) -> Result<Item, BaseError> {
        match self {
            Expr::Imm(item) => Ok(item.clone()),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn into_item(self) -> Result<Item, BaseError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn to_node(&self) -> Result<Node, BaseError> {
        match self {
            Expr::Eval(node) => Ok(node.to_owned()),
            Expr::Imm(imm) => Err(format!("expected node, found {:?}", imm).into()),
        }
    }

    pub(crate) fn apply(self, source: &Option<Box<Expr>>, args: &Vec<Expr>) -> Result<Expr, StreamError> {
        match self {
            Expr::Imm(_) => Ok(self),
            Expr::Eval(node) => match node.head {
                Head::Repl('#', None) => source.as_ref()
                        .ok_or(StreamError::new("no source provided", node))
                        .map(|boxed| (**boxed).clone()),
                Head::Repl('#', Some(ix)) => args.get(ix - 1)
                    .ok_or(StreamError::new("no such input", node))
                    .cloned(),
                _ => Ok(Expr::Eval(node.apply(source, args)?))
            }
        }
    }

    /// Evaluates this `Expr`. If it already describes an `Item`, returns that, otherwise calls
    /// `Node::eval()`.
    pub fn eval(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => node.eval(env)
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
    /*pub(crate) fn check_no_source(&self) -> Result<(), BaseError> {
        match &self.source {
            Some(_) => Err("no source accepted".into()),
            None => Ok(())
        }
    }*/

    pub(crate) fn source_checked(&self) -> Result<&Expr, BaseError> {
        match &self.source {
            Some(source) => Ok(source),
            None => Err("source required".into()),
        }
    }

    /*pub(crate) fn check_args_nonempty(&self) -> Result<(), BaseError> {
        if self.args.is_empty() {
            Err("at least 1 argument required".into())
        } else {
            Ok(())
        }
    }*/

    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// The evaluation is done by finding the head of the node in a global keyword table.
    /// Locally defined symbols aren't handled here.
    // Note to self: for assignments, this will happen in Session::process. For `with`, this will
    // happen in Expr::apply(Context).
    pub fn eval(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self.head {
            Head::Symbol(ref sym) | Head::Oper(ref sym) => match find_keyword(sym) {
                Ok(func) => func(self, env),
                Err(e) => Err(StreamError::new(e, self))
            },
            Head::Block(blk) => (*blk).apply(&self.source, &self.args)?.eval(env),
            Head::Repl(_, _) => Err(StreamError::new("out of context", self)),
            Head::Lang(ref lang) => find_keyword(lang.keyword()).unwrap()(self, env)
        }
    }

    pub(crate) fn eval_all(self, env: &Rc<Env>) -> Result<ENode, StreamError> {
        let source = match self.source {
            Some(source) => Some((*source).eval(env)?),
            None => None
        };
        let args = self.args.into_iter()
            .map(|x| x.eval(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ENode{head: self.head, source, args})
    }

    pub(crate) fn eval_source(self, env: &Rc<Env>) -> Result<Node, StreamError> {
        let source = self.source.map(|x| (*x).eval(env))
            .transpose()?
            .map(|x| Box::new(Expr::new_imm(x)));
        Ok(Node{head: self.head, source, args: self.args})
    }

    pub(crate) fn eval_args(self, env: &Rc<Env>) -> Result<Node, StreamError> {
        let args = self.args.into_iter()
            .map(|x| x.eval(env).map(Expr::new_imm))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Node{head: self.head, source: self.source, args})
    }

    pub(crate) fn apply(self, source: &Option<Box<Expr>>, args: &Vec<Expr>) -> Result<Node, StreamError> {
        Ok(Node {
            head: self.head,
            source: match self.source {
                None => None,
                Some(boxed) => Some(Box::new((*boxed).apply(source, args)?))
            },
            args: self.args.into_iter()
                .map(|expr| expr.apply(source, args))
                .collect::<Result<Vec<_>, _>>()?
        })
    }

    pub(crate) fn describe_helper<T>(head: &Head, source: Option<&T>, args: &[T]) -> String
        where T: Describe
    {
        let mut ret = String::new();
        if let Some(source) = source {
            ret += &source.describe();
            match head {
                Head::Lang(LangItem::Map) => ret.push(':'),
                Head::Lang(LangItem::Args) => ret.push('@'),
                Head::Lang(LangItem::Part) => (),
                _ => ret.push('.')
            }
        }
        match head {
            Head::Symbol(s) => ret += s,
            Head::Block(b) => {
                ret.push('{');
                ret += &b.describe();
                ret.push('}');
            },
            Head::Oper(o) => { // special, early return
                ret.push('(');
                let mut it = args.iter().map(Describe::describe);
                if args.len() > 1 { // if len == 1, print {op}{arg}, otherwise {arg}{op}{arg}...
                    if let Some(s) = it.next() {
                        ret += &s;
                    }
                }
                for s in it {
                    ret += o;
                    ret += &s;
                }
                ret.push(')');
                return ret;
            },
            Head::Repl(chr, opt) => {
                ret.push(*chr);
                if let Some(num) = opt {
                    ret += &format!("{num}");
                }
            },
            Head::Lang(_) => ()
        };
        if !args.is_empty() {
            match head {
                Head::Lang(LangItem::Part | LangItem::List) => ret.push('['),
                Head::Lang(LangItem::Map) => (),
                _ => ret.push('(')
            };
            let mut it = args.iter().map(Describe::describe);
            if let Some(s) = it.next() {
                ret += &s;
                for s in it {
                    ret += ", ";
                    ret += &s
                }
            }
            match head {
                Head::Lang(LangItem::Part | LangItem::List) => ret.push(']'),
                Head::Lang(LangItem::Map) => (),
                _ => ret.push(')')
            };
        }
        ret
    }
}

macro_rules! try_with {
    ($node:ident, $expr:expr) => {
        match (|| -> Result<_, BaseError> { $expr })() {
            Ok(result) => result,
            Err(err) => return Err(StreamError::new(err, $node.into()))
        }
    }
}

pub(crate) use try_with;


#[test]
fn test_block() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("{#1}(3,4)").unwrap().eval(&env).unwrap().to_string(), "3");
    assert_eq!(parse("{#2}(3,4)").unwrap().eval(&env).unwrap().to_string(), "4");
    assert!(parse("{#3}(3,4)").unwrap().eval(&env).is_err());
    assert!(parse("#1").unwrap().eval(&env).is_err());
    assert_eq!(parse("1.{2}(3)").unwrap().eval(&env).unwrap().to_string(), "2");
    assert_eq!(parse("{#1+{#1}(2,3)}(4,5)").unwrap().eval(&env).unwrap().to_string(), "6");
    assert_eq!(parse("{#1}({#2}(3,4),5)").unwrap().eval(&env).unwrap().to_string(), "4");
}

impl Describe for Node {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, self.source.as_deref(), &self.args)
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
    let orig = parse(r#"[1,true,'a"\'b\nc',"a'b\"c\n"]"#).unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

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

    // map
    let orig = parse("a:b:c").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // args
    let orig = parse("a@b@c(d)[e]").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);
}


/// A variant of `Node` in which all the arguments and source are type-guaranteed to be evaluated.
/// This is achieved by using `Item` instead of `Expr`, avoiding the possibility of `Expr::Eval`.
#[derive(Debug, PartialEq, Clone)]
pub struct ENode {
    pub head: Head,
    pub source: Option<Item>,
    pub args: Vec<Item>
}

impl ENode {
    pub(crate) fn check_no_source(&self) -> Result<(), BaseError> {
        match &self.source {
            Some(_) => Err("no source accepted".into()),
            None => Ok(())
        }
    }

    pub(crate) fn source_checked(&self) -> Result<&Item, BaseError> {
        match &self.source {
            Some(source) => Ok(source),
            None => Err("source required".into()),
        }
    }

    pub(crate) fn check_args_nonempty(&self) -> Result<(), BaseError> {
        if self.args.is_empty() {
            Err("at least 1 argument required".into())
        } else {
            Ok(())
        }
    }
}

impl From<ENode> for Node {
    fn from(enode: ENode) -> Node {
        Node {
            head: enode.head,
            source: enode.source.map(|item| Box::new(Expr::new_imm(item))),
            args: enode.args.into_iter().map(Expr::new_imm).collect()
        }
    }
}

impl Describe for ENode {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, self.source.as_ref(), &self.args)
    }
}
