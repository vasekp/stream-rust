use std::fmt::{Display, Formatter, Debug};
use std::ops::RangeBounds;
use dyn_clone::DynClone;
use crate::utils::describe_range;
use num::{Signed, One, Zero};
use crate::keywords::find_keyword;
use std::cell::Cell;
pub use crate::alphabet::*;

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

    pub fn as_num(&self) -> Result<&Number, StreamError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }

    pub fn to_num(&self) -> Result<Number, StreamError> {
        self.as_num().map(ToOwned::to_owned)
    }

    pub fn check_num(&self) -> Result<(), StreamError> {
        self.as_num().map(|_| ())
    }

    pub fn into_num(self) -> Result<Number, StreamError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }

    pub fn as_char(&self) -> Result<&Char, StreamError> {
        match self {
            Item::Char(c) => Ok(c),
            _ => Err(format!("expected char, found {:?}", &self).into())
        }
    }

    pub fn as_stream(&self) -> Result<&dyn Stream, StreamError> {
        match self {
            Item::Stream(s) => Ok(&**s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn into_stream(self) -> Result<Box<dyn Stream>, StreamError> {
        match self {
            Item::Stream(s) => Ok(s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn format(&self, max_len: usize) -> (String, Option<StreamError>) {
        struct Stateful<'item> {
            item: &'item Item,
            cell: ErrorCell
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

    pub(crate) fn format_int(&self, f: &mut Formatter<'_>, error: &ErrorCell)
        -> std::fmt::Result
    {
        use Item::*;
        match self {
            Number(n) => write!(f, "{n}"),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "'{c}'"),
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


/// The runtime error type with an optional indication of the [`Node`] whose construction or
/// evaluation caused it.
///
/// Even though `node` is optional, the intended use is that every function supplies it.
#[derive(PartialEq, Debug)]
pub struct StreamError {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.node {
            Some(node) => write!(f, "{}: {}", node.describe(), self.reason),
            None => write!(f, "{}", self.reason)
        }
    }
}

type ErrorCell = Cell<Option<StreamError>>;


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
    fn writeout(&self, f: &mut Formatter<'_>, error: &ErrorCell)
        -> std::fmt::Result
    {
        if self.is_string() {
            self.writeout_string(f, error)
        } else {
            self.writeout_stream(f, error)
        }
    }

    #[doc(hidden)]
    fn writeout_stream(&self, f: &mut Formatter<'_>, error: &ErrorCell)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (prec, usize::MAX),
            None => (usize::MAX, 3)
        };
        if prec < 4 {
            return Err(std::fmt::Error)
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
                        let (string, err) = item.format(prec - plen);
                        s += &string;
                        if err.is_some() {
                            error.set(err);
                            break 'a;
                        }
                    },
                    Some(Err(err)) => {
                        s += "<!>";
                        error.set(Some(err));
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
    fn writeout_string(&self, f: &mut Formatter<'_>, error: &ErrorCell)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (prec, usize::MAX),
            None => (usize::MAX, 20)
        };
        if prec < 4 {
            return Err(std::fmt::Error)
        }
        let mut s = String::new();
        let mut i = 0;
        s.push('"');
        'a: {
            while s.len() < prec && i < max {
                match iter.next().map(|res| res.and_then(|it| Ok(it.as_char()?.to_owned()))) {
                    None => {
                        s.push('"');
                        break 'a;
                    },
                    Some(Ok(c)) => {
                        s += &format!("{c:#}");
                    },
                    Some(Err(err)) => {
                        s += "<!>";
                        error.set(Some(err));
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
    fn skip_n(&mut self, mut n: Number) -> Result<(), Number> {
        assert!(!n.is_negative());
        if let Some(len) = self.len_remain() {
            if n > len {
                return Err(n - len);
            }
        }
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
    Repl(char, Option<usize>)
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

    /// Creates a new `Expr` of a node with a block head.
    pub fn new_block(body: Expr, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{
            head: Head::Block(Box::new(body)),
            source: source.map(Box::new),
            args
        })
    }

    /// For an `Expr::Imm(value)`, returns a reference to `value`.
    pub fn as_item(&self) -> Result<&Item, StreamError> {
        match self {
            Expr::Imm(ref item) => Ok(item),
            _ => Err(format!("expected value, found {:?}", self).into()),
        }
    }

    /// For an `Expr::Imm(value)`, returns a owned copy of the `value`.
    pub fn to_item(&self) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item.clone()),
            _ => Err(format!("expected value, found {:?}", self).into()),
        }
    }

    pub fn to_node(&self) -> Result<Node, StreamError> {
        match self {
            Expr::Eval(node) => Ok(node.to_owned()),
            _ => Err(format!("expected node, found {:?}", self).into()),
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
    pub fn eval(self, env: &Env) -> Result<Item, StreamError> {
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
    pub(crate) fn check_no_source(&self) -> Result<(), StreamError> {
        match &self.source {
            Some(_) => Err("no source accepted".into()),
            None => Ok(())
        }
    }

    pub(crate) fn source_checked(&self) -> Result<&Expr, StreamError> {
        match &self.source {
            Some(source) => Ok(source),
            None => Err("source required".into()),
        }
    }

    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// The evaluation is done by finding the head of the node in a global keyword table.
    /// Locally defined symbols aren't handled here.
    // Note to self: for assignments, this will happen in Session::process. For `with`, this will
    // happen in Expr::apply(Context).
    pub fn eval(self, env: &Env) -> Result<Item, StreamError> {
        match self.head {
            Head::Symbol(ref sym) | Head::Oper(ref sym) => match find_keyword(sym) {
                Ok(func) => func(self, env),
                Err(e) => Err(e.with_node(self))
            },
            Head::Block(blk) => (*blk).apply(&self.source, &self.args)?.eval(env),
            Head::Repl(_, _) => Err(StreamError::new("out of context", self))
        }
    }

    pub(crate) fn eval_all(self, env: &Env) -> Result<Node, StreamError> {
        let source = self.source.map(|x| (*x).eval(env))
            .transpose()?
            .map(|x| Box::new(Expr::new_imm(x)));
        let args = self.args.into_iter()
            .map(|x| x.eval(env).map(Expr::new_imm))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Node{head: self.head, source, args})
    }

    pub(crate) fn eval_source(self, env: &Env) -> Result<Node, StreamError> {
        let source = self.source.map(|x| (*x).eval(env))
            .transpose()?
            .map(|x| Box::new(Expr::new_imm(x)));
        Ok(Node{head: self.head, source, args: self.args})
    }

    pub(crate) fn with<T, F>(self, f: F) -> Result<T, StreamError>
        where F: FnOnce(&Node) -> Result<T, StreamError>
    {
        f(&self).map_err(|e| e.with_node(self))
    }

    pub(crate) fn with_keep<T, F>(self, f: F) -> Result<(T, Node), StreamError>
        where F: FnOnce(&Node) -> Result<T, StreamError>
    {
        match f(&self) {
            Ok(ret) => Ok((ret, self)),
            Err(err) => Err(err.with_node(self))
        }
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
}


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
        let mut ret = String::new();
        if let Some(source) = &self.source {
            ret += &source.describe();
            ret.push('.');
        }
        match &self.head {
            Head::Symbol(s) => ret += s,
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
                    ret += o;
                    ret += &expr.describe();
                }
                ret.push(')');
                return ret;
            },
            Head::Repl(chr, opt) => {
                ret.push(*chr);
                if let Some(num) = opt {
                    ret += &format!("{num}");
                }
            }
        };
        if !self.args.is_empty() {
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
