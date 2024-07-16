use std::fmt::{Display, Formatter, Debug};
use dyn_clone::DynClone;
pub use num::*;
pub use crate::error::*;
pub use crate::alphabet::Char;
use crate::keywords::find_keyword;
use std::cell::Cell;
use std::rc::Rc;
use crate::alphabet::*;

/// The type for representing all numbers in Stream. The requirement is that it allows
/// arbitrary-precision integer arithmetics. Currently alias to BigInt, but may become an i64 with
/// BigInt fallback in the future for better performance.
pub type Number = num::BigInt;


/// A trait for the ability to turn a Stream language object (notably, [`Expr`]) into an input form.
pub trait Describe {
    /// Construct a string representation of `self`. This is meant for storing object across
    /// sessions. The resulting `String` must be a syntactically valid input that reconstruct a
    /// copy of the original object on [`parser::parse()`](crate::parser::parse()) and
    /// [`Expr::eval()`].
    fn describe(&self) -> String;
}


/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Imm(Item),
    Eval(Node)
}

impl Default for Expr {
    fn default() -> Expr {
        Expr::Imm(Default::default())
    }
}

impl Expr {
    pub fn new_number(value: impl Into<Number>) -> Expr {
        Item::new_number(value).into()
    }

    pub fn new_bool(value: bool) -> Expr {
        Item::new_bool(value).into()
    }

    pub fn new_char(value: impl Into<Char>) -> Expr {
        Item::new_char(value).into()
    }

    /*pub fn new_stream(value: impl Stream + 'static) -> Expr {
        Item::new_stream(value).into()
    }*/

    pub fn new_string(value: impl Into<String>) -> Expr {
        Item::new_string(value).into()
    }

    pub fn new_node(head: impl Into<Head>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{head: head.into(), source: None, args})
    }

    /// Operands are provided as `args`.
    pub fn new_op(op: impl Into<String>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{head: Head::Oper(op.into()), source: None, args})
    }

    /// Creates a new `Node` with a [`Head::Repl`] head (for `#1` etc.). By nature these don't take
    /// any arguments or source.
    pub fn new_repl(chr: char, ix: Option<usize>) -> Expr {
        Expr::Eval(Node{
            head: Head::Repl(chr, ix),
            source: None,
            args: vec![]
        })
    }

    pub fn chain(self, next: PreNode) -> Expr {
        Expr::Eval(Node{
            head: next.head,
            source: Some(Box::new(self)),
            args: next.args
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

    /// Evaluates this `Expr` in a default environment.
    pub fn eval(self) -> Result<Item, StreamError> {
        self.eval_env(&Default::default())
    }

    /// Evaluates this `Expr`. If it already describes an `Item`, returns that, otherwise calls
    /// `Node::eval_env()`.
    pub fn eval_env(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => node.eval(env)
        }
    }
}

impl From<Item> for Expr {
    fn from(item: Item) -> Expr {
        Expr::Imm(item)
    }
}

impl<T> From<T> for Expr where T: Into<Node> {
    fn from(item: T) -> Expr {
        Expr::Eval(item.into())
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

impl Node {
    /// Creates a new `Node`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, source: Option<Expr>, args: Vec<Expr>) -> Node {
        Node{head: head.into(), source: source.map(Box::new), args}
    }

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
            Head::Block(blk) => (*blk).apply(&self.source, &self.args)?.eval_env(env),
            Head::Repl(_, _) => Err(StreamError::new("out of context", self)),
            Head::Lang(ref lang) => find_keyword(lang.keyword()).unwrap()(self, env)
        }
    }

    pub(crate) fn eval_all(self, env: &Rc<Env>) -> Result<ENode, StreamError> {
        let source = match self.source {
            Some(source) => Some((*source).eval_env(env)?),
            None => None
        };
        let args = self.args.into_iter()
            .map(|x| x.eval_env(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ENode{head: self.head, source, args})
    }

    pub(crate) fn eval_source(mut self, env: &Rc<Env>) -> Result<Node, StreamError> {
        if let Some(source) = self.source.take() {
            self.source = Some(Box::new((*source).eval_env(env)?.into()));
        }
        Ok(self)
    }

    /*pub(crate) fn eval_args(mut self, env: &Rc<Env>) -> Result<Node, StreamError> {
        self.args = self.args.into_iter()
            .map(|x| x.eval_env(env).map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self)
    }*/

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
                Head::Lang(LangItem::Part) => (),
                _ => ret.push('.')
            }
        }
        ret += &(*head).describe();
        if let Head::Oper(o) = head {
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
        } else if !args.is_empty() {
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
        } else if head == &Head::Lang(LangItem::List) {
            ret += "[]";
        }
        ret
    }
}

impl Describe for Node {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, self.source.as_deref(), &self.args)
    }
}


/// A variant of [`Node`] in which all the arguments and source are type-guaranteed to be evaluated.
/// This is achieved by using [`Item`] instead of [`Expr`], avoiding the possibility of [`Expr::Eval`].
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ENode {
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
            source: enode.source.map(|item| Box::new(item.into())),
            args: enode.args.into_iter().map(Expr::from).collect()
        }
    }
}

impl From<&ENode> for Node {
    fn from(enode: &ENode) -> Node {
        Node::from(enode.clone())
    }
}

impl Describe for ENode {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, self.source.as_ref(), &self.args)
    }
}


/// A precursor of [`Node`] which type-guarantees that the source is left empty.
#[derive(Debug, PartialEq, Clone)]
pub struct PreNode {
    pub head: Head,
    pub args: Vec<Expr>
}

impl PreNode {
    /// Creates a new `PreNode`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, args: Vec<Expr>) -> PreNode {
        PreNode{head: head.into(), args}
    }
}

impl From<PreNode> for Node {
    fn from(prenode: PreNode) -> Node {
        Node{head: prenode.head, source: None, args: prenode.args}
    }
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

// Only for private use in Node::describe_helper.
impl Head {
    fn describe(&self) -> String {
        match self {
            Head::Symbol(s) => s.to_owned(),
            Head::Block(b) => format!("{{{}}}", b.describe()),
            Head::Oper(_) => Default::default(),
            Head::Repl(chr, None) => chr.to_string(),
            Head::Repl(chr, Some(num)) => format!("{chr}{num}"),
            Head::Lang(LangItem::Args(head)) => format!("{}@", head.describe()),
            Head::Lang(_) => Default::default(),
        }
    }

    pub fn args<T>(head: T) -> Head where T: Into<Head> {
        Head::Lang(LangItem::Args(Box::new(head.into())))
    }
}

impl<T> From<T> for Head where T: Into<String> {
    fn from(symbol: T) -> Head {
        Head::Symbol(symbol.into())
    }
}

impl From<LangItem> for Head {
    fn from(lang: LangItem) -> Head {
        Head::Lang(lang)
    }
}

impl From<Expr> for Head {
    fn from(expr: Expr) -> Head {
        Head::Block(Box::new(expr))
    }
}

impl From<Item> for Head {
    fn from(expr: Item) -> Head {
        Head::Block(Box::new(expr.into()))
    }
}

impl From<Node> for Head {
    fn from(expr: Node) -> Head {
        Head::Block(Box::new(expr.into()))
    }
}


/// Special types of [`Head`] for language constructs with special syntax.
#[derive(Debug, PartialEq, Clone)]
pub enum LangItem {
    /// List (`[1, 2, 3]` ~ `$part(1, 2, 3)`)
    List,
    /// Parts (`source[1, 2]` ~ `source.$part(1, 2)`)
    Part,
    /// Colon (`source:func` ~ `source.$map(func)`)
    Map,
    /// At-sign (`source.head@args` ~ `source.$args{head}(args)`)
    Args(Box<Head>)
}

impl LangItem {
    fn keyword(&self) -> &'static str {
        use LangItem::*;
        match self {
            List => "$list",
            Part => "$part",
            Map => "$map",
            Args(_) => "$args"
        }
    }
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

    pub fn new_string(value: impl Into<String>) -> Item {
        use crate::lang::LiteralString;
        Item::Stream(Box::new(LiteralString::from(value.into())))
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

    pub fn format(&self, max_len: usize) -> (String, Option<StreamError>) {
        struct Stateful<'item> {
            item: &'item Item,
            cell: Cell<Option<StreamError>>
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

    pub(crate) fn format_int(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        use Item::*;
        match self {
            Number(n) => write!(f, "{n}"),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "{c}"),
            Stream(s) => s.writeout(f, error)
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

impl Default for Item {
    fn default() -> Item {
        Item::Number(Default::default())
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


/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream: DynClone + Describe {
    /// Create an [`SIterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    #[must_use]
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node>;

    /// An indication whether this stream should be treated as a string. The implementation should
    /// only return `true` if it can be sure that the iterator will produce a stream of [`Char`]s.
    /// If so, this affects the behaviour of `Stream::writeout()`.
    ///
    /// The default implementation returns `false`.
    // TODO link do <dyn Stream>::writeout unsupported?
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
    /// looking at [`length()`](Stream::length). If the information is insufficient, constructs the
    /// iterator and tries answering using `Iterator::size_hint()`. As a last resort, the iterator
    /// is consumed.
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
                    _ => iter.next().is_none()
                }
            }
        }
    }
}

impl dyn Stream {
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
    pub fn writeout(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        if self.is_string() {
            self.writeout_string(f, error)
        } else {
            self.writeout_stream(f, error)
        }
    }

    fn writeout_stream(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
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

    fn writeout_string(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
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
                                error.set(Some(StreamError::new(err, self.to_expr())));
                                break 'a;
                            }
                        },
                        Err(err) => {
                            s += "<!>";
                            error.set(Some(err));
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

    pub(crate) fn to_expr(&self) -> Expr {
        Expr::Imm(Item::Stream(dyn_clone::clone_box(self)))
    }
}

impl Display for dyn Stream {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.writeout(f, &Default::default())
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

    pub fn intersection(l1: &Length, l2: &Length) -> Length {
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
    /// It is possible to return `Ok(Some(0))` to indicate that all elements have been skipped
    /// without remainder, but that `next()` should not be called again. If `None` is returned,
    /// `next()` may be called and must return `None` for consistency.
    ///
    /// The default implementation calls `next()` an appropriate number of times, and thus is
    /// reasonably usable only for small values of `n`, except when `n` is found to exceed the
    /// value given by [`SIterator::len_remain()`].
    ///
    /// # Panics
    /// This function may panic if a negative value is passed in `n`.
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        assert!(!n.is_negative());
        if let Some(len) = self.len_remain() {
            if n > &len {
                return Ok(Some(n - &len));
            }
        }
        let mut n = n.to_owned();
        while !n.is_zero() {
            match self.next() {
                Some(Ok(_)) => (),
                Some(Err(err)) => return Err(err),
                None => return Ok(Some(n))
            }
            n.dec();
        }
        Ok(None)
    }
}

impl<T, U, V> SIterator for std::iter::Map<T, U>
where T: Iterator<Item = V>,
      U: FnMut(V) -> Result<Item, StreamError>
{ }

impl SIterator for std::iter::Once<Result<Item, StreamError>> { }

impl SIterator for std::iter::Empty<Result<Item, StreamError>> { }

pub(crate) struct Forever<'node> {
    pub item: &'node Item
}

impl Iterator for Forever<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok(self.item.clone()))
    }
}

impl SIterator for Forever<'_> {
    fn skip_n(&mut self, _n: &Number) -> Result<Option<Number>, StreamError> {
        Ok(None)
    }
}


/// The environment in which expressions are evaluated. This is passed as an argument to
/// [`Expr::eval()`]. Currently a placeholder, but in the future to be defined through `env`.
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

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Alphabet { &Alphabet::Std26 }
}

impl Describe for Env {
    fn describe(&self) -> String { todo!() }
}

#[test]
fn test_block() {
    use crate::parser::parse;
    assert_eq!(parse("{#1}(3,4)").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("{#2}(3,4)").unwrap().eval().unwrap().to_string(), "4");
    assert!(parse("{#3}(3,4)").unwrap().eval().is_err());
    assert!(parse("#1").unwrap().eval().is_err());
    assert_eq!(parse("1.{2}(3)").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("{#1+{#1}(2,3)}(4,5)").unwrap().eval().unwrap().to_string(), "6");
    assert_eq!(parse("{#1}({#2}(3,4),5)").unwrap().eval().unwrap().to_string(), "4");
}

#[test]
fn test_describe() {
    use crate::parser::parse;

    // chaining, args, block
    let orig = parse("a.b(c,d).{e}(f,g)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // value types
    let orig = parse(r#"[1,true,'a"\'b\nc',"a'b\"c\n",[],""]"#).unwrap();
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

#[cfg(test)]
#[track_caller]
pub(crate) fn test_len_exact(item: &Item, len: usize) {
    let stm = item.as_stream().unwrap();
    assert_eq!(stm.iter().map(Result::unwrap).count(), len);
    assert!(Length::possibly_eq(&stm.length(), &Length::Exact(len.into())));
    assert_eq!(len == 0, stm.is_empty());
}

#[cfg(test)]
#[track_caller]
pub(crate) fn test_skip_n(item: &Item) {
    let stm = item.as_stream().unwrap();
    const TEST: u32 = 5;

    // skip(0) = no-op on fresh iterator
    let (mut i1, mut i2) = (stm.iter(), stm.iter());
    assert_eq!(i1.next(), match i2.skip_n(&Number::zero()).unwrap() {
        Some(_) => None,
        None => i2.next() // same None, same element or same error
    });

    if !stm.is_empty() {
        assert_ne!(stm.iter().next().transpose().unwrap(), None); // fails if the first item fails

        // skip(1) = next() on fresh iterator
        let (mut i1, mut i2) = (stm.iter(), stm.iter());
        i1.next();
        assert_eq!(i1.next(), match i2.skip_n(&Number::one()).unwrap() {
            Some(_) => None,
            None => i2.next()
        });

        match stm.length() {
            Length::Exact(len) => {
                assert!(len.is_positive());

                // skip(len - 1) leaves exactly one element
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&(&len - 1)).unwrap(), None);
                assert_ne!(it.next().transpose().unwrap(), None);
                assert_eq!(it.next(), None);

                // skip(len) leaves nothing
                let mut it = stm.iter();
                match it.skip_n(&len).unwrap() {
                    None => assert_eq!(it.next(), None),
                    Some(rem) => assert_eq!(rem, Number::zero())
                }

                // skip(len + N) reports N remaining elements
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&(&len + 1)).unwrap(), Some(Number::one()));
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&(&len + 100)).unwrap(), Some(100.into()));

                // skip() following skip()
                let mut half = (&len - 1) / 2;
                let mut rest = &len - 1 - &half;
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&half).unwrap(), None);
                assert_eq!(i1.skip_n(&rest).unwrap(), None);
                assert_eq!(i2.skip_n(&(&len - 1)).unwrap(), None);
                assert_eq!(i1.next(), i2.next());

                // skip(0) = no-op later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&half).unwrap(), None);
                assert_eq!(i1.skip_n(&Number::zero()).unwrap(), None);
                assert_eq!(i2.skip_n(&half).unwrap(), None);
                assert_eq!(i1.next(), i2.next());

                // skip(1) = next() later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&half).unwrap(), None);
                assert_eq!(i1.skip_n(&Number::one()).unwrap(), None);
                assert_eq!(i2.skip_n(&half).unwrap(), None);
                assert_ne!(i2.next().transpose().unwrap(), None);
                assert_eq!(i1.next(), i2.next());

                // skip() from within past end
                half.inc();
                rest.inc(); // now half + rest = len + 1
                let mut it = stm.iter();
                assert_eq!(it.skip_n(&half).unwrap(), None);
                assert_eq!(it.skip_n(&rest).unwrap(), Some(Number::one()));

                // test actually comparing a few elements after a small skip
                if len > (2 * TEST).into() {
                    let (mut i1, mut i2) = (stm.iter(), stm.iter());
                    assert_eq!(i1.skip_n(&TEST.into()).unwrap(), None);
                    for _ in 0..TEST {
                        i2.next().unwrap().unwrap();
                    }
                    for _ in 1..TEST {
                        assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
                    }
                }
            },
            Length::Infinite => {
                let many = 10000000000_i64.into();

                // skip() following skip()
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&many).unwrap(), None);
                assert_eq!(i1.skip_n(&many).unwrap(), None);
                assert_eq!(i2.skip_n(&(&many * 2)).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // skip(0) = no-op later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&many).unwrap(), None);
                assert_eq!(i1.skip_n(&Number::zero()).unwrap(), None);
                assert_eq!(i2.skip_n(&many).unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // skip(1) = next() later in stream
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&many).unwrap(), None);
                assert_eq!(i1.skip_n(&Number::one()).unwrap(), None);
                assert_eq!(i2.skip_n(&many).unwrap(), None);
                assert_ne!(i2.next().transpose().unwrap(), None);
                assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());

                // test actually comparing a few elements after a small skip
                let (mut i1, mut i2) = (stm.iter(), stm.iter());
                assert_eq!(i1.skip_n(&TEST.into()).unwrap(), None);
                for _ in 0..TEST {
                    i2.next().unwrap().unwrap();
                }
                for _ in 0..TEST {
                    assert_eq!(i1.next().unwrap().unwrap(), i2.next().unwrap().unwrap());
                }
            },
            _ => ()
        }
    } else {
        assert_eq!(stm.iter().next(), None);
        assert_eq!(stm.iter().skip_n(&Number::one()).unwrap(), Some(Number::one()));
    }
}
