use crate::base::*;
use std::fmt::{Display, Formatter, Debug};

pub type SResult<T> = std::result::Result<T, StreamError>;

/// The runtime error type.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct StreamError {
    reason: Reason,
    trace: Vec<Expr>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Reason {
    Generic(String),
    Usage(&'static str),
    Interrupt,
}

impl StreamError {
    pub fn with_expr(reason: impl Into<Reason>, expr: &impl ToExpr) -> Self {
        Self{reason: reason.into(), trace: vec![expr.to_expr()]}
    }

    pub fn usage(head: &Head) -> Self {
        let head_str = head.as_str().expect("StreamError::usage should be called with Head::Symbol or Head::Oper");
        Self{reason: Reason::Usage(head_str), trace: vec![]}
    }

    pub fn interrupt() -> Self {
        Self{reason: Reason::Interrupt, trace: vec![]}
    }

    pub(crate) fn wrap(mut self, expr: &impl ToExpr) -> Self {
        self.trace.push(expr.to_expr());
        self
    }
}

impl std::error::Error for StreamError { }

impl<T: Into<Reason>> From<T> for StreamError {
    fn from(reason: T) -> Self {
        Self{reason: reason.into(), trace: vec![]}
    }
}

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        //if let Some(expr) = &self.trace.first() {
        //    write!(f, "{}: ", expr.describe())?;
        //}
        // TODO
        for expr in self.trace.iter().rev() {
            writeln!(f, "{}:", expr.describe())?;
        }
        write!(f, "{}", self.reason)
    }
}

impl<T: Into<String>> From<T> for Reason {
    fn from(s: T) -> Self {
        Reason::Generic(s.into())
    }
}

impl Display for Reason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(s) => write!(f, "{s}"),
            Self::Usage(sym) => write!(f, "invalid call pattern: see ?{sym}"),
            Self::Interrupt => write!(f, "interrupted")
        }
    }
}

pub trait ToExpr {
    fn to_expr(&self) -> Expr;
}

impl ToExpr for Expr {
    fn to_expr(&self) -> Expr {
        self.clone()
    }
}

impl ToExpr for Node {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::new(self.clone()))
    }
}

impl ToExpr for Node<Item> {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::new(self.clone().into()))
    }
}

impl ToExpr for Rc<Node> {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::clone(self))
    }
}

impl ToExpr for Item {
    fn to_expr(&self) -> Expr {
        Expr::Imm(self.clone())
    }
}

impl<I: ItemType> ToExpr for Rc<dyn Stream<I>> {
    fn to_expr(&self) -> Expr {
        Expr::Imm(Item::from(self))
    }
}

impl ToExpr for Char {
    fn to_expr(&self) -> Expr {
        Expr::Imm(Item::from(*self))
    }
}


/// The error type returned by [`parser::parse`](crate::parser::parse). Contains the description of
/// the error and its location within the input string. The lifetime is bound to the lifetime of
/// the input string.
#[derive(Debug, PartialEq)]
pub struct ParseError<'str> {
    reason: String,
    slice: &'str str
}

impl<'str> ParseError<'str> {
    pub fn new(text: impl Into<String>, slice: &'str str) -> ParseError<'str> {
        ParseError{reason: text.into(), slice}
    }

    /// Shows the location of the parse error. For this purpose, the input string is reproduced in
    /// full. The part causing the error is highlighted using ANSI color sequences.
    ///
    /// For the actual description of the error, use the `Display` trait.
    pub fn display(&self, input: &'str str) {
        if self.slice.is_empty() { return; }
        let start = unsafe { self.slice.as_ptr().offset_from(input.as_ptr()) } as usize;
        let length = self.slice.len();
        //println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
        println!("{}\x1b[1;31m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)], &input[(start+length)..]);
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.reason, f)
    }
}

macro_rules! check_stop {
    () => {
        if stop::should_stop() {
            Err(StreamError::interrupt())?;
        }
    };
}

pub(crate) use check_stop;
