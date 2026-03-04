use crate::base::*;
use std::fmt::{Display, Formatter, Debug};


/// The runtime error type.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum StreamError {
    ExprError { reason: String, expr: Option<Expr> },
    Interrupt
}

impl StreamError {
    pub fn new(reason: impl Into<String>, expr: impl Into<Expr>) -> StreamError {
        StreamError::ExprError{reason: reason.into(), expr: Some(expr.into())}
    }

    pub fn new0(reason: impl Into<String>) -> StreamError {
        StreamError::ExprError{reason: reason.into(), expr: None}
    }
}

impl std::error::Error for StreamError { }

/*impl From<T: Into<String>> for StreamError {
    fn from(s: T) -> StreamError {
        StreamError::ExprError{reason: t.into(), expr: None}
    }
}*/

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExprError { reason, expr: Some(expr) } => write!(f, "{}: {}", expr.describe(), reason),
            Self::ExprError { reason, expr: None } => write!(f, "{}", reason),
            Self::Interrupt => write!(f, "interrupted")
        }
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

macro_rules! iter_try_call {
    ($expr:expr) => {
        match (|| -> Result<_, StreamError> { Ok($expr) })() {
            Ok(value) => value,
            Err(err) => return Some(Err(err))
        }
    }
}

macro_rules! iter_try_expr {
    ($expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(err) => return Some(Err(err))
        }
    }
}

macro_rules! check_stop {
    () => {
        if stop::should_stop() {
            Err(StreamError::Interrupt)?;
        }
    };
    (iter) => {
        if stop::should_stop() {
            return Some(Err(StreamError::Interrupt));
        }
    }
}

pub(crate) use iter_try_call;
pub(crate) use iter_try_expr;
pub(crate) use check_stop;
