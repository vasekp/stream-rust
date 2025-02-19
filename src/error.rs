use crate::base::*;
use std::fmt::{Display, Formatter, Debug};

/// The base error returned by helper functions. In most situations this is intended to be
/// turned into [`StreamError`] by supplementing a [`Expr`].
#[derive(Debug, PartialEq, Clone)]
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


/// The runtime error type with an indication of the [`Expr`] whose evaluation caused it.
#[derive(PartialEq, Debug)]
pub struct StreamError {
    reason: BaseError,
    expr: Expr
}

impl StreamError {
    pub fn new(reason: impl Into<BaseError>, expr: impl Into<Expr>) -> StreamError {
        StreamError{reason: reason.into(), expr: expr.into()}
    }
}

impl std::error::Error for StreamError { }

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.expr.describe(), self.reason)
    }
}


macro_rules! try_with {
    ($blame:expr, $expr:expr) => {
        match (|| -> Result<_, BaseError> { Ok($expr) })() {
            Ok(result) => result,
            Err(err) => return Err(StreamError::new(err, $blame))
        }
    }
}

pub(crate) use try_with;


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
