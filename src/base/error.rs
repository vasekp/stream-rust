use crate::base::*;
use std::fmt::{Display, Formatter, Debug};

/// The base error returned by helper functions. In most situations this is intended to be
/// turned into [`StreamError`] by supplementing a [`Expr`].
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum BaseError {
    String(String),
    StreamError(Box<StreamError>)
}

impl From<String> for BaseError {
    fn from(string: String) -> BaseError {
        BaseError::String(string)
    }
}

impl From<&str> for BaseError {
    fn from(string: &str) -> BaseError {
        BaseError::String(string.to_string())
    }
}

impl From<StreamError> for BaseError {
    fn from(err: StreamError) -> BaseError {
        BaseError::StreamError(Box::new(err))
    }
}

impl Display for BaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::StreamError(s) => write!(f, "{s}")
        }
    }
}


/// The runtime error type with an indication of the [`Expr`] whose evaluation caused it.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum StreamError {
    ExprError { reason: String, expr: Expr },
    Interrupt
}

impl StreamError {
    pub fn new(base: impl Into<BaseError>, expr: impl Into<Expr>) -> StreamError {
        match base.into() {
            BaseError::String(reason) => StreamError::ExprError{reason, expr: expr.into()},
            BaseError::StreamError(err) => *err
        }
    }
}

impl std::error::Error for StreamError { }

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExprError { reason, expr } => write!(f, "{}: {}", expr.describe(), reason),
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

macro_rules! try_with {
    ($blame:expr, $expr:expr) => {
        match (|| -> Result<_, BaseError> { Ok($expr) })() {
            Ok(result) => result,
            Err(err) => return Err(StreamError::new(err, $blame))
        }
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

pub(crate) use try_with;
pub(crate) use iter_try_call;
pub(crate) use iter_try_expr;
pub(crate) use check_stop;
