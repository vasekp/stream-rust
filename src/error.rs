use crate::base::*;
use std::fmt::{Display, Formatter, Debug};

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


/// The runtime error type with an indication of the [`Node`] whose evaluation caused it.
#[derive(PartialEq, Debug)]
pub struct StreamError {
    reason: BaseError,
    node: Node
}

impl StreamError {
    pub fn new(reason: impl Into<BaseError>, node: impl Into<Node>) -> StreamError {
        StreamError{reason: reason.into(), node: node.into()}
    }
}

impl std::error::Error for StreamError { }

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.node.describe(), self.reason)
    }
}


macro_rules! try_with {
    ($node:ident, $expr:expr) => {
        match (|| -> Result<_, BaseError> { $expr })() {
            Ok(result) => result,
            Err(err) => return Err(StreamError::new(err, $node))
        }
    }
}

pub(crate) use try_with;


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

impl<'str> Display for ParseError<'str> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.reason, f)
    }
}
