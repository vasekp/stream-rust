use std::fmt::{Display, Formatter, Debug};

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

    pub fn range_within(&self, input: &'str str) -> Option<std::ops::Range<usize>> {
        let start = input.as_bytes()
            .element_offset(self.slice.as_bytes().get(0)?)?;
        let length = self.slice.len();
        Some(start..(start+length))
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.reason, f)
    }
}
