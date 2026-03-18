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

    /// Shows the location of the parse error. For this purpose, the input string is reproduced in
    /// full. The part causing the error is highlighted using ANSI color sequences.
    ///
    /// For the actual description of the error, use the `Display` trait.
    pub fn display(&self, input: &'str str) {
        if self.slice.is_empty() { return; }
        let start = input.as_bytes()
            .element_offset(&self.slice.as_bytes()[0]);
        let length = self.slice.len();
        if let Some(start) = start {
            println!("{}\x1b[1;31m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)], &input[(start+length)..]);
        } else {
            println!("{input}");
        }
    }
}

impl Display for ParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.reason, f)
    }
}
