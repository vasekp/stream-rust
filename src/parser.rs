use std::str::Chars;
use std::iter::Peekable;
use crate::base::{StreamError, StreamResult};

pub struct Tokenizer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    index: usize
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    slice: &'a str,
    start: usize,
    length: usize
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer{input, chars: input.chars().peekable(), index: 0}
    }

    fn read_string(&mut self) -> Option<StreamResult<&'a str>> {
        let start = self.index;
        self.chars.next();
        let mut length = 1;
        'b: {
            while let Some(ch) = self.chars.next() {
                length = length + 1;
                if ch == '\\' {
                    if let None = self.chars.next() {
                        return Some(Err(StreamError("unterminated string".to_string())));
                    }
                    length = length + 1;
                } else if ch == '"' {
                    break 'b;
                }
            }
            return Some(Err(StreamError("unterminated string".to_string())));
        }
        self.index = self.index + length;
        Some(Ok(&self.input[start..self.index]))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = StreamResult<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&ch) = self.chars.peek() {
            if ch == '"' {
                return self.read_string();
            }
            let start = self.index;
            self.chars.next();
            let mut length = 1;
            while let Some(&ch2) = self.chars.peek() {
                if ch2 != ch {
                    break;
                }
                length = length + 1;
                self.chars.next();
            }
            self.index = self.index + length;
            Some(Ok(&self.input[start..self.index]))
        } else {
            None
        }
    }
}

#[test]
fn test_parser() {
    let mut tk = Tokenizer::new(r#"a""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\\"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\\\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a\"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\\")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);
}
