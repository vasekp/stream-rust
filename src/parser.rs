use std::str::CharIndices;
use std::iter::Peekable;
use crate::base::{StreamError, StreamResult};

pub struct Tokenizer<'a> {
    input: &'a str,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer{input, iter: input.char_indices().peekable()}
    }

    fn read_string(&mut self) -> Option<StreamResult<&'a str>> {
        let start = self.byte_pos();
        self.iter.next();
        'b: {
            while let Some((_, ch)) = self.iter.next() {
                if ch == '\\' {
                    if self.iter.next().is_none() {
                        return Some(Err(StreamError("unterminated string".to_string())));
                    }
                } else if ch == '"' {
                    break 'b;
                }
            }
            return Some(Err(StreamError("unterminated string".to_string())));
        }
        let end = self.byte_pos();
        Some(Ok(&self.input[start..end]))
    }

    fn byte_pos(&mut self) -> usize {
        match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = StreamResult<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(_, ch)) = self.iter.peek() {
            if ch == '"' {
                return self.read_string();
            }
            let start = self.byte_pos();
            while let Some(&(_, ch2)) = self.iter.peek() {
                if ch2 != ch {
                    break;
                }
                self.iter.next();
            }
            let end = self.byte_pos();
            Some(Ok(&self.input[start..end]))
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

    let mut tk = Tokenizer::new(r#"a💖b"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("💖")));
    assert_eq!(tk.next(), Some(Ok("b")));
    assert_eq!(tk.next(), None);
}
